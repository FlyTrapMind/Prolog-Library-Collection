:- module(
  open_any,
  [
    close_any/2, % +Outstream
                 % -Metadata:dict
    open_any/3, % +Input
                % -Substream:stream
                % +Options:list(nvpair)
    open_any/4 % +Input
               % -Substream:stream
               % -Metadata:dict
               % +Options:list(nvpair)
  ]
).

/** <module> RDF: load any

Open a recursive data stream from files/URIs.

@author Wouter Beek
@author Jan Wielemaker
@tbd Only supports URI schemes `http` and `https`.
@version 2014/03-2014/07, 2014/10-2014/12, 2015/02
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(http/http_cookie)). % Redirection may require cookies.
:- use_module(library(http/http_header)). % Private predicates.
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).
:- use_module(library(uri)).

:- use_module(generics(atom_ext)).
:- use_module(generics(pair_ext)).
:- use_module(os(datetime_ext)).

:- predicate_options(open_any/3, 3, [
     pass_to(open_any/4, 4)
   ]).
:- predicate_options(open_any/4, 4, [
     pass_to(open_input/5, 5)
   ]).
:- predicate_options(open_input/5, 5, [
     pass_to(http_open/3, 3),
     pass_to(open/4, 4)
   ]).

:- public(ssl_verify/5).

%! ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
% Currently we accept all certificates.

ssl_verify(
  _SSL,
  _ProblemCertificate,
  _AllCertificates,
  _FirstCertificate,
  _Error
).





%! close_any(+Outstream, -Metadata:dict) is det.

close_any(Out, Metadata):-
  % A BOM (Byte Order Mark) was detected while opening the file for reading
  (   stream_property(Out, bom(Bom))
  ->  true
  ;   Bom = false
  ),
  stream_property(Out, encoding(Encoding)),
  stream_property(Out, locale(CurrentLocale)),
  stream_property(Out, newline(NewlineMode)),
  stream_property(Out, type(Type)),
  byte_count(Out, Bytes),
  character_count(Out, Characters),
  line_count(Out, Lines),
  Metadata = stream{
      'bom-detected':Bom,
      'byte-count':Bytes,
      'character-count':Characters,
      encoding:Encoding,
      'current-locale':CurrentLocale,
      'line-count':Lines,
      'newline-mode':NewlineMode,
      'stream-type':Type
  },
  close(Out).



%! open_any(+Input, -Substream:stream, +Options:list(nvpair)) is nondet.

open_any(Input, Substream, Options):-
  open_any(Input, Substream, _, Options).


%! open_any(
%!   +Input,
%!   -Substream:stream,
%!   -Metadata:dict,
%!   +Options:list(nvpair)
%! ) is nondet.
% Provide access to plain data elements in   a  stream that may be
% compressed and/or contain (nested) archives.   For  each element
% found, it succeeds with  Stream  unified   to  a  binary  stream
% associated with the data  and  Location   is  a  dict containing
% information about the  applied  filtering   and  possibly  other
% meta-data available about the element.
%
% Stream must be closed, but  it  is   not  required  to  read all
% content from the stream.  Committing   before  all  elements are
% exhausted is allowed and will cause   all allocated resources to
% be reclaimed.
%
% @arg  Input has one of the following forms:
%         - `file(+atom)`
%         - `file_pattern(+atom)`
%           File pattern, handled by expand_file_name/2.
%         - `file_spec(+atom)`
%           File specification, handled by absolute_file_name/3.
%         - `stream(+stream)`
%            A stream which is _not_ automatically closed after processing.
%         - `uri(+atom)` / `url(+atom)`
%         - `uri_components(+atom,+atom,?atom,?atom,?atom)`
%
%       In addition, Input can be given directly as a stream handle.
%
%       In addition, Input can be given directly,
%       i.e., as a plain atom, stream handle, file pattern, etc.
%       In these cases the interpretation is ambiguous
%       and the following heuristics are used (in that order):
%
%         1. A file if exists_file/1 succeeds.
%         2. A URI if uri_components/2 succeeds.
%         3. A file if absolute_file_name/3 succeeds.
%         4. A file if expand_file_name/2 succeeds with at least one file.
%
% @arg  In An input stream.
%
% @arg  Metadata is a dict.
%       The tag indicates the type of Input and
%       is currently one of =stream=, =file= or =url=.
%       For URLs, the keys =content_type=, =content_length= and
%       =last_modified= may be available.
%
%       The dict contains a key =data=, holding a list that
%       describes the decoding pipeline used for the data
%       element. Elements of this list are:
%
%         - filter(Filter)
%           The indicated content filter was applied.
%         - archive_entry{}, where the keys provide all
%           solutions of archive_header_property/2 and the
%           key =name= provides the name of the archive entry.
%
% @arg  Options

open_any(Input, Substream, Metadata, Options):-
  open_input(Input, Stream, Metadata0, Close, Options),
  Metadata = Metadata0.put(archive, ArchiveMetadata),
  setup_call_cleanup(
    archive_open(
      stream(Stream),
      Archive,
      [close_parent(Close),format(all),format(raw)]
    ),
    % True when Substream is a raw content stream for data in Substream
    % and ArchiveMetadata describes the location of Substream
    % in the substream tree.
    archive_data_stream(Archive, Substream, [meta_data(ArchiveMetadata)]),
    archive_close(Archive)
  ).


%! open_input(
%!   +Input,
%!   -Out:stream,
%!   -Metadata:dict,
%!   -Close:boolean,
%!   +Options:list(nvpair)
%! ) is det.

% A1. File
open_input(file(File), Out, file{path:File}, true, Options1):-
  exists_file(File), !,
  merge_options([type(binary)], Options1, Options2),
  open(File, read, Out, Options2).

% A2. File pattern
%     @see expand_file_name/2
open_input(file_pattern(Wildcard), Out, Metadata, Close, Options):-
  atom(Wildcard),
  expand_file_name(Wildcard, Files),
  Files \== [], !,
  % NONDET: Backtracking over files.
  member(File, Files),
  open_input(File, Out, Metadata, Close, Options).

% A3. File specification.
open_input(file_spec(Spec), Out, Metadata, Close, Options):-
  compound(Spec), !,
  absolute_file_name(Spec, File, [access(read)]),
  open_input(file(File), Out, Metadata, Close, Options).

% A4. Stream: already opened.
% @tbd Can we check for read access?
open_input(stream(Out), Out, stream{stream:Out}, false, _):-
  is_stream(Out),
  stream_property(Out, input), !.

% A5. URI Components: opens files and URLs.
% @compat Only supports URLs with schemes `http` or `https`.
open_input(uri_components(UriComponents), Out, Metadata, Close, Options1):-
  UriComponents = uri_components(Scheme,Authority,_,_,_),
  % Make sure the URL may be syntactically correct,
  % having at least the required `Scheme` and `Authority` components.
  maplist(atom, [Scheme,Authority]), !,

  % If the URI scheme is `file` we must open a file.
  % Otherwise, a proper URL has to be opened.
  uri_components(Uri, UriComponents),
  (   Scheme == file
  ->  uri_file_name(Uri, File),
      open_input(file(File), Out, Metadata, Close, Options1)
  ;   http_scheme(Scheme),
      Headers1 = [
          header('Access-Control-Allow-Origin', _),
          header('Accept-Ranges', _),
          header('Cache-Control', _),
          header('Connection', _),
          header('Content-Type', _),
          header('Content-Length', _),
          header('Date', _),
          header('ETag', _),
          header('Expires', _),
          header('Last-Modified', _),
          header('Server', _),
          header('Transfer-Encoding', _)
      ],
      merge_options(
        [method(get),status_code(StatusCode)|Headers1],
        Options1,
        Options2
      ),
      http_open(Uri, Out, Options2),

      % Exclude headers that did not occur in the HTTP response.
      exclude(empty_http_header, Headers1, Headers2),

      maplist(http_header_dict, Headers2, Headers3),
      dict_pairs(HttpHeaders, 'http-headers', Headers3),
      http_header:status_number_fact(ReasonKey, StatusCode),
      phrase(http_header:status_comment(ReasonKey), ReasonPhrase0),
      string_codes(ReasonPhrase, ReasonPhrase0),
      Metadata = meta_data{
          'HTTP':'http-metadata'{
              headers:HttpHeaders,
              status:'http-status'{
                  code:StatusCode,
                  'reason-phrase':ReasonPhrase
              }
          },
          'URI':Uri
      },
      Close = true
  ).

% A6. URI: convert to URI components term.
open_input(uri(Url), Out, Metadata, Close, Options):- !,
  uri_components(Url, UriComponents),
  open_input(UriComponents, Out, Metadata, Close, Options).

% A6'. URL: same as URI.
open_input(url(Url), Out, Metadata, Close, Options):- !,
  open_input(uri(Url), Out, Metadata, Close, Options).


% B. Stream
open_input(Stream, Out, Metadata, Close, Options):-
  is_stream(Stream), !,
  open_input(stream(Stream), Out, Metadata, Close, Options).


% C1. File
open_input(File, Out, Metadata, Close, Options):-
  exists_file(File), !,
  open_input(file(File), Out, Metadata, Close, Options).

% C2. URI
open_input(Uri, Out, Metadata, Close, Options):-
  uri_components(Uri, UriComponents), !,
  open_input(uri_components(UriComponents), Out, Metadata, Close, Options).

% C3. File specification
open_input(Spec, Out, Metadata, Close, Options):-
  compound(Spec),
  absolute_file_name(Spec, _, [access(read),file_errors(fail)]), !,
  open_input(file_spec(Spec), Out, Metadata, Close, Options).

% C4. File pattern
open_input(Wildcard, Out, Metadata, Close, Options):-
  open_input(file_pattern(Wildcard), Out, Metadata, Close, Options).


% D. Out of options...
open_input(Input, _, _, _, _):-
  print_message(warning, cannot_open(Input)),
  fail.



% HELPERS

/* Body length (RFC 7230)
      % Determine message body length.
      (   option(body_length(BodyLength), Options1)
      ->  body_length(
            Method,
            StatusCode,
            TransferEncoding,
            ContentLength,
            BodyLength
          )
      ;   true
      ),

%! body_length(
%!   +Method:oneof([connect,get,head,options,post,put]),
%!   +StatusCode:between(100,599),
%!   ?TransferEncoding:atom,
%!   ?ContentLength:atom,
%!   -BodyLength:nonneg
%! ) is det.
% @tbd If a Transfer-Encoding header field is present and the chunked
%      transfer coding (Section 4.1) is the final encoding, the message
%      body length is determined by reading and decoding the chunked
%      data until the transfer coding indicates the data is complete.
% @tbd If a Transfer-Encoding header field is present in a response and
%      the chunked transfer coding is not the final encoding, the
%      message body length is determined by reading the connection until
%      it is closed by the server.  If a Transfer-Encoding header field
%      is present in a request and the chunked transfer coding is not
%      the final encoding, the message body length cannot be determined
%      reliably; the server MUST respond with the 400 (Bad Request)
%      status code and then close the connection.
% @tbd If a message is received with both a Transfer-Encoding and a
%      Content-Length header field, the Transfer-Encoding overrides the
%      Content-Length.  Such a message might indicate an attempt to
%      perform request smuggling (Section 9.5) or response splitting
%      (Section 9.4) and ought to be handled as an error.  A sender MUST
%      remove the received Content-Length field prior to forwarding such
%      a message downstream.
% @tbd If a message is received without Transfer-Encoding and with
%      either multiple Content-Length header fields having differing
%      field-values or a single Content-Length header field having an
%      invalid value, then the message framing is invalid and the
%      recipient MUST treat it as an unrecoverable error.
%      If this is a response message received by a user agent,
%      the user agent MUST close the connection to the server
%      and discard the received response.

% Any response to a HEAD request and any response with a 1xx
% (Informational), 204 (No Content), or 304 (Not Modified) status
% code is always terminated by the first empty line after the
% header fields, regardless of the header fields present in the
% message, and thus cannot contain a message body.
body_length(head, _, _, _, 0):- !.
body_length(_, StatusCode, _, _, 0):-
  between(100, 199, StatusCode), !.
body_length(_, 204, _, _, 0):- !.
body_length(_, 304, _, _, 0):- !.
% Any 2xx (Successful) response to a CONNECT request implies that
% the connection will become a tunnel immediately after the empty
% line that concludes the header fields.  A client MUST ignore any
% Content-Length or Transfer-Encoding header fields received in
% such a message.
body_length(connect, StatusCode, _, _, 0):-
  between(200, 299, StatusCode), !.
% If a valid Content-Length header field is present without
% Transfer-Encoding, its decimal value defines the expected message
% body length in octets.  If the sender closes the connection or
% the recipient times out before the indicated number of octets are
% received, the recipient MUST consider the message to be
% incomplete and close the connection.
body_length(_, _, TransferEncoding, ContentLength, Length):-
  var(TranferEncoding),
  integer(ContentLength), !,
  Length = ContentLength.
% Otherwise, this is a response message without a declared message
% body length, so the message body length is determined by the
% number of octets received prior to the server closing the
% connection.
body_length(_, _, _, _, _).
*/



%! empty_http_header(+Header:compound) is semidet.
% Succeeds for HTTP headers with empty value.

empty_http_header(header(_,'')).



%! http_header_dict(+Header:compound, -Header:pair) is det.
% Converts a given HTTP header compound term to its JSON equivalent.

http_header_dict(header(Key,Value0), Key-Value):-
  http_header_value_dict(Key, Value0, Value).



%! http_header_value_dict(+Key:atom, +Value:atom, -Value:dict) is det.
% Converts a given HTTP header value to its JSON equivalent.

http_header_value_dict('Content-Length', Atom, Number):- !,
  atom_number(Atom, Number).
http_header_value_dict(
  'Content-Type',
  Atom,
  'media-type'{type:Type, subtype:Subtype, parameters:ParameterDicts}
):- !,
  atomic_list_concat([Type0|Parameters0], ';', Atom),
  maplist(media_type_parameter, Parameters0, Parameters),
  atomic_list_concat([Type,Subtype], '/', Type0),
  maplist(json_pair, Parameters, ParameterDicts).
http_header_value_dict(Key, Atom, Dict):-
  memberchk(Key, ['Date','Expires','Last-Modified']), !,
  parse_time(Atom, rfc_1123, Stamp),
  stamp_date_time(Stamp, DateTime, 'UTC'),
  date_time_dict(DateTime, Dict).
http_header_value_dict(_, Atom, Atom).



%! media_type_parameter(+Atom:atom, -Parameter:pair) is det.

media_type_parameter(Parameter0, Name-Value):-
  strip_atom([' '], Parameter0, Parameter),
  atomic_list_concat([Name,Value], '=', Parameter).



%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).





% MESSAGES

:- multifile(prolog:message//1).

prolog:message(cannot_open(Input)) -->
  ['Cannot open input ',Input,nl].
