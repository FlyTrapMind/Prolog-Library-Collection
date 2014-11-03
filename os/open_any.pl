:- module(
  open_any,
  [
    close_any/2, % +Outstream
                 % -Metadata:dict
    open_any/4 % +Input
               % -Substream:stream
               % -Metadata:dict
               % -Options:list(nvpair)
  ]
).

/** <module> RDF: load any

Load RDF data from various sources.

@author Jan Wielemaker
@author Wouter Beek
@tbd Only supports URI schemes `http` and `https`.
@version 2014/03-2014/07, 2014/10-2014/11
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(http/http_cookie)). % Redirection may require cookies.
:- use_module(library(http/http_header)). % Private predicates.
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(option)).
:- use_module(library(uri)).

:- use_module(generics(atom_ext)).
:- use_module(generics(pair_ext)).
:- use_module(os(datetime_ext)).

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



%! archive_content(+Archive:archive, -Entry:stream, -Pipeline:dict, +PipeTail:list) is nondet.

archive_content(Archive, Entry, Pipeline, PipeTail) :-
  archive_property(Archive, filter(Filters)),
  maplist(wrap_filter, Filters, PipeElements),
  append(PipeElements, RestPipe, Pipeline),
  repeat,
  (   archive_next_header(Archive, Name)
  ->  findall(
        ArchiveProperty,
        archive_header_property(Archive, ArchiveProperty),
        ArchiveProperties
      ),
      dict_create(Pipe, archive_entry, [name(Name)|ArchiveProperties]),
      (   Pipe.filetype == file
      ->  archive_open_entry(Archive, Entry0),
          (   Name == data,
              Pipe.format == raw
          ->  !,
              RestPipe = PipeTail,
              Entry = Entry0
          ;   RestPipe = [Pipe|RestPipe1],
              open_substream(Entry0, Entry, true, RestPipe1, PipeTail)
          )
      ;   fail
      )
  ;   !,
      fail
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
  dict_pairs(
    Metadata,
    json,
    [
      'bom-detected'-Bom,
      'byte-count'-Bytes,
      'character-count'-Characters,
      encoding-Encoding,
      'current-locale'-CurrentLocale,
      'line-count'-Lines,
      'newline-mode'-NewlineMode,
      'stream-type'-Type
    ]
  ),
  close(Out).



%! open_any(
%!   +Input,
%!   -Out:stream,
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
% @arg  Out An output stream possibly containing RDF data.
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

open_any(Input, Out, Metadata, Options):-
  open_input(Input, FileOut, Metadata0, Close, Options),
  Metadata = Metadata0.put(data, Data),
  open_substream(FileOut, Out, Data, Close).


%! open_substream(+Stream:stream, -Substream:stream, -Pipeline:dict) is nondet.
% True when SubStream is a raw content stream for data in Stream
% and Pipeline describes the location of SubStream in the substream tree.
%
% @arg  Pipeline is a list of applied filters and archive selection
%       operations.
%       List elements take the form:
%
%         - `archive(Member, Format)`
%         - `filter(Name)`

open_substream(In, Entry, Pipeline, Close) :-
  open_substream(In, Entry, Close, Pipeline, []).

open_substream(In, Entry, Close, Pipeline, PipeTail) :-
  setup_call_cleanup(
    archive_open(
      stream(In),
      Archive,
      [close_parent(Close),format(all),format(raw)]
    ),
    archive_content(Archive, Entry, Pipeline, PipeTail),
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

% A2. Stream: already opened.
% @tbd Can we check for read access?
open_input(stream(Out), Out, stream{stream:Out}, false, _):-
  is_stream(Out),
  stream_property(Out, input), !.

% A3. URI Components: opens files and URLs.
% @compat Only supports URLs with schemes `http` or `https`.
open_input(UriComponents, Out, Metadata, Close, Options1):-
  UriComponents = uri_components(Scheme,Authority,_,_,_), !,

  % Make sure the URL may be syntactically correct,
  % haivng at least the requires `Scheme` and `Authority` components.
  maplist(atom, [Scheme,Authority]),

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
          header('Server', _)
      ],
      merge_options([status_code(Code)|Headers1], Options1, Options2),
      http_open(Uri, Out, Options2),
      % Exclude headers that did not occur in the HTTP response.
      exclude(empty_header, Headers1, Headers2),
      % Convert headers to JSON-compatible pairs.
      maplist(header_json, Headers2, Headers3),
      % HTTP header JSON object.
      dict_pairs(HeadersDict, json, Headers3),
      % HTTP status.
      http_header:status_number_fact(ReasonKey, Code),
      phrase(http_header:status_comment(ReasonKey), ReasonPhrase),
      dict_pairs(StatusDict, json, [code-Code,'reason-phrase'-ReasonPhrase]),
      % HTTP response/request dictionary.
      dict_pairs(
        Metadata,
        json,
        [headers-HeadersDict,status-StatusDict,'URI'-Uri]
      ),
      Close = true
  ).

% A4. URI: convert to URI components term.
open_input(uri(Url), Out, Metadata, Close, Options):- !,
  uri_components(Url, UriComponents),
  open_input(UriComponents, Out, Metadata, Close, Options).

% A4'. URL: same as URI.
open_input(url(Url), Out, Metadata, Close, Options):- !,
  open_input(uri(Url), Out, Metadata, Close, Options).

% A5. File pattern
open_input(pattern(Pattern), Out, Metadata, Close, Options):-
  atom(Pattern),
  expand_file_name(Pattern, Files),
  Files \== [],
  Files \== [Pattern], !,
  % Backtrack over files.
  member(File, Files),
  open_input(File, Out, Metadata, Close, Options).

% A6. File specification.
open_input(file_spec(Spec), Out, Metadata, Close, Options):-
  compound(Spec), !,
  absolute_file_name(Spec, File, [access(read)]),
  open_input(file(File), Out, Metadata, Close, Options).


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
  open_input(UriComponents, Out, Metadata, Close, Options).

% C3. File specification
open_input(Spec, Out, Metadata, Close, Options):-
  absolute_file_name(Spec, File, [access(read),file_errors(fail)]), !,
  open_input(file(File), Out, Metadata, Close, Options).

% C4. File pattern
open_input(Pattern, Out, Metadata, Close, Options):-
  expand_file_name(Pattern, Files),
  File \== [], !,
  member(File, Files),
  open_input(file(File), Out, Metadata, Close, Options).


% D. Out of options...
open_input(Input, _, _, _, _):-
  print_message(warning, cannot_open(Input)),
  fail.



% HELPERS

%! empty_header(+Header:compound) is semidet.
% Succeeds for HTTP headers with empty value.

empty_header(header(_,'')).


%! header_json(+Header:compound, -ConvertedHeader:pair) is det.
% Converts a given HTTP header compound term to its JSON equivalent.

header_json(header(Key,Value1), Key-Value2):-
  header_value_json(Key, Value1, Value2).


%! header_value_json(+Key:atom, +Value:atom, -ConvertedValue) is det.
% Converts a given HTTP header value to its JSON equivalent.

header_value_json('Content-Length', Atom, Number):- !,
  atom_number(Atom, Number).
header_value_json('Content-Type', Atom, Dict):- !,
  atomic_list_concat([Type|Parameters0], ';', Atom),
  maplist(media_type_parameter, Parameters0, Parameters),
  atomic_list_concat([Type,Subtype], '/', Type),
  maplist(json_pair, Parameters, ParameterDicts),
  dict_pairs(
    Dict,
    json,
    [parameters-ParameterDicts,subtype-Subtype,type-Type]
  ).
header_value_json(Key, Atom, Dict):-
  memberchk(Key, ['Date','Expires','Last-Modified']), !,
  parse_time(Atom, rfc_1123, Stamp),
  stamp_date_time(Stamp, DateTime, 'UTC'),
  date_time_json(DateTime, Dict).
header_value_json(_, Atom, Atom).


%! media_type_parameter(+Atom:atom, -Parameter:pair) is det.

media_type_parameter(Parameter0, Name-Value):-
  strip_atom([' '], Parameter0, Parameter),
  atomic_list_concat([Name,Value], '=', Parameter).


%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).


%! wrap_filter(+FilterName:atom, -FilterTerm:compound) is det.

wrap_filter(Filter, filter(Filter)).



% MESSAGES

:- multifile(prolog:message//1).

prolog:message(cannot_open(Input)) -->
  ['Cannot open input ',Input,nl].
