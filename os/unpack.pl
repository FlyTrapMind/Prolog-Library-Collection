:- module(
  unpack,
  [
    unpack/3 % +Spec
             % -Read:blob,
             % -Location:dict
  ]
).

/** <module> Unpack encoded and archived material

@author Jan Wielemaker
@author Wouter Beek
@version 2014/03-2014/07
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(http/http_cookie)). % Sometimes redirection requires cookies.
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists)).
:- use_module(library(uri)).

:- use_module(plRdf_ser(rdf_file_db)).



%! unpack(+Spec, -Read:blob, -Location:dict) is nondet.
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
% @arg  Location is a dict.  The tag indicates the type of Spec and
%   is currently one of =stream=, =file= or =url=. For URLs,
%   the keys =content_type=, =content_length= and
%   =last_modified= may be available.
%
%   The dict contains a key =data=, holding a list that
%   describes the decoding pipeline used for the data
%   element. Elements of this list are:
%
%     - filter(Filter)
%     The indicated content filter was applied.
%     - archive_entry{}, where the keys provide all
%     solutions of archive_header_property/2 and the
%     key =name= provides the name of the archive entry.
%
% @arg  Spec is a stream, URL or file name. If Spec is a stream,
%   it is _not_ closed after processing.

unpack(Spec, Stream, Location) :-
  open_input(Spec, In, Meta, CloseStream),
  Location = Meta.put(data, Data),
  content(In, Stream, Data, CloseStream).


%! open_input(
%!   +Spec:compound,
%!   -Read:blob,
%!   -MetaData:dict,
%!   -Close:boolean
%! ) is semidet.
% Spec can be of the following forms:
%   - `uri_components(
%         +Scheme:atom,
%         +Authority:atom,
%         ?Path:atom,
%         ?Search:atom,
%         ?Fragment:atom
%     )`
%   - `url(+Url:atom)`
%   - `stream(+Read:stream)`

% File
open_input(file(File), Read, file{path:File}, true) :-
  exists_file(File), !,
  open(File, read, Read, [type(binary)]).

% Stream: already opened.
% @tbd Can we check for read access?
open_input(stream(Read), Read, stream{stream:Read}, false):-
  is_stream(Read), !.

% URI Components: opens files and URLs.
open_input(UriComponents, Read, Meta, StreamClose):-
  UriComponents = uri_components(Scheme,Authority,_,_,_), !,
  
  % Make sure the URL may be syntactically correct,
  % haivng at least the requires `Scheme` and `Authority` components.
  maplist(atom, [Scheme,Authority]),
  
  % If the URI scheme is `file` we must open a file.
  % Otherwise, a proper URL has to be opened.
  (   Scheme == file
  ->  uri_components(Uri, UriComponents),
      uri_file_name(Uri, File),
      open_input(file(File), Read, Meta, StreamClose)
  ;   open_url(Scheme, UriComponents, Read, Meta),
      StreamClose = true
  ).

% URL: convert to URI components term.
open_input(url(Url), Read, Meta, CloseStream):- !,
  uri_components(Url, UriComponents),
  open_input(UriComponents, Read, Meta, CloseStream).

% File specification.
% @see absolute_file_name/3
open_input(Spec, Read, file{path:Path}, true):-
  compound(Spec), !,
  absolute_file_name(Spec, Path, [access(read)]),
  open(Path, read, Read, [type(binary)]).

% File pattern
% @see expand_file_name/2
open_input(Pattern, Read, Meta, Close) :-
  atom(Pattern),
  expand_file_name(Pattern, Files),
  Files \== [],
  Files \== [Pattern], !,
  % Backtrack over files.
  member(File, Files),
  open_input(File, Read, Meta, Close).

% Out of options...
open_input(Input, _, _, _):-
  print_message(warning, unpack(cannot_open(Input))),
  fail.


%! open_url(+Scheme:atom, +Url:atom, -Read:blob, -Metadata:dict) is semidet.
% Helper for open_input/4 that deals with URLs.
%
% @compat Only supports URLs with schemes `http` or `https`.

open_url(Scheme, Url, Read, Meta) :-
  http_scheme(Scheme), !,
  rdf_extra_headers(Extra),
  http_open(
    Url,
    Read,
    [
      header(content_type, ContentType),
      header(content_length, ContentLength),
      header(last_modified, ModifiedText)
    | Extra]
  ),
  url_meta_pairs(
    [
      content_type=ContentType,
      content_length=ContentLength,
      last_modified=ModifiedText
    ],
    Pairs
  ),
  dict_pairs(Meta, url, [url-Url|Pairs]).

http_scheme(http).
http_scheme(https).

url_meta_pairs([], []).
url_meta_pairs([_=''|T0], T) :- !,
  url_meta_pairs(T0, T).
url_meta_pairs([content_length=Atom|T0], [content_length-Bytes|T]) :-
  atom_number(Atom, Bytes), !,
  url_meta_pairs(T0, T).
url_meta_pairs([Name=Value|T0], [Name-Value|T]) :- !,
  url_meta_pairs(T0, T).


%% content(+Stream, -SubStream, -PipeLine) is nondet.
%
% True when SubStream is a raw content   stream for data in Stream
% and PipeLine describes the location of the data.
%
% @arg  PipeLine is a list of applied filters and archive selection
%   operations.  Elements take the form
%
%     - filter(Name)
%     - archive(Member, Format)

content(In, Entry, PipeLine, CloseStream) :-
  content(In, Entry, CloseStream, PipeLine, []).

content(In, Entry, CloseStream, PipeLine, PipeTail) :-
  setup_call_cleanup(
      archive_open(stream(In), Ar,
       [ format(all), format(raw),
         close_parent(CloseStream)
       ]),
      archive_content(Ar, Entry, PipeLine, PipeTail),
      archive_close(Ar)).

archive_content(Ar, Entry, PipeLine, PipeTail) :-
  archive_property(Ar, filter(Filters)),
  maplist(wrap_filter, Filters, PipeElements),
  append(PipeElements, RestPipe, PipeLine),
  repeat,
  (
    archive_next_header(Ar, Name)
  ->
    findall(
      P,
      archive_header_property(Ar, P),
      Pl
    ),
    dict_create(Pipe, archive_entry, [name(Name)|Pl]),
    (
      Pipe.filetype == file
    ->
      archive_open_entry(Ar, Entry0),
      (
        Name == data,
        Pipe.format == raw
      -> !,
        RestPipe = PipeTail,
        Entry = Entry0
      ;
        RestPipe = [ Pipe | RestPipe1 ],
        content(Entry0, Entry, true, RestPipe1, PipeTail)
      )
    ;
      fail
    )
  ; !,
    fail
  ).

wrap_filter(Filter, filter(Filter)).

     /*******************************
     *      HTTP SUPPORT  *
     *******************************/

:- public ssl_verify/5.

rdf_extra_headers([
  cert_verify_hook(ssl_verify),
  request_header('Accept'=AcceptValue)
]):-
  rdf_accept_header_value(AcceptValue).


%%     ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%      Currently we accept  all  certificates.

ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).



% Messages

:- multifile(prolog:message//1).

prolog:message(unpack(cannot_open(Spec))) -->
  [ 'Unpack: cannot open ~p'-[Spec] ].

