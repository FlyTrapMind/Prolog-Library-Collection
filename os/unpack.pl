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

:- use_module(library(http/http_open)).
:- use_module(library(archive)).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(http/http_cookie)). % Sometimes redirection requires cookies.
:- use_module(library(http/http_ssl_plugin)).



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


%! open_input(+Spec, -Read:blob, -MetaData:dict, -Close:boolean) is semidet.

open_input(stream(In), In, stream{stream:In}, false) :- !.
open_input(In, In, stream{stream:In}, false) :-
  is_stream(In), !.
open_input(URL, In, Meta, true) :-
  uri_components(URL, Components),
  uri_data(scheme, Components, Scheme),
  nonvar(Scheme),
  Scheme \== file, !,
  open_url(Scheme, URL, In, Meta).
open_input(URL, In, file{path:File}, true) :-
  uri_file_name(URL, File), !,
  open(File, In, [type(binary)]).
open_input(Spec, In, file{path:Path}, true) :-
  compound(Spec), !,
  absolute_file_name(Spec, Path, [access(read)]),
  open(Path, read, In, [type(binary)]).
open_input(File, In, file{path:File}, true) :-
  exists_file(File), !,
  open(File, read, In, [type(binary)]).
open_input(Pattern, In, Location, Close) :-
  atom(Pattern),
  expand_file_name(Pattern, Files),
  Files \== [], Files \== [Pattern], !,
  member(File, Files),
  open_input(File, In, Location, Close).
open_input(Input, _, _, _) :-
  print_message(warning, unpack(cannot_open(Input))),
  fail.


%! open_url(+Scheme:atom, +Url:url, -In:blob, -MetaData:dict) is semidet.
% Helper for open_input/4 that deals with URLs

open_url(Scheme, Url, In, Meta) :-
  http_scheme(Scheme), !,
  rdf_extra_headers(Extra),
  http_open(
    Url,
    In,
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

rdf_accept_header_value(Value):-
  findall(
    Value,
    (
      rdf_content_type(ContentType, Q),
      format(atom(Value), '~a; q=~1f', [ContentType,Q])
    ),
    Values
  ),
  atomic_list_concat(Values, ', ', Value).


% RDFa
rdf_content_type('text/html',              0.3).
% N-Quads
rdf_content_type('application/n-quads',    0.8).
% N-Triples
rdf_content_type('application/n-triples',  0.8).
% RDF/XML
rdf_content_type('application/rdf+xml',    0.7).
rdf_content_type('text/rdf+xml',           0.7).
rdf_content_type('application/xhtml+xml',  0.3).
rdf_content_type('application/xml',        0.3).
rdf_content_type('text/xml',               0.3).
rdf_content_type('application/rss+xml',    0.5).
% Trig
rdf_content_type('application/trig',       0.8).
rdf_content_type('application/x-trig',     0.5).
% Turtle
rdf_content_type('text/turtle',            0.9).
rdf_content_type('application/x-turtle',   0.5).
rdf_content_type('application/turtle',     0.5).
rdf_content_type('application/rdf+turtle', 0.5).
% N3
rdf_content_type('text/n3',                0.8).
rdf_content_type('text/rdf+n3',            0.5).
% All
rdf_content_type('*/*',                    0.1).


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

