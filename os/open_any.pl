:- module(
  open_any,
  [
    open_any/3 % +Input
               % -Substream:stream
               % -Options:list(nvpair)
  ]
).

/** <module> RDF: load any

Load RDF data from various sources.

@author Jan Wielemaker
@author Wouter Beek
@tbd Only supports URI schemes `http` and `https`.
@version 2014/03-2014/07, 2014/10
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(http/http_cookie)). % Redirection may require cookies.
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).

:- predicate_options(open_any/3, 3, [
     metadata(-dict),
     pass_to(open_input/4, 4)
   ]).
:- predicate_options(open_input/4, 4, [
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


%! open_any(+Input, -Out:stream, +Options:list(nvpair)) is nondet.
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

open_any(Input, Out, Options):-
  open_input(Input, FileOut, Metadata0, Close, Options),

  % Metadata option.
  (   option(metadata(Metadata), Options)
  ->  Metadata = Metadata0.put(data, Data)
  ;   true
  ),
  
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
      merge_options(
        [
          header(content_type, ContentType),
          header(content_length, ContentLength),
          header(last_modified, ModifiedText)
        ],
        Options1,
        Options2
      ),
      http_open(Uri, Out, Options2),
      url_meta_pairs(
        [
          content_type=ContentType,
          content_length=ContentLength,
          last_modified=ModifiedText
        ],
        HeaderPairs
      ),
      dict_pairs(Metadata, url, [url-Uri|HeaderPairs]),
      Close = true
  ).

% A4. URI: convert to URI components term.
open_input(uri(Url), Out, Metadata, Close):- !,
  uri_components(Url, UriComponents),
  open_input(UriComponents, Out, Metadata, Close).

% A4'. URL: same as URI.
open_input(url(Url), Out, Metadata, Close):- !,
  open_input(uri(Url), Out, Metadata, Close).

% A5. File pattern
open_input(pattern(Pattern), Out, Metadata, Close) :-
  atom(Pattern),
  expand_file_name(Pattern, Files),
  Files \== [],
  Files \== [Pattern], !,
  % Backtrack over files.
  member(File, Files),
  open_input(File, Out, Metadata, Close).

% A6. File specification.
open_input(file_spec(Spec), Out, file{path:Path}, true):-
  compound(Spec), !,
  absolute_file_name(Spec, Path, [access(read)]),
  open(Path, read, Out, [type(binary)]).


% B. Stream
open_input(Stream, Out, Metadata, Close):-
  is_stream(Stream), !,
  open_input(stream(Stream), Out, Metadata, Close).


% C1. File
open_input(File, Out, Metadata, Close):-
  exists_file(File), !,
  open_input(file(File), Out, Metadata, Close).

% C2. URI
open_input(Uri, Out, Metadata, Close):-
  uri_components(Uri, UriComponents), !,
  open_input(UriComponents, Out, Metadata, Close).

% C3. File specification
open_input(Spec, Out, Metadata, Close):-
  absolute_file_name(Spec, File, [access(read),file_errors(fail)]), !,
  open_input(file(File), Out, Metadata, Close).

% C4. File pattern
open_input(Pattern, Out, Metadata, Close):-
  expand_file_name(Pattern, Files),
  File \== [], !,
  member(File, Files),
  open_input(file(File), Out, Metadata, Close).


% D. Out of options...
open_input(Input, _, _, _):-
  print_message(warning, cannot_open(Input)),
  fail.



% HELPERS

%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).


%! url_meta_pair(
%!   +Metadata1:list(nvpair),
%!   -Metadata2:list(pair(atomic))
%! ) is det.

url_meta_pairs([], []).
url_meta_pairs([_=''|T0], T):- !,
  url_meta_pairs(T0, T).
url_meta_pairs([content_length=Atom|T0], [content_length-Bytes|T]):-
  atom_number(Atom, Bytes), !,
  url_meta_pairs(T0, T).
url_meta_pairs([Name=Value|T0], [Name-Value|T]):- !,
  url_meta_pairs(T0, T).


%! wrap_filter(+FilterName:atom, -FilterTerm:compound) is det.

wrap_filter(Filter, filter(Filter)).



% MESSAGES

:- multifile(prolog:message//1).

prolog:message(cannot_open(Input)) -->
  ['Cannot open input ',Input,nl].
