:- module(
  uri_ext,
  [
    atom_to_email/2, % +Atom:atom
                     % -Email:atom
    atom_to_iri/2, % +Atom:atom
                   % -Iri:iri
    download_and_extract_to_files/3, % +Options:list(nvpair)
                                     % +Url:url
                                     % -Files:ordset(atom)
    download_to_file/3, % +Options:list(nvpair)
                        % +Url:url
                        % ?File:atom
    is_image_url/1, % +Url:url
    uri_path/2, % +PathComponents:list(term)
                % -Path:atom
    url_to_directory_name/2, % +Url:iri
                             % -Directory:atom
    url_to_file_name/2, % +Url:atom
                        % -File:atom
    url_to_graph_name/2, % +Url:url
                         % -Graph:atom
    uri_query_add/4, % +FromURI:uri
                     % +Name:atom
                     % +Value:atom
                     % +ToURI:atom
    uri_query_read/3 % +URI:uri
                     % +Name:atom
                     % -Value:atom
  ]
).

/** <module> URI extensions

@author Wouter Beek
@version 2013/05, 2013/09, 2013/11-2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_replace)).
:- use_module(generics(atom_ext)).
:- use_module(generics(option_ext)).
:- use_module(generics(typecheck)).
:- use_module(http(http)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).



%! atom_to_email(+Atom:atom, -Email:atom) is det.
% Try to make some minor alterations to non-email atoms
% in the hope that they become email addresses.

atom_to_email(Email, Email):-
  is_of_type(email, Email), !.
% Add scheme and scheme-authority separator.
atom_to_email(A1, Email):-
  atomic_list_concat([mailto,A1], ':', A2),
  atom_to_email(A2, Email).
% Remove leading and trailing spaces.
atom_to_email(A1, Email):-
  strip_atom([' '], A1, A2),
  A1 \== A2,
  atom_to_email(A2, Email).


%! atom_to_iri(+Atom:atom, -Iri:iri) is det.
% Try to make some minor alterations to non-Iri atoms
% in the hope that they become IRIs.

atom_to_iri(Iri, Iri):-
  is_of_type(iri, Iri), !.
% Add percent-encoding for spaces!
atom_to_iri(A1, Iri):-
  dcg_phrase(dcg_replace(space, percent_encoding(space)), A1, A2),
  A1 \== A2, !,
  atom_to_iri(A2, Iri).
% Add scheme and scheme-authority separator.
atom_to_iri(A1, Iri):-
  uri_components(A1, uri_components(Scheme,Authority,Path,Query,FragmentId)), !,
  (
    var(Authority)
  ->
    atomic_concat('http://', A1, A2)
  ;
    var(Scheme)
  ->
    uri_components(A2, uri_components(http,Authority,Path,Query,FragmentId))
  ),
  atom_to_iri(A2, Iri).
% Remove leading and trailing spaces.
atom_to_iri(A1, Iri):-
  strip_atom([' '], A1, A2),
  A1 \== A2,
  atom_to_iri(A2, Iri).

percent_encoding(space) -->
  percent_sign,
  integer(20).


%! download_and_extract_to_files(
%!   +Options:list(nvpair),
%!   +Url:url,
%!   -Files:list(atom)
%! ) is det.

download_and_extract_to_files(O1, Url, Files):-
  download_to_file(O1, Url, File),
  file_directory_name(File, Dir),
  extract_archive(File),
  directory_files(
    [
      include_directories(true),
      include_self(false),
      order(lexicographic),
      recursive(true)
    ],
    Dir,
    Files
  ).


%! download_to_file(+Options:list(nvpair), +Url:url, ?File:atom) is det.
% Downloads files from a URL to either the given file (when instantiated)
% of to the a file name that is created based on the URL.
%
% The following options are supported:
%   * =|force(+Redownload:boolean)|=
%     Sets whether files that were downloaded in the past
%     are overwritten or not.
%     Default: `false`.
%   * Other options are passed to http_goal/3 and, subsequently, http_open/3.
%
% @see url_to_file_name/2 for how the file name is created based on the URL.

% The file was already downloaded in the past.
download_to_file(O1, _, File):-
  nonvar(File),
  option(force(false), O1, false),
  exists_file(File), !,
  access_file(File, read).
% An absolute file name is specified.
download_to_file(O1, Url, File):-
  nonvar(File),
  is_absolute_file_name(File), !,
  file_directory_name(File, Dir),
  make_directory_path(Dir),

  % Check write access to the file.
  access_file(File, write),

  % Check the URL.
  uri_is_global(Url),

  % Multiple threads could be downloading the same file,
  % so we cannot download to the file's systematic name.
  % Instead we save to a thread-specific name.
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', ThreadName),
  file_name_extension(File, ThreadName, TmpFile),

  % The actual downloading part.
  merge_options([nocatch(true)], O1, O2),
  http_goal(Url, O2, file_from_stream(TmpFile)),

  % Give the file its original name.
  rename_file(TmpFile, File).
% No file name is given; create a file name is a standardized way,
% based on the URL.
download_to_file(O1, Url, File):-
  url_to_file_name(Url, File),
  download_to_file(O1, Url, File).


file_from_stream(File, HTTP_Stream):-
  setup_call_cleanup(
    open(File, write, FileStream, [type(binary)]),
    copy_stream_data(HTTP_Stream, FileStream),
    close(FileStream)
  ).


%! is_image_url(+Url:url) is semidet.
% Succeeds if the given Url locates an image file.

is_image_url(Url):-
  is_of_type(iri, Url),
  uri_components(
    Url,
    uri_components(_Scheme, _Authority, Path, _Search, _Fragment)
  ),
  is_image_file(Path).


%! uri_path(+URI_PathComponents:list(term), -URI_Path:atom) is det.
% Constructs absolute URI paths out of their constituent components.
%
% # Variable path components
%
% Path components are allowed to be variables.
%
% A sample usage of this is a variable `API_Version` which may or may not
% be instantiated with the version number of an online API.
% Many Web services automatically resolve paths like [1] to paths like [2].
% ~~~
% [1]   /api/something
% [2]   /api/default-version/something
% ~~~

uri_path(T1, Path):-
  exclude(var, T1, T2),
  atomic_list_concat([''|T2], '/', Path).


url_to_directory_name(URI, Dir):-
  uri_components(
    URI,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  atomic_list_concat(Subdirs, '/', Path),
  create_nested_directory(data([Scheme,Authority|Subdirs]), Dir).


%! url_to_file_name(+Url:atom, -File:atom) is det.
% Returns a file name based on the given URI.
%
% @param Url The universal location of a file.
% @param File The atomic name of a file based on the given Url,
%         relative to the given root.

url_to_file_name(Url, File):-
  uri_components(Url, uri_components(Scheme,Authority,UrlPath,_,_)),
  directory_subdirectories(UrlPath, UrlPathComponents),
  directory_subdirectories(
    RelativePath,
    [Scheme,Authority|UrlPathComponents]
  ),
  absolute_file_name(
    data(.),
    RelativeTo,
    [access(read),file_type(directory)]
  ),
  relative_file_path(File, RelativeTo, RelativePath).


url_to_graph_name(Url, G):-
  dcg_phrase(url_to_graph, Url, G).

url_to_graph --> dcg_end, !.
url_to_graph, [X] -->
  [X],
  {code_type(X, alnum)}, !,
  url_to_graph.
url_to_graph, "_" -->
  [_],
  url_to_graph.


%! uri_query_add(+FromURI:uri, +Name:atom, +Value:atom, -ToURI:atom) is det.
% Inserts the given name-value pair as a query component into the given URI.

uri_query_add(URI1, Name, Value, URI2):-
  uri_components(
    URI1,
    uri_components(Scheme, Authority, Path, Search1_, Fragment)
  ),
  (var(Search1_) -> Search1 = '' ; Search1 = Search1_),
  uri_query_components(Search1, SearchPairs1),
  add_option(SearchPairs1, Name, Value, SearchPairs2),
  uri_query_components(Search2, SearchPairs2),
  uri_components(
    URI2,
    uri_components(Scheme, Authority, Path, Search2, Fragment)
  ).

%! uri_query_read(+URI:uri, +Name:atom, -Value:atom) is semidet.
% Returns the value for the query item with the given name, if present.

uri_query_read(URI, Name, Value):-
  uri_components(URI, Components),
  uri_data(search, Components, QueryString),
  uri_query_components(QueryString, QueryPairs),
  member(Name=Value, QueryPairs).

