:- module(
  uri_ext,
  [
    download_to_directory/3, % +URL:atom
                             % +ToDirectory:atom
                             % -AP_Status:compound
    download_to_file/3, % +Options:list(nvpair)
                        % +URL:atom
                        % ?File:atom
    is_image_url/1, % +URL:url
    uri_path/2, % +PathComponents:list(term)
                % -Path:atom
    url_to_file/2, % +URL:atom
                   % -File:atom
    url_to_graph_name/2, % +URL:url
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
@version 2013/05, 2013/09, 2013/11-2014/01
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(option_ext)).
:- use_module(generics(typecheck)).
:- use_module(http(http)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).



download_to_directory(URL, ToDir, ap(status(succeed),download(File3))):-
  url_to_file(URL, File1),
  directory_file_path(_, File2, File1),
  file_name_extensions(Base, Extensions, File2),
  create_file(ToDir, Base, Extensions, File3),
  download_to_file([], URL, File3),
  size_file(File3, Size),
  % Expressed in megabytes.
  TooBig is 1024 * 1024 * 100,
  (Size > TooBig -> permission_error(open,'BIG-file',File3) ; true).


%! download_to_file(+Options:list(nvpair), +URL:atom, ?File:atom) is det.
% Options are passed to http_goal/3, http_open/3.

download_to_file(O1, URL, File):-
  (
    nonvar(File),
    is_absolute_file_name(File)
  ->
    download_to_file0(O1, URL, File)
  ;
    url_to_file(URL, File),
    download_to_file0(O1, URL, File)
  ).

download_to_file0(O1, URL, File1):-
  % Check the URL.
  uri_is_global(URL),
  
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', Name2),
  file_alternative(File1, _, Name2, _, File2),
  
  merge_options([nocatch(true)], O1, O2),
  http_goal(URL, O2, file_from_stream(File2)),
  
  rename_file(File2, File1).


file_from_stream(File, HTTP_Stream):-
  setup_call_cleanup(
    open(File, write, FileStream, [type(binary)]),
    copy_stream_data(HTTP_Stream, FileStream),
    close(FileStream)
  ).


%! is_image_url(+URL:url) is semidet.
% Succeeds if the given URL locates an image file.

is_image_url(URL):-
  is_of_type(iri, URL),
  uri_components(
    URL,
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

%! url_to_file(+URL:atom, -File:atom) is det.
% Returns a file name based on the given URI.
%
% @param URL The universal location of a file.
% @param File The atomic name of a file based on the given URL,
%         relative to the given root.

url_to_file(URI, File):-
  uri_components(
    URI,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  
  % Split path into directory and file names.
  file_directory_name(Path, PathDir),
  file_base_name(Path, PathFile1),
  % When the URL path is a directory,
  %  then the relative file is the empty string.
  % This causes problems when creating the absolute file name,
  %  since the resulting `File` must not be a directory.
  (
    PathFile1 == ''
  ->
    PathFile2 = dummy
  ;
    PathFile2 = PathFile1
  ),
  
  % Create the local directory.
  directory_to_subdirectories(PathDir, PathDirComponents),
  create_nested_directory(data([Scheme,Authority|PathDirComponents]), Dir),
  
  % Construct the local file name.
  absolute_file_name(PathFile2, File, [relative_to(Dir)]).


url_to_graph_name(URL, G):-
  dcg_phrase(url_to_graph, URL, G).

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

