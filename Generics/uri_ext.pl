:- module(
  uri_ext,
  [
    download_to_file/2, % +URL:atom
                        % ?File:atom
    is_image_url/1, % +URL:url
    uri_path/2, % +PathComponents:list(term)
                % -Path:atom
    url_to_file/2, % +URL:atom
                   % -File:atom
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

:- use_module(generics(option_ext)).
:- use_module(http(http)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).



download_to_file(URL, File):-
  nonvar(File),

  % Check the URL.
  uri_is_global(URL),

  % Either the file name is given or it is deduced from the URL.
  (
    is_absolute_file_name(File), !
  ;
    url_to_file(URL, File)
  ),

  http_goal(URL, [nocatch(true)], file_from_stream(File)).
download_to_file(URL, File):-
  url_to_file(URL, File),
  download_to_file(URL, File).

file_from_stream(File, HTTP_Stream):-
  setup_call_cleanup(
    open(File, write, FileStream, [type(binary)]),
    copy_stream_data(HTTP_Stream, FileStream),
    close(FileStream)
  ).


%! is_image_url(+URL:url) is semidet.
% Succeeds if the given URL locates an image file.

is_image_url(URL):-
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
  directory_to_subdirectories(Path, PathComponents),
  create_nested_directory(data([Scheme,Authority|PathComponents]), Dir),
  file_base_name(Path, RelativeFile),
  absolute_file_name(RelativeFile, File, [relative_to(Dir)]).

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

