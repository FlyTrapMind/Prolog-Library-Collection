:- module(
  uri_ext,
  [
    download_to_file/2, % +URL:url
                        % ?File:atom
    is_image_url/1, % +URL:url
    uri_path/2, % +PathComponents:list(term)
                % -Path:atom
    uri_to_file_name/2, % +URI:uri
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
@version 2013/05, 2013/09, 2013/11-2013/12
*/

:- use_module(generics(option_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(uri)).

:- debug(uri_ext).



% Do not download a file that is already locally present.
download_to_file(_URL, File):-
  nonvar(File),
  access_file(File, exist), !.
download_to_file(URL, File):-
  % Check the URL.
  uri_is_global(URL),
  % Either the file name is given or it is deduced from the URL.
  (
    nonvar(File),
    is_absolute_file_name(File), !
  ;
    uri_to_file_name(URL, File)
  ),

  % Copy the remote image to a local file.
  setup_call_cleanup(
    (
      open(File, write, Write, [type(binary)]),
      http_open(URL, Read, [timeout(60)])
    ),
    copy_stream_data(Read, Write),
    (
      close(Write),
      close(Read)
    )
  ).

%! is_image_url(+URL:url) is semidet.
% Succeeds if the given URL locates an image file.

is_image_url(URL):-
  uri_components(
    URL,
    uri_components(_Scheme, _Authority, Path, _Search, _Fragment)
  ),
  directory_file_path(_Dir, File, Path),
  file_name_extension(_Base, Ext, File),
  memberchk(Ext, [jpg,png,svg]).

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

%! uri_to_file_name(+URI:uri, -FileName:atom) is det.
% Returns a file name based on the given URI.

uri_to_file_name(URI, FileName):-
  uri_components(
    URI,
    uri_components(_Scheme, _Auhtority, Path, _Search, _Fragment)
  ),
  file_base_name(Path, RelativeFileName),
  absolute_file_name(data(RelativeFileName), FileName).

%! uri_query_add(+FromURI:uri, +Name:atom, +Value:atom, -ToURI:atom) is det.
% Inserts the given name-value pair as a query component into the given URI.

uri_query_add(URI1, Name, Value, URI2):-
  uri_components(
    URI1,
    uri_components(Scheme, Authority, Path, Search1_, Fragment)
  ),
  (var(Search1_) -> Search1 = '' ; Search1 = Search1_),
  uri_query_components(Search1, SearchPairs1),
  option_add(SearchPairs1, Name, Value, SearchPairs2),
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
