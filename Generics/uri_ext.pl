:- module(
  uri_ext,
  [
    atom_to_email/2, % +Atom:atom
                     % -Email:atom
    atom_to_iri/2, % +Atom:atom
                   % -Iri:iri
    is_image_url/1, % +Url:url
    url_directory/3, % +ParentDirectory:atom
                     % +Url:url
                     % -UrlDirectory:atom
    uri_path/2, % +PathComponents:list(term)
                % -Path:atom
    url_to_directory_name/2, % +Url:url
                             % -Directory:atom
    url_to_file_name/2, % +Url:url
                        % -File:atom
    url_to_graph_name/2 % +Url:url
                        % -Graph:atom
  ]
).

/** <module> URI extensions

@author Wouter Beek
@version 2013/05, 2013/09, 2013/11-2014/04
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_replace)).
:- use_module(generics(atom_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
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


%! is_image_url(+Url:url) is semidet.
% Succeeds if the given Url locates an image file.

is_image_url(Url):-
  is_of_type(iri, Url),
  uri_components(
    Url,
    uri_components(_Scheme, _Authority, Path, _Search, _Fragment)
  ),
  is_image_file(Path).


%! url_directory(+ParentDirectory:atom, +Url:url, -UrlDirectory:atom) is det.

url_directory(ParentDir, Url, UrlDir):-
  rdf_atom_md5(Url, 1, Hash),
  directory_file_path(ParentDir, Hash, UrlDir),
  make_directory_path(UrlDir).


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


url_to_directory_name(Uri, Dir):-
  uri_components(
    Uri,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  atomic_list_concat(Subdirs, '/', Path),
  create_nested_directory(data([Scheme,Authority|Subdirs]), Dir).


%! url_to_file_name(+Url:atom, -File:atom) is det.
% Returns a file name based on the given URI.
%
% @param Url The universal location of a file.
% @param File The atomic name of a file based on the given Url,
%        relative to the given root.

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
  relative_file_path(FileOrDir, RelativeTo, RelativePath),
  (
    sub_atom(FileOrDir, _, 1, 0, '/')
  ->
    rdf_atom_md5(Url, 1, Hash),
    file_name(File, FileOrDir, Hash, _)
  ;
    File = FileOrDir
  ).


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

