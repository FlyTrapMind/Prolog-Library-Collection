:- module(
  uri_ext,
  [
    atom_to_email/2, % +Atom:atom
                     % -Email:atom
    atom_to_iri/2, % +Atom:atom
                   % -Iri:iri
    is_image_url/1, % +Url:url
    relative_url_path/3, % ?Url:url
                         % ?RelativeTo:url
                         % ?RelativeUrl:url
    uri_path/2, % +PathComponents:list(term)
                % -Path:atom
    uri_component/3, % +Uri:uri
                     % +Field:oneof([scheme,authority,path,search,fragment])
                     % +Data:atom
    url_authority_directory/2, % +Url:atom
                               % -Directory:atom
    url_flat_directory/3, % +ParentDirectory:atom
                          % +Url:url
                          % -UrlDirectory:atom
    url_nested_directory/3, % +ParentDirectory:atom
                            % +Url:url
                            % -Directory:atom
    url_nested_file/3, % +ParentDirectory:atom
                       % +Url:url
                       % -File:atom
    url_rdf_graph/2 % +Url:url
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
:- use_module(generics(uri_query)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- multifile(error:has_type/2).



% email/0
error:has_type(email, Term):-
  dcg_phrase(email, Term).
% uri/0
error:has_type(uri, Term):-
  error:has_type(iri, Term).
% iri/0
error:has_type(iri, Term):-
  uri_components(
    Term,
    uri_components(Scheme,Authority,Path,_Search,_Fragment)
  ),
  maplist(nonvar, [Scheme,Authority,Path]).
  % @tbd
  %%%%once(dcg_phrase('IRI'(_), Term)),

email -->
  dcg_until([end_mode(inclusive),output_format(codes)], at_sign, _),
  dcg_all.


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
  uri_component(Url, path, Path),
  is_image_file(Path).


%! relative_url_path(+Url:url, +RelativeTo:url, -RelativeUrl:url) is det.
%! relative_url_path(-Url:url, +RelativeTo:url, +RelativeUrl:url) is det.

relative_url_path(Url, RelativeTo1, Relative1):-
  maplist(nonvar, [Url,RelativeTo1]), !,
  uri_components(Url, uri_components(Scheme,Authority,Path,Search,Fragment)),
  uri_components(
    RelativeTo1,
    uri_components(Scheme,Authority,RelativeTo2,'','')
  ),
  relative_file_path(Path, RelativeTo2, Relative2),
  uri_components(Relative1, uri_components('','',Relative2,Search,Fragment)).
relative_url_path(Url, RelativeTo1, Relative1):-
  maplist(nonvar, [RelativeTo1,Relative1]), !,
  uri_components(
    RelativeTo1,
    uri_components(Scheme,Authority,RelativeTo2,'','')
  ),
  uri_components(
    Relative1,
    uri_components(Scheme,Authority,Relative2,Search,Fragment)
  ),
  relative_file_path(Path, RelativeTo2, Relative2),
  uri_components(Url, uri_components(Scheme,Authority,Path,Search,Fragment)).


%! uri_component(
%!   +Uri:uri,
%!   +Field:oneof([scheme,authority,path,search,fragment]),
%!   +Data:atom
%! ) is semidet.
%! uri_component(
%!   +Uri:uri,
%!   +Field:oneof([scheme,authority,path,search,fragment]),
%!   -Data:atom
%! ) is det.
%! uri_component(
%!   +Uri:uri,
%!   -Field:oneof([scheme,authority,path,search,fragment]),
%!   -Data:atom
%! ) is nondet.
% Abbreviates two predicates from `library(uri)` into one:
%   - uri_components/2
%   - uri_data/3

uri_component(Uri, Field, Data):-
  uri_components(Uri, Components),
  uri_data(Field, Components, Data).


%! uri_path(+PathComponents:list(atom), -Path:atom) is det.
% Constructs absolute URI paths out of their constituent components.
%
% # Variable path components
%
% Path components are allowed to be variables.
%
% A sample usage of this is a variable `ApiVersion` which may or may not
% be instantiated with the version number of an online API.
%
% Many Web services automatically resolve paths like [1] to paths like [2].
% ~~~
% [1]   /api/something
% [2]   /api/default-version/something
% ~~~

uri_path(PathComponents1, Path):-
  % Exclude the variable components.
  exclude(var, PathComponents1, PathComponents2),

  % A URI path is similar enough to a POSIX path.
  directory_subdirectories(Path, PathComponents2).


%! url_authority_directory(+Url:atom, -Directory:atom) is det.

url_authority_directory(Url, Dir):-
  uri_component(Url, scheme, Scheme),
  uri_component(Url, authority, Authority),
  absolute_file_name(data(.), DataDir, [access(read),file_type(directory)]),
  directory_subdirectories(DataDir, DataDirComponents),
  append(DataDirComponents, [Scheme,Authority], DirComponents),
  directory_subdirectories(Dir, DirComponents).


%! url_flat_directory(+ParentDirectory:atom, +Url:url, -UrlDirectory:atom) is det.
% Creates a directory for the given URL that is a subdirectory
% of the given parent directory.
%
% This is an easy way to store files related to separate URLs
% in separate places.

url_flat_directory(ParentDir, Url, UrlDir):-
  % A unique name for each URL that does not contain characters
  % that do not comply with POSIX file names.
  rdf_atom_md5(Url, 1, Hash),

  % Make it a subdirectory of the given parent directory
  directory_file_path(ParentDir, Hash, UrlDir),

  % Make sure the directory exists.
  (
    exists_directory(UrlDir)
  ->
    true
  ;
    make_directory_path(UrlDir)
  ).


%! url_nested_directory(+ParentDirectory:atom, +Url:url, -Directory:atom) is det.
% Returns a nested path denoting a directory
% that is as similar as possible to the original URL.

url_nested_directory(ParentDir1, Url, Dir):-
  uri_component(Url, scheme, Scheme),
  uri_component(Url, authority, Authority),
  uri_component(Url, path, Path),
  directory_subdirectories(Path, PathComponents),
  directory_subdirectories(UrlPath, [Scheme,Authority|PathComponents]),
  absolute_file_name(ParentDir1, ParentDir2, [file_type(directory)]),
  relative_file_path(Dir, ParentDir2, UrlPath),
  make_directory_path(Dir).


%! url_nested_file(+ParentDirectory, +Url:url, -File:atom) is det.
% Returns a nested path denoting a regular file
% that is based on the original URL.
%
% The purpose of this predicate is to create file names that
% resemble the URL as most as possible for a human being to discern.
%
% @arg ParentDirectory The directory relative to which
%      the URL-based files are stored.
%      This is either (1) an absolute file name of a directory file,
%      or (2) a relative file name of a directory file,
%      or (3) a specification of the form `outer(inner)`,
%      where `outer` can be resolved using `user:file_search_path/2`
%      declarations.
% @arg Url A standards-compliant URL on which the file name is based.
% @arg File A non-directory absolute file name,
%      whose directory either exists or is created by this predicate.

url_nested_file(ParentDir1, Url, File):-
  % Extract the URL components that will be used.
  uri_component(Url, scheme, Scheme),
  uri_component(Url, authority, Authority),
  uri_component(Url, path, Path),
  
  % Make sure the path ends in a non-directory file.
  %
  % According to the Prolog library a file ending in `.../a/b/`
  % has `.../a` as its directory.
  % If we do nothing we would end up with the non-directory file `.../a/b`.
  % Now, a problem occurs in case another file `.../a/b/c` exists,
  % since `.../a/b` is now supposed to be a directory file.
  %
  % We circumvent this by adding a dummy file name.
  % This means that data about a directory file are stored in
  % a summy file of that directory.
  (
    sub_atom(Path, _, 1, 0, '/')
  ->
    PathDir = Path,
    Base = directory_dummy
  ;
    file_directory_name(Path, PathDir),
    file_base_name(Path, Base)
  ),
  
  % Use (1) the URL scheme, (2) the URL authority,
  % and (3) the directory-part of the URL path to construct
  % the directory of the URL-based file.
  directory_subdirectories(PathDir, PathDirComponents),
  directory_subdirectories(UrlPath, [Scheme,Authority|PathDirComponents]),
  
  % Resolve the parent directory input to an absolute file name.
  absolute_file_name(ParentDir1, ParentDir2, [file_type(directory)]),
  
  % The URL path is now created relative to the parent directory.
  relative_file_path(Dir, ParentDir2, UrlPath),
  
  % Make sure the directory exists.
  make_directory_path(Dir),
  
  % Return the file.
  directory_file_path(Dir, Base, File).


%! url_rdf_graph(+Url:url, -RdfGraph:atom) is det.

url_rdf_graph(Url, G):-
  dcg_phrase(url_to_graph, Url, G).

url_to_graph --> dcg_end, !.
url_to_graph, [X] -->
  [X],
  {code_type(X, alnum)}, !,
  url_to_graph.
url_to_graph, "_" -->
  [_],
  url_to_graph.

