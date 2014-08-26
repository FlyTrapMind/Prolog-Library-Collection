:- module(
  uri_ext,
  [
    relative_url_path/3, % ?Url:url
                         % ?RelativeTo:url
                         % ?RelativeUrl:url
    uri_path/2, % +PathComponents:list(term)
                % -Path:atom
    uri_component/3, % +Uri:uri
                     % +Field:oneof([scheme,authority,path,search,fragment])
                     % ?Data:atom
    url_authority_directory/2, % +Url:atom
                               % -Directory:atom
    url_file_extension/2, % +Url:url
                          % ?FileExtension:atom
    url_flat_directory/3, % +ParentDirectory:atom
                          % +Url:url
                          % -UrlDirectory:atom
    url_nested_directory/3, % +ParentDirectory:atom
                            % +Url:url
                            % -Directory:atom
    url_nested_file/3 % +ParentDirectory:atom
                      % +Url:url
                      % -File:atom
  ]
).

/** <module> URI extensions

@author Wouter Beek
@version 2013/05, 2013/09, 2013/11-2014/04
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(generics(atom_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_query)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- multifile(error:has_type/2).
   error:has_type(email, Term):-
     sub_atom(Term, Before, 1, After, '@'),
     Before > 0,
     After > 0.
   error:has_type(uri, Term):-
     error:has_type(iri, Term).
   error:has_type(iri, Term):-
     uri_components(
       Term,
       uri_components(Scheme,Authority,Path,_Search,_Fragment)
     ),
     maplist(nonvar, [Scheme,Authority,Path]).



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


%! url_file_extension(+Url:url, +FileExtension:atom) is semidet.
%! url_file_extension(+Url:url, -FileExtension:atom) is semidet.
% Returns the empty atom in case there is no file extension.

url_file_extension(Url, FileExtension):-
  % Extract the path.
  uri_components(Url, uri_components(_,_,Path,_,_)),

  % Extract the file.
  atomic_list_concat(PathComponents, '/', Path),
  last(PathComponents, FileComponent),

  % Extract the file extensions.
  atomic_list_concat(FileComponents, '.', FileComponent),
  length(FileComponents, Length),
  Length > 1,
  last(FileComponents, FileExtension).


%! url_flat_directory(
%!   +ParentDirectory:or([atom,compound]),
%!   +Url:url,
%!   -UrlDirectory:atom
%! ) is det.
% Creates a directory for the given URL that is a subdirectory
% of the given parent directory.
%
% This is an easy way to store files related to separate URLs
% in separate places.
%
% This merely gives the directory name,
% but does *not* ensure that the directory exists.
%
% Remote directories are denoted by
% `remote(User:atom,Machine:atom,Directory:atom)`.

url_flat_directory(
  remote(User,Machine,ParentDir),
  Url,
  remote(User,Machine,UrlDir)
):- !,
  url_flat_directory(ParentDir, Url, UrlDir).
url_flat_directory(ParentDir, Url, UrlDir):-
  % A unique name for each URL that does not contain characters
  % that do not comply with POSIX file names.
  rdf_atom_md5(Url, 1, Md5),

  % Make it a subdirectory of the given parent directory
  directory_file_path(ParentDir, Md5, UrlDir).


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
