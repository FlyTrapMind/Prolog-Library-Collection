:- module(
  file_ext,
  [
    absolute_file_name_number/4, % +Spec
                                 % +Options:list(nvpair)
                                 % +Number:integer
                                 % -Absolute:atom
    base_or_file_to_file/3, % +BaseOrFile:atom
                            % ?FileType:atom
                            % -File:atom
    common_prefix_path/3, % +Path1:atom
                          % +Path2:atom
                          % ?CommonPrefixPath:atom
    copy_file/5, % ?ToDirectory:atom
                 % ?ToName:atom
                 % ?ToExtension:atom
                 % +FromFile:atom
                 % -ToFile:atom
    create_file/1, % +File:atom
    create_file/4, % +NestedDir:term
                   % +Name:atom
                   % +Type:atom
                   % -File:atom
    create_file_directory/1, % +File:atom
    file_age/2, % +File:atom
                % -Age:between(0.0,inf)
    file_alternative/5, % +FromFile:atom
                        % ?Directory:atom
                        % ?Name:atom
                        % ?Extension:atom
                        % -ToFile:atom
    file_lines/2, % +File:atom
                  % -NumberOfLines:nonneg
    file_name/4, % +File:atom
                 % ?Dir:atom
                 % ?Name:atom
                 % ?Ext:atom
    file_name/4, % -File:atom
                 % +Dir:atom
                 % +Name:atom
                 % +Ext:atom
    file_name_type/3, % ?Base:atom
                      % ?Type:atom
                      % ?Name:atom
    file_type/2, % ?FileType:atom
                 % ?File:atom
    file_type_alternative/2, % +FromFile:atom
                             % ?ToFile:atom
    file_type_alternative/3, % +FromFile:atom
                             % +ToFileType:atom
                             % -ToFile:atom
    hidden_file_name/2, % +File:atom
                        % -HiddenFile:atom
    http_path_correction/2, % +HttpPath:atom
                            % -Path:atom
    is_absolute_file_name2/1, % ?File:atom
    is_fresh_file/2, % +File:atom
                     % +FreshnessLifetime:between(0.0,inf)
    last_path_component/2, % +Path:atom
                           % -BaseOrLastSubdir:atom
    link_file/2, % +ToDirectory:atom
                 % +FromFile:atom
    mv2/2, % +From:atom
           % +To:atom
    merge_into_one_file/2, % +FromDir:atom
                           % +ToFile:atom
    new_file/2, % +File1:atom
                % -File2:atom
    prefix_path/2, % ?PrefixPath:atom
                   % +Path:atom
    relative_file_path/3, % ?Path:atom
                          % ?RelativeTo:atom
                          % ?RelativePath:atom
    spec_atomic_concat/3, % +Spec
                          % +Atomic:atom
                          % -NewSpec
    split_into_smaller_files/3, % +BigFile:atom
                                % +SmallDir:atom
                                % +Prefix:atom
    touch_file/1 % +File:atom
  ]
).

/** <module> File methods extending the standart SWI-Prolog repertoire.

Extra methods for creating, opening, removing, and searching files.

# Abbreviations

We use the following abbreviations in this module:
  * Dir
    Directory
  * Ext
    Extension
  * PL
    Prolog
  * QLF
    QuickLoadFormat
  * RE
    RegularExpression

@author Wouter Beek
@tbd Remove the dependency on module AP.
@version 2011/08-2012/05, 2012/09, 2013/04-2013/06, 2013/09-2014/01, 2014/05,
%        2014/08-2014/09
*/

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(readutil)).

:- use_module(generics(atom_ext)).
:- use_module(generics(error_ext)).
:- use_module(generics(list_ext)).
:- use_module(math(math_ext)).
:- use_module(os(dir_ext)).



%! absolute_file_name_number(
%!   +Spec,
%!   +Options:list(nvpair),
%!   +Number:integer,
%!   -Absolute:atom
%! ) is det.
% This comes in handy for numbered files, e.g. '/home/some_user/test_7.txt'.
%
% The order of the arguments differs from absolute_file_name/3
% to be compliant with =library(apply)=.

absolute_file_name_number(Spec, Options, Number, Absolute):-
  atom_number(Atomic, Number),
  spec_atomic_concat(Spec, Atomic, NumberSpec),
  absolute_file_name(NumberSpec, Absolute, Options).


%! base_or_file_to_file(
%!   +BaseOrFile:atom,
%!   +FileType:atom,
%!   -File:atom
%! ) is semidet.
% Predicates that take file arguments can use this to allow either
% absolute file names or file base names to be accepted.
%
% This is useful when there are multiple file extensions associated with
% the same file type and the calling predicate only looks at the file type
% level.
%
% @arg BaseOrFile Either a full file name or the base name of a file.
%      In the former case we check for a supported file extension.
%      In the latter case we add a supported file extension.
% @arg FileType The atomic name of a registered file type.
% @arg File An absolute file name.

base_or_file_to_file(BaseOrFile, FileType, File):-
  (
    file_type(FileType, BaseOrFile)
  ->
    File = BaseOrFile
  ;
    file_name_type(BaseOrFile, FileType, File)
  ),
  access_file(File, read),
  % Since there may be multiple file type / file extension translations,
  % the above may backtrack. Therefore we discard these choice-points here.
  % I.e., we only use the first file we find.
  !.


%! common_prefix_path(
%!   +Path1:atom,
%!   +Path2:atom,
%!   +CommonPrefixPath:atom
%! ) is semidet.
%! common_prefix_path(
%!   +Path1:atom,
%!   +Path2:atom,
%!   -CommonPrefixPath:atom
%! ) is det.

common_prefix_path(Path1, Path2, CommonPrefixPath):-
  directory_subdirectories(Path1, PathComponents1),
  directory_subdirectories(Path2, PathComponents2),
  common_list_prefix(PathComponents1, PathComponents2, CommonComponentPrefix),
  directory_subdirectories(CommonPrefixPath, CommonComponentPrefix).


%! copy_file(
%!   ?ToDirectory:atom,
%!   ?ToName:atom,
%!   ?ToExtension:atom,
%!   +FromFile:atom,
%!   -ToFile:atom
%! ) is det.

copy_file(ToDir, ToName, ToExt, FromFile, ToFile):-
  file_alternative(FromFile, ToDir, ToName, ToExt, ToFile),
  copy_file(FromFile, ToFile).


create_file(File):-
  exists_file(File), !,
  debug(file_ext, 'File ~w already exists.', [File]).
create_file(File):-
  is_absolute_file_name(File), !,
  touch_file(File).
create_file(File):-
  type_error(absolute_file_name, File).


%! create_file(+NestedDir:term, +Name:atom, +Type:atom, -File:atom) is det.
% Creates a file with the given name, inside the given directory, and that
% is of the given file type.
%
% File types are resolved using prolog_file_type/2.
%
% @arg NestedDir The atomic name of a directory or a compound term that
%      can be resolved by subsequent applications of
%      absolute_file_name/[2,3], e.g. =|aaa(bbb(ccc))|=.
% @arg Base The atomic base name of a file.
% @arg TypeOrExtension The atomic name of a file type, as registered with
%      prolog_file_type/2, e.g. =|mp3|=.
% @arg File The atomic absolute name of a file.

create_file(NestedDir, Base, TypeOrExtension, File):-
  % Resolve the directory in case the compound term notation employed
  % by absolute_file_name/3 is used.
  (
    compound(NestedDir)
  ->
    absolute_file_name(NestedDir, Directory)
  ;
    Directory = NestedDir
  ),

  % Make sure that the directory exists.
  create_directory(Directory),

  % Create the local file name by appending the base and extension names.
  % The extension must be of the given type.
  (
    file_name_type(Base, TypeOrExtension, Local)
  ->
    true
  ;
    file_name_extension(Base, TypeOrExtension, Local)
  ),

  % Append directory and file name.
  directory_file_path(Directory, Local, File),

  create_file(File).


%! create_file_directory(+File:atom) is det.
% Ensures that the directory structure for the given file exists.

create_file_directory(File):-
  file_name(File, Dir, _, _),
  create_directory(Dir).


%! file_age(+File:atom, -Age:between(0.0,inf)) is det.

file_age(File, Age):-
  time_file(File, LastModified),
  get_time(Now),
  Age is Now - LastModified.


%! file_alternative(
%!   +FromFile:atom,
%!   ?ToDirectory:atom,
%!   ?ToName:atom,
%!   ?ToExtension:atom,
%!   -ToFile:atom
%! ) is det.
% Creates similar file names, allowing a different
% directory, base name, and/or extension to be specified.

file_alternative(FromFile, ToDir1, ToName1, ToExt1, ToFile):-
  file_name(FromFile, FromDir, FromName, FromExt),
  maplist(
    default,
    [ToDir1,ToName1,ToExt1],
    [FromDir,FromName,FromExt],
    [ToDir2,ToName2,ToExt2]
  ),
  file_name(ToFile, ToDir2, ToName2, ToExt2).


%! file_lines(+File:atom, -NumberOfLines:nonneg) is det.

file_lines(File, NumberOfLines):-
  process_create(path(wc), ['-l',file(File)], [stdout(pipe(Stream))]),
  read_stream_to_codes(Stream, Codes),
  phrase(integer(NumberOfLines), Codes).


%! file_name(
%!   ?Path:atom,
%!   ?Directory:atom,
%!   ?Base:atom,
%!   ?Extension:atom
%! ) is semidet.
% The splitting of a file into its directory, local name and type parts.
%
% For directories, the base and extension are the empty atom.

file_name(Path, Directory, Base, Extension):-
  nonvar(Directory), nonvar(Base), nonvar(Extension), !,
  file_name_extension(Base, Extension, File),
  directory_file_path(Directory, File, Path).
file_name(Path1, Directory, Base, Extension):-
  nonvar(Path1), !,
  http_path_correction(Path1, Path2),
  (
    exists_directory(Path2)
  ->
    Directory = Path2,
    Base = '',
    Extension = ''
  ;
    directory_file_path(Directory, File, Path2),
    file_name_extension(Base, Extension, File)
  ).


%! file_name_type(?Base:atom, ?Type:atom, ?File:atom) is semidet.
% Decomposes a file name into its base name and its file type.
%
% @arg Base The atomic name of a file, without a directory and without
%      an extension.
% @arg Type An atomic file type. These are registered with
%	    prolog_file_type/2, or uninstantiated in case the type
%	    could not be established.
% @arg File The full name of a file.

file_name_type(File, directory, File):-
  nonvar(File),
  exists_directory(File), !.
% (+,+,-)
file_name_type(Base, Type, File):-
  maplist(nonvar, [Base,Type]), !,
  prolog_file_type(Extension, Type),
  file_name_extension(Base, Extension, File).
% For files with no extension and thus no type.
file_name_type(File, none, File):-
  \+ file_name_extension(_, _, File),
  exists_file(File), !.
% (?,?,+)
file_name_type(Base, Type, File):-
  nonvar(File),
  file_name_extension(Base, Extension, File),
  (
    Extension \== '',
    user:prolog_file_type(Extension, Type)
  ->
    % The file extension is registered with a type.
    true
  ;
    % The file extension is not registered with a type,
    % or there is no extension at all.
    % Make sure the file type is uninstantiated.
    var(Type)
  ).
file_name_type(_, Type, _):-
  % Type is uninstantiated,
  % make sure it stays that way.
  var(Type).


file_type(FileType, File):-
  file_name_type(_, FileType, File).


%! file_type_alternative(+File1:atom, +File2:atom) is semidet.
% Succeeds if the files are type-alternatives of each other.

file_type_alternative(File1, File2):-
  file_name_extension(Base, _Extension1, File1),
  file_name_extension(Base, _Extension2, File2).

%! file_type_alternative(
%!   +FromFile:atom,
%!   +ToFileType:atom,
%!   -ToFile:atom
%! ) is det.
% Returns an alternative of the given file with the given file type.
%
% @arg FromFile The atomic name of a file.
% @arg ToFileType The atomic name of a file type.
% @arg ToFile The atomic name of a file.

file_type_alternative(FromFile, ToFileType, ToFile):-
  file_name_type(Base, _FromFileType, FromFile),
  file_name_type(Base, ToFileType, ToFile), !.

%! hidden_file_name(+FileName:atom, -HiddenFileName:atom) is det.
% Returns the hidden file name for the given atomic name.

hidden_file_name(FileName, HiddenFileName):-
  atomic(FileName), !,
  atomic_concat('.', FileName, HiddenFileName).


http_path_correction(Path1, Path2):-
  atom_concat('file://', Path2, Path1), !.
http_path_correction(Path, Path).


%! is_absolute_file_name2(+File:atom) is semidet.
% Wrapper around is_absolute_file_name/1 that fails for variable arguments.
%
% @see is_absolute_file_name/1

is_absolute_file_name2(F):-
  var(F), !, fail.
is_absolute_file_name2(F):-
  is_absolute_file_name(F).


%! is_fresh_file(+File:atom, +FreshnessLifetime:between(0.0,inf)) is semidet.

is_fresh_file(File, FreshnessLifetime):-
  file_age(File, Age),
  is_fresh_age(Age, FreshnessLifetime).


%! last_path_component(+Path:atom, -BaseOrLastSubdir:atom) is det.
% Returns the last path component.
% If `Path` is a (non-directory) file, then this is the base name.
% If `Path` is a directory, then this is the last subdirectory name.

last_path_component(Path, Base):-
  file_base_name(Path, Base),
  Base \== '', !.
last_path_component(Dir, LastSubdir):-
  directory_subdirectories(Dir, Subdirs),
  last(Subdirs, LastSubdir).


%! link_file(+ToDir:atom, +FromFile:atom) is det.
% Create a single symbolic link for the given file.
% The symbolic link has the same base nane and extension as the file
%  linked to.

link_file(ToDir, FromFile):-
  file_alternative(FromFile, ToDir, _, _, ToFile),
  link_file(FromFile, ToFile, symbolic).


%! mv2(+From:atom, +To:atom) is det.

mv2(From, To):-
  copy_file(From, To),
  delete_file(From).


%! merge_into_one_file(+FromDir:atom, +ToFile:atom) is det.
% RE and To must be in the same directory.
% How arbitrary this restriction is!

merge_into_one_file(FromDir, ToFile):-
  directory_files(
    [
      file_types([text]),
      include_directories(false),
      % It may for instance be reasonable to assume that
      % files with names ending in numbers are merged in the order
      % that is indicated by those numbers.
      order(lexicographic),
      recursive(true)
    ],
    FromDir,
    FromFiles
  ),

  setup_call_cleanup(
    open(ToFile, write, Out, [type(binary)]),
    maplist(merge_into_one_stream(Out), FromFiles),
    close(Out)
  ).

merge_into_one_stream(Out, FromFile):-
  setup_call_cleanup(
    open(FromFile, read, In, [type(binary)]),
    copy_stream_data(In, Out),
    close(In)
  ).


%! prefix_path(+PrefixPath:atom, +Path:atom) is semidet.
%! prefix_path(-PrefixPath:atom, +Path:atom) is multi.

prefix_path(PrefixPath, Path):-
  var(PrefixPath), !,
  directory_subdirectories(Path, Components),
  prefix(PrefixComponents, Components),
  directory_subdirectories(PrefixPath, PrefixComponents).
prefix_path(PrefixPath, Path):-
  maplist(
    directory_subdirectories,
    [PrefixPath,Path],
    [PrefixComponents,Components]
  ),
  prefix(PrefixComponents, Components).


%! new_file(+OldFile:atom, -NewFile:atom) is det.
% If a file with the same name exists in the same directory, then
% then distinguishing integer is appended to the file name.

new_file(F, F):-
  \+ exists_file(F), !.
new_file(F1, F2):-
  file_name_extension(Base1, Ext, F1),
  new_atom(Base1, Base2),
  file_name_extension(Base2, Ext, F2).


%! relative_file_path(
%!   +Path:atom,
%!   +RelativeTo:atom,
%!   -RelativePath:atom
%! ) is det.
%! relative_file_path(
%!   -Path:atom,
%!   +RelativeTo:atom,
%!   +RelativePath:atom
%! ) is det.

relative_file_path(Path, RelativeTo, RelativePath):-
  maplist(nonvar, [Path,RelativeTo]), !,
  relative_file_name(Path, RelativeTo, RelativePath).
relative_file_path(Path, RelativeTo, RelativePath):-
  maplist(nonvar, [RelativeTo,RelativePath]), !,
  directory_subdirectories(RelativePath, RelativePathSubs1),
  uplength(RelativePathSubs1, Uplength, RelativePathSubs2),
  directory_subdirectories(RelativeTo, RelativeToSubs1),
  length(Postfix, Uplength),
  % Actively close the choicepoint on `Postfix` instantiation.
  once(append(RelativeToSubs2, Postfix, RelativeToSubs1)),
  append(RelativeToSubs2, RelativePathSubs2, PathSubs),
  directory_subdirectories(Path, PathSubs).

uplength(['..'|T1], N1, T2):- !,
  uplength(T1, N2, T2),
  N1 is N2 + 1.
uplength(L, 0, L).


%! spec_atomic_concat(+Spec, +Atomic:atom, -NewSpec) is det.
% Concatenates the given atom to the inner atomic term of the given
% specification.

spec_atomic_concat(Atomic1, Atomic2, Atom):-
  atomic(Atomic1), !,
  atomic_concat(Atomic1, Atomic2, Atom).
spec_atomic_concat(Spec1, Atomic, Spec2):-
  compound(Spec1), !,
  Spec1 =.. [Outer, Inner1],
  spec_atomic_concat(Inner1, Atomic, Inner2),
  Spec2 =.. [Outer, Inner2].

split_into_smaller_files(BigFile, SmallDir, Prefix):-
  % Split the big file by byte size into small files.
  % (We cannot split on the number of lines since the file is one big line.)
  process_create(
    path(split),
    ['--bytes=1m','-d','--suffix-length=4',BigFile,Prefix],
    [cwd(SmallDir)]
  ),
  debug(
    file_ext,
    'File ~w was split into smaller files in directory ~w.',
    [BigFile,SmallDir]
  ).


%! touch_file(+File:atom) is det.

touch_file(File):-
  process_create(path(touch), [File], [detached(true)]).

