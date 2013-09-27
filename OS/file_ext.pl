:- module(
  file_ext,
  [
    absolute_file_name2/3, % +Spec:compound
                           % -File:atom
                           % +Options:list(nvpair)
    base_or_file_to_file/3, % +BaseOrFile:atom
                            % ?FileType:atom
                            % -File:atom
    create_file/1, % +File:atom
    create_file/4, % +NestedDir:term
                   % +Name:atom
                   % +Type:atom
                   % -File:atom
    file_extension_alternative/3, % +FromFile:atom
                                  % +ToExtension:atom
                                  % -ToFile:atom
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
                      % +Name:atom
    file_name_type/3, % +Base:atom
                      % +Type:atom
                      % ?Name:atom
    file_type_alternative/2, % +FromFile:atom
                             % ?ToFile:atom
    file_type_alternative/3, % +FromFile:atom
                             % +ToFileType:atom
                             % -ToFile:atom
    hidden_file_name/2, % +File:atom
                        % -HiddenFile:atom
    merge_into_one_file/2, % +FromDir:atom
                           % +ToFile:atom
    new_file/2, % +File1:atom
                % -File2:atom
    safe_copy_file/2, % +From:atom
                      % +To:atom
    safe_delete_file/1, % +File:atom
    safe_move_file/2, % +From:atom
                      % +To:atom
    safe_rename_file/2, % +From:atom
                        % +To:atom
    spec_atomic_concat/3, % +Spec
                          % +Atomic:atom
                          % -NewSpec
    split_into_smaller_files/3 % +BigFile:atom
                               % +SmallDir:atom
                               % +Prefix:atom
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
@version 2011/08-2012/05, 2012/09, 2013/04-2013/06, 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(print_ext)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(os(dir_ext)).
:- use_module(os(os_ext)).

:- debug(file_ext).



%! absolute_file_name2(
%!   +Spec:compound,
%!   -File:atom,
%!   +Options:list(nvpair)
%! ) is semidet.
% @see Like absolute_file_name/3 but fails on existence exceptions.

absolute_file_name2(Spec, F, O1):-
  catch(
    absolute_file_name(Spec, F, O1),
    error(existence_error(_,_), _),
    fail
  ).

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
% @param BaseOrFile Either a full file name or the base name of a file.
%      In the former case we check for a supported file extension.
%      In the latter case we add a supported file extension.
% @param FileType The atomic name of a registered file type.
% @param File An absolute file name.

base_or_file_to_file(BaseOrFile, FileType, File):-
  (
    file_name_type(_Base, FileType, BaseOrFile)
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

create_file(File):-
  is_absolute_file_name(File), !,
  touch(File).

%! create_file(+NestedDir:term, +Name:atom, +Type:atom, -File:atom) is det.
% Creates a file with the given name, inside the given directory, and that
% is of the given file type.
%
% File types are resolved using prolog_file_type/2.
%
% @param NestedDir The atomic name of a directory or a compound term that
%      can be resolved by subsequent applications of
%      absolute_file_name/[2,3], e.g. =|aaa(bbb(ccc))|=.
% @param Base The atomic base name of a file.
% @param Type The atomic name of a file type, as registered with
%      prolog_file_type/2, e.g. =|mp3|=.
% @param File The atomic absolute name of a file.

create_file(NestedDir, Base, Type, File):-
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
  once(file_name_type(Base, Type, Local)),

  % Append directory and file name.
  directory_file_path(Directory, Local, File),

  create_file(File).

file_extension_alternative(FromFile, ToExtension, ToFile):-
  file_name_extension(Base, _FromExtension, FromFile),
  file_name_extension(Base, ToExtension, ToFile).

%! file_name(
%!   +Path:atom,
%!   ?Directory:atom,
%!   ?Base:atom,
%!   ?Extension:atom
%! ) is semidet.
%! file_name(
%!   -Path:atom,
%!   +Directory:atom,
%!   +Base:atom,
%!   +Extension:atom
%! ) is det.
% The splitting of a file into its directory, local name and type parts.

file_name(Path, Directory, Base, Extension):-
  nonvar(Directory), nonvar(Base), nonvar(Extension), !,
  file_name_extension(Base, Extension, File),
  directory_file_path(Directory, File, Path).
file_name(Path, Directory, Base, Extension):-
  nonvar(Path), !,
  directory_file_path(Directory, File, Path),
  file_name_extension(Base, Extension, File).

%! file_name_type(?Name:atom, ?Type:atom, +Path:atom) is semidet.
%! file_name_type(+Name:atom, +Type:atom, ?Path:atom) is semidet.
% Decomposes a file name into its base name and its file type.
%
% @param Name The atomic name of a file, without a directory and without
%           an extension.
% @param Type An atomic file type. These are registered with
%           prolog_file_type/2.
% @param Path The full name of a file.

file_name_type(Path, directory, Path):-
  exists_directory(Path), !.
file_name_type(Name, Type, Path):-
  nonvar(Name), nonvar(Type), !,
  prolog_file_type(Ext, Type),
  file_name_extension(Name, Ext, Path).
file_name_type(Path, directory, Path):-
  nonvar(Path), exists_directory(Path), !.
file_name_type(Name, Type, Path):-
  nonvar(Path), !,
  file_name_extension(Name, Ext, Path),
  user:prolog_file_type(Ext, Type).

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
% @param FromFile The atomic name of a file.
% @param ToFileType The atomic name of a file type.
% @param ToFile The atomic name of a file.

file_type_alternative(FromFile, ToFileType, ToFile):-
  file_name_type(Base, _FromFileType, FromFile),
  file_name_type(Base, ToFileType, ToFile), !.

%! hidden_file_name(+FileName:atom, -HiddenFileName:atom) is det.
% Returns the hidden file name for the given atomic name.

hidden_file_name(FileName, HiddenFileName):-
  atomic(FileName), !,
  atomic_concat('.', FileName, HiddenFileName).

%! merge_into_one_file(+FromDir:atom, +ToFile:atom) is det.
% RE and To must be in the same directory.
% How arbitrary this restriction is!

merge_into_one_file(FromDir, ToFile):-
  directory_files(FromDir, text, FromFiles),
  setup_call_cleanup(
    open(ToFile, write, Out, [type(binary)]),
    maplist(merge_into_one_file0(Out), FromFiles),
    close(Out)
  ),
  % DEB: Mention which files were merged.
  with_output_to(atom(Files), print_list([], FromFiles)),
  debug(file_ext, 'Files ~w were merged into ~w.', [Files,ToFile]).
merge_into_one_file0(Out, FromFile):-
  setup_call_cleanup(
    open(FromFile, read, In, [type(binary)]),
    copy_stream_data(In, Out),
    close(In)
  ).

%! new_file(+File1:atom, -File2:atom) is det.
% If a file with the same name exists in the same directory, then
% then distinguishing integer is appended to the file name.

new_file(File, File):-
  \+ exists_file(File), !.
new_file(File, NewFile):-
  file_name_extension(Base, Extension, File),
  dcg_phrase(dcg_separated_list(underscore, Codess), Base),
  reverse(Codess, [LastCodes | RestCodess]),
  (
    number_codes(OldNumber, LastCodes)
  ->
    NewNumber is OldNumber + 1,
    number_codes(NewLastCodes, NewNumber),
    reverse([NewLastCodes | RestCodess], NewCodess)
  ;
    reverse(["1", LastCodes | RestCodess], NewCodess)
  ),
  maplist(atom_codes, NewAtoms, NewCodess),
  atomic_list_concat(NewAtoms, '_', NewBase),
  file_name_extension(NewBase, Extension, NewFile).

safe_copy_file(From, To):-
  access_file(From, read),
  access_file(To, write), !,
  safe_delete_file(To),
  copy_file(From, To),
  debug(file_ext, 'File ~w was safe-copied to ~w.', [From,To]).

%! safe_delete_file(+File:atom) is det.
% Delete the given file, but keep a copy around in thake trashcan.

safe_delete_file(File):-
  \+ exists_file(File), !,
  debug(
    file_ext,
    'File ~w cannot be deleted since it does not exist.',
    [File]
  ).
safe_delete_file(File):-
  access_file(File, write), !,
  absolute_file_name(project(.), ProjectDir, [file_type(directory)]),
  relative_file_name(File, ProjectDir, RelativeFile),
  trashcan(Trashcan),
  directory_file_path(Trashcan, RelativeFile, CopyFile),
  % Copying to a nonexisting directory does not work, so first
  % we need to recursively create the directory.
  file_directory_name(CopyFile, CopyDirectory),
  create_directory(CopyDirectory),
  copy_file(File, CopyFile),
  catch(
    (
      delete_file(File),
      debug(file_ext, 'File ~w was safe-deleted.', [File])
    ),
    Exception,
    debug(file_ext, '~w', [Exception])
  ).

safe_move_file(From, To):-
  safe_copy_file(From, To),
  safe_delete_file(From),
  debug(file_ext, 'File ~w was safe-moved to ~w.', [From,To]).

safe_rename_file(From, To):-
  access_file(From, write),
  \+ exists_file(To), !,
  rename_file(From, To).
safe_rename_file(From, To):-
  access_file(From, write),
  exists_file(To), !,
  safe_delete_file(To),
  safe_rename_file(From, To),
  debug(file_ext, 'File ~w was safe-renamed to ~w.', [From,To]).

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
    ['--bytes=1m', '-d', '--suffix-length=4', BigFile, Prefix],
    [cwd(SmallDir)]
  ),
  debug(
    file_ext,
    'File ~w was split into smaller files in directory ~w.',
    [BigFile,SmallDir]
  ).

