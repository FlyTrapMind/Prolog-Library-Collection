:- module(
  filepath_ext,
  [
    absolute_file_name_number/4, % +Spec
                                 % +Options:list(nvpair)
                                 % +Number:integer
                                 % -Absolute:atom
    path_walk_tree/3 % +RootDir:file
                     % +FileExtension:regex
                     % -Paths:list(file)
  ]
).

/** <module> FILEPATH_EXT

Filepath extensions.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(lists)).
:- use_module(library(pce)).
:- use_module(os(file_ext)).



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

%! path_walk_forest(
%!   +Dirs:list(atom),
%!   +RE:atom,
%!   -AbsoluteFileNames:list(atom)
%! ) is det.
% Returns the absolute paths of all files that are in the given directories.
% Search recurses through subdirectories.
%
% @arg Dirs A list of directory names.
% @arg RE A regular expression filter on file search.
% @arg AbsoluteFileNames A list of absolute file names.

path_walk_forest([], _RE, []).
path_walk_forest([Dir | Dirs], RE, AbsoluteFileNames):-
  path_walk_tree(Dir, RE, AbsoluteFileNames1),
  path_walk_forest(Dirs, RE, AbsoluteFileNames2),
  append(AbsoluteFileNames1, AbsoluteFileNames2, AbsoluteFileNames).

%! path_walk_tree(+MainDir:atom, +RE:atom, -Paths:list(atom)) is det.
% Returns the file paths in the given directory answering the given regular
% expression. The directories are searched recursively.
%
% @arg MainDir A directory path.
% @arg RE A regular expression filter. Example: =|'.*.pl$'|=
% @arg Paths A list of absolute file paths.

path_walk_tree(MainDir, RE, AbsoluteFileNames1):-
  % Find all relative file names.
  new(RelativeFileNamesChain, chain),
  new(RelativeSubDirsChain, chain),
  send(directory(MainDir), scan,
    files := RelativeFileNamesChain,
    directories := RelativeSubDirsChain,
    hidden_too := @off,
    pattern := regex(RE)
  ),

  % Turn all relative file names into absolute file names.
  chain_list(RelativeFileNamesChain, RelativeFileNames),
  findall(
    AbsoluteFileName,
    (
      member(RelativeFileName, RelativeFileNames),
      absolute_file_name(
        RelativeFileName,
        AbsoluteFileName,
        [relative_to(MainDir)]
      )
    ),
    AbsoluteFileNames
  ),

  % Turn all relative subdirectories into absolute subdirectories.
  chain_list(RelativeSubDirsChain, RelativeSubDirs),
  findall(
    AbsoluteSubDir,
    (
      member(RelativeSubDir, RelativeSubDirs),
      format(atom(AbsoluteSubDir), '~w/~w', [MainDir, RelativeSubDir])
    ),
    AbsoluteSubDirs
  ),

  % Traverse the subdirectories recursively.
  path_walk_forest(AbsoluteSubDirs, RE, RecursiveAbsoluteFileNames),

  % Combine the results.
  append(AbsoluteFileNames, RecursiveAbsoluteFileNames, AbsoluteFileNames1).

