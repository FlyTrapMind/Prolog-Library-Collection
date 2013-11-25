:- module(
  ap_dir,
  [
    ap_create_directory/3, % +Options:list(nvpair)
                           % +DirectoryName:atom
                           % -Directory:atom
    ap_create_stage_directory/3, % +Options:list(nvpair)
                                 % +StageNumber:positive_integer
                                 % -StageDirectory:atom
    ap_find_last_stage_directories/2, % +Options:list(nvpair)
                                      % -LastStageDirectories:list(atom)
    ap_find_stage_directories/2, % +Options:list(nvpair)
                                 % -StageDirectories:list(atom)
    ap_directory/3, % +Options:list(nvpair)
                    % +DirectoryName:atom
                    % -Directory:atom
    ap_input_directory/2, % +Options:list(nvpair)
                          % -InputDirectory:atom
    ap_output_directory/2, % +Options:list(nvpair)
                           % -OutputDirectory:atom
    ap_stage_directory/3 % +Options:list(nvpair)
                         % +StageNumber:positive_integer
                         % -StageDirectory:atom
  ]
).

/** <module> Auto-processing directories

Directory management for running automated processes.

@author Wouter Beek
@version 2013/11
*/

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(os(dir_ext)).



%! ap_create_directory(O1, DirName, Dir) is det.

ap_create_directory(O1, DirName, Dir):-
  option(project(Project), O1, project),
  option(ap(Process), O1, process),
  atomic_list_concat([Project,Process], '_', ProcessDirName),
  DirSpec =.. [ProcessDirName,DirName],
  create_nested_directory(DirSpec, Dir).

ap_create_stage_directory(O1, StageNumber, StageDir):-
  ap_stage_name(StageNumber, StageName),
  ap_create_directory(P1, StageName, StageDir).

%! ap_find_last_stage_directories(
%!   +Options:list(nvpair),
%!   -LastStageDirectories:list(atom)
%! ) is det.
% Returns the last two stage directories,
% or returns less if there are less than 2 stage directories.

ap_find_last_stage_directories(O1, LSDirs):-
  ap_find_stage_directories(O1, SDirs),
  (
    (SDirs = [] ; SDirs = [_])
  ->
    LSDirs = SDirs
  ;
    reverse(SDirs, [Dir1,Dir2|_]),
    LSDirs = [Dir1,Dir2]
  ).

%! ap_find_stage_directories(
%!   +Options:list(nvpair),
%!   -StageDirectories:list(atom)
%! ) is det.

ap_find_stage_directories(O1, StageDirs):-
  ap_find_stage_directories(O1, StageDirs, 1).

%! ap_find_stage_directories(
%!   +Options:list(nvpair),
%!   -StageDirectories:list(atom),
%!   +Stage:nonneg
%! ) is det.

ap_find_stage_directories(O1, [H|T], StageNumber):-
  ap_stage_directory(O1, StageNumber, H),
  NextStageNumber is StageNumber + 1,
  ap_find_stage_directories(T, NextStageNumber).
ap_find_stage_directories(_O1, [], _StageNumber):- !.

%! ap_directory(
%!   +Options:list(nvpair),
%!   +DirectoryName:atom,
%!   -Directory:atom
%! ) is det.
% Creates a new AP subdirectory.

ap_directory(O1, Name, Dir):-
  option(project(Project), O1, project),
  option(ap(Process), O1, process),
  atomic_list_concat([Project,Process,Name], '_', DirName),
  absolute_file_name(
    DirName,
    Dir,
    [access(read),file_errors(fail),file_type(directory)]
  ).

ap_input_directory(O1, InputDir):-
  ap_directory(O1, input, InputDir).

ap_output_directory(O1, OutputDir):-
  ap_directory(O1, output, OutputDir).

ap_stage_directory(O1, StageNumber, StageDir):-
  ap_stage_name(StageNumber, StageName),
  ap_directory(O1, StageName, StageDir).

