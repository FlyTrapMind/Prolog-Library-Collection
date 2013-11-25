:- module(
  ap_dir,
  [
% ALIAS
    ap_process_alias/2, % +Options:list(nvpair)
                        % -ProcessAlias:atom
    ap_stage_alias/3, % +Options:list(nvpair)
                      % +StageNumber:nonneg
                      % -StageAlias:atom
    ap_stage_name/2, % +StageNumber:nonneg
                     % -StageName:atom
% DIRECTORY: CREATE
    ap_clean/1, % +Options:list(nvpair)
    ap_create_input_directory/2, % +Options:list(nvpair)
                                 % -InputDirectory:atom
    ap_create_output_directory/2, % +Options:list(nvpair)
                                  % -OutputDirectory:atom
    ap_create_process_directory/2, % +Options:list(nvpair)
                                   % -ProcessDirectory:atom
    ap_create_stage_directory/3, % +Options:list(nvpair)
                                 % +StageNumber:nonneg
                                 % -StageDirectory:atom
% DIRECTORY: READ
    ap_input_directory/2, % +Options:list(nvpair)
                          % -InputDirectory:atom
    ap_output_directory/2, % +Options:list(nvpair)
                           % -OutputDirectory:atom
    ap_process_directory/2, % +Options:list(nvpair)
                            % -ProcessDirectory:atom
    ap_stage_directory/3, % +Options:list(nvpair)
                          % +StageNumber:nonneg
                          % -StageDirectory:atom
    ap_stage_directories/2, % +Options:list(nvpair)
                            % -StageDirectories:list(atom)
% LAST DIRECTORIES
    ap_last_stage_directories/2 % +Options:list(nvpair)
                                % -LastStageDirectories:list(atom)
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



% ALIAS %

ap_process_alias(O1, ProcessAlias):-
  option(project(Project), O1, project),
  option(ap(Process), O1, process),
  atomic_list_concat([Project,Process], '_', ProcessAlias).

ap_stage_alias(O1, StageNumber, StageAlias):-
  ap_process_alias(O1, ProcessAlias),
  ap_stage_name(StageNumber, StageName),
  atomic_list_concat([ProcessAlias,StageName], '_', StageAlias).

%! ap_stage_name(+StageNumber:nonneg, -StageName:atom) is det.
% Returns the stage name for the stage with the given number.
%
% This makes sure we name stages in a consistent way throguhout the codebase.

ap_stage_name(StageNumber, StageName):-
  format(atom(StageName), 'stage~w', [StageNumber]).



% DIRECTORY: CREATE %

%! ap_clean(+Options:list(nvpair)) is det.
% This is run after results have been saved to the `Output` directory.

ap_clean(O1):-
  ap_stage_directories(O1, StageDirs),
  maplist(safe_delete_directory_contents([]), StageDirs).

%! ap_create_directory(O1, DirName, Dir) is det.

ap_create_directory(O1, DirName, Dir):-
  ap_process_alias(O1, ProcessAlias),
  DirSpec =.. [ProcessAlias,DirName],
  create_nested_directory(DirSpec, Dir).

ap_create_input_directory(O1, InputDir):-
  ap_create_directory(O1, 'Input', InputDir).

ap_create_output_directory(O1, OutputDir):-
  ap_create_directory(O1, 'Output', OutputDir).

ap_create_process_directory(O1, ProcessDir):-
  ap_create_directory(O1, '.', ProcessDir),
  ap_process_alias(O1, ProcessAlias),
  db_add_novel(user:file_search_path(ProcessAlias, ProcessDir)).

ap_create_stage_directory(O1, StageNumber, StageDir):-
  ap_stage_name(StageNumber, StageName),
  ap_create_directory(O1, StageName, StageDir).



% DIRECTORIES: READ %

%! ap_directory(
%!   +Options:list(nvpair),
%!   +DirectoryName:atom,
%!   -Directory:atom
%! ) is det.
% Creates a new AP subdirectory.

ap_directory(O1, Name, Dir):-
  ap_process_alias(O1, ProcessAlias),
  DirSpec =.. [ProcessAlias,Name],
  absolute_file_name(
    DirSpec,
    Dir,
    [access(read),file_errors(fail),file_type(directory)]
  ).

ap_input_directory(O1, InputDir):-
  ap_directory(O1, input, InputDir).

ap_output_directory(O1, OutputDir):-
  ap_directory(O1, output, OutputDir).

ap_process_directory(O1, ProcessDir):-
  ap_directory(O1, '.', ProcessDir).

ap_stage_directory(O1, StageNumber, StageDir):-
  ap_stage_name(StageNumber, StageName),
  ap_directory(O1, StageName, StageDir).

%! ap_stage_directories(
%!   +Options:list(nvpair),
%!   -StageDirectories:list(atom)
%! ) is det.

ap_stage_directories(O1, StageDirs):-
  ap_stage_directories(O1, StageDirs, 1).

%! ap_stage_directories(
%!   +Options:list(nvpair),
%!   -StageDirectories:list(atom),
%!   +Stage:nonneg
%! ) is det.

ap_stage_directories(O1, [H|T], StageNumber):-
  ap_stage_directory(O1, StageNumber, H),
  NextStageNumber is StageNumber + 1,
  ap_stage_directories(T, NextStageNumber).
ap_stage_directories(_O1, [], _StageNumber):- !.



% LAST STAGE DIRECTORIES %

%! ap_last_stage_directories(
%!   +Options:list(nvpair),
%!   -LastStageDirectories:list(atom)
%! ) is det.
% Returns the last two stage directories,
% or returns less if there are less than 2 stage directories.

ap_last_stage_directories(O1, LSDirs):-
  ap_stage_directories(O1, SDirs),
  (
    (SDirs = [] ; SDirs = [_])
  ->
    LSDirs = SDirs
  ;
    reverse(SDirs, [Dir1,Dir2|_]),
    LSDirs = [Dir1,Dir2]
  ).

