:- module(
  script_stage,
  [
    script_begin/0,
    script_end/0,
    script_stage/2 % +Script:nonneg
                   % :Goal
  ]
).

/** <module> SCRIPT_STAGE

Run scripts in stages.

We presuppose that the =data= directory has been set and is writeable.

@author Wouter Beek
@version 2013/06, 2013/10
*/

:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- meta_predicate(script_stage(+,2)).

:- debug(script_stage).



%! find_last_stage(-LastStageDirectory:atom) is det.

find_last_stage(LastStageDir):-
  find_stage_directories(StageDirs),
  last(StageDirs, LastStageDir).

%! find_stage_directories(-StageDirectories:list(atom)) is det.

find_stage_directories(StageDirs):-
  find_stage_directories(StageDirs, 1).

%! find_stage_directories(-StageDirectories:list(atom), +Stage:nonneg) is det.

find_stage_directories([H|T], Stage):-
  atomic_list_concat([stage,Stage], '_', StageName),
  absolute_file_name2(
    data(StageName),
    H,
    [access(write),file_type(directory)]
  ),
  NextStage is Stage + 1,
  find_stage_directories(T, NextStage).
find_stage_directories([], _Stage):- !.

%! init_data_directory is det.
% Makes sure there exists a `Data` subdirectory of the current project.

init_data_directory:-
  file_search_path(data, _DataDir), !.
init_data_directory:-
  create_project_subdirectory('Data', DataDir),
  db_add_novel(user:file_search_path(data, DataDir)).

script_begin:-
  script_clean.

%! script_clean is det.
% This is run after results have been saved to the `Output` directory.

script_clean:-
  find_stage_directories(StageDirs),
  maplist(safe_delete_directory_contents, StageDirs).

%! script_end is det.
% End the script, saving the results to the `Output` directory.

script_end:-
  find_last_stage(FromDir),
  create_nested_directory(data('Output'), ToDir),
  safe_copy_directory(FromDir, ToDir),
  script_clean.

%! script_stage(+Stage:nonneg, :Goal) is det.

script_stage(Stage, Goal):-
  init_data_directory,
  stage_directory(Stage, FromDir),
  NextStage is Stage + 1,
  stage_directory(NextStage, ToDir),
  call(Goal, FromDir, ToDir),
  debug(script_stage, 'Stage ~w is done.', [Stage]).

%! stage_directory(+Stage:nonneg, -StageDirectory:atom) is det.
% Creates a directory in `Data` for the given stage number
% and adds it to the file search path.

% In stage 0 we use stuff form the `Input` directory.
stage_directory(0, StageDir):- !,
  create_nested_directory(data('Input'), StageDir),
  db_add_novel(user:file_search_path(data_input, StageDir)).
% For stages `N >= 1` we use stuff from directories called `stage_N`.
stage_directory(Stage, StageDir):-
  atomic_list_concat([stage,Stage], '_', StageName),
  create_nested_directory(data(StageName), StageDir),
  db_add_novel(user:file_search_path(Stage, StageDir)).

