:- module(
  script_stage,
  [
    script_stage/2 % +Script:integer
                   % :Goal
  ]
).

/** <module> SCRIPT_STAGE

Run scripts in stages.

We presuppose that the =data= directory has been set and is writeable.

@author Wouter Beek
@version 2013/06
*/

:- use_module(generics(cowspeak)).
:- use_module(generics(db_ext)).
:- use_module(os(dir_ext)).

:- meta_predicate(script_stage(+,2)).



init:-
  file_search_path(data, _), !.
init:-
  create_personal_subdirectory('Data', Absolute),
  db_add_novel(user:file_search_path(data, Absolute)).

script_stage(Stage, Goal):-
  init,
  stage_directory(Stage, From),
  NextStage is Stage + 1,
  stage_directory(NextStage, To),
  call(Goal, From, To),
  cowspeak('Stage ~w is done.'-[Stage]).

stage_directory(0, StageDir):- !,
  absolute_file_name(
    data('Input'),
    StageDir,
    [access(write), file_type(directory)]
  ).
stage_directory(Stage, StageDir):-
  format(atom(StageName), 'stage_~w', [Stage]),
  absolute_file_name(data(StageName), StageDir),
  create_directory(StageDir).

