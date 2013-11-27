:- module(
  ap_dir,
  [
    ap_clean/1, % +Options:list(nvpair)
    ap_dir/3, % +Options:list(nvpair)
              % +AliasSubdir:atom
              % -AbsoluteDir:atom
    ap_last_stage_directories/2, % +Options:list(nvpair)
                                 % -LastStageDirectories:list(atom)
    ap_stage_alias/3, % +Options:list(nvpair)
                      % +Stage:or([nonneg,oneof([input,output])])
                      % -StageAlias:atom
    ap_stage_name/2 % +Stage:nonneg
                    % -StageName:atom
  ]
).

/** <module> Auto-processing directories

Directory management for running automated processes.

@author Wouter Beek
@version 2013/11
*/

:- use_module(generics(typecheck)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(os(dir_ext)).



%! ap_clean(+Options:list(nvpair)) is det.
% This is run after results have been saved to the `Output` directory.

ap_clean(O1):-
  ap_stage_directories(O1, StageDirs),
  maplist(safe_delete_directory_contents([]), StageDirs).

%! ap_dir(+Options:list(nvpair), +AliasSubdir:atom, -AbsoluteDir:atom) is det.
% Aliases and subdirs are *|exactly the same|*.
% This makes the implementation easier.

ap_dir(O1, AliasSubdir1, AbsoluteDir):-
  ap_stage_name(AliasSubdir1, AliasSubdir2),
  ap_process_alias(O1, ProcessAlias),
  (
    % The root is a special case.
    % It has not alias for a subdirectory.
    AliasSubdir2 == '.'
  ->
    Alias = ProcessAlias
  ;
    % Non-root cases.
    atomic_list_concat([ProcessAlias,AliasSubdir2], '_', Alias)
  ),
  (
    file_search_path(Alias, _)
  ->
    Spec =.. [Alias,'.'],
    absolute_file_name(
      Spec,
      AbsoluteDir,
      [access(read),file_errors(error),file_type(directory)]
    )
  ;
    (
      % The root is a special case.
      % It is a subdirectory of the project directory.
      AliasSubdir2 == '.'
    ->
      option(project(Project), O1, project),
      option(process(Process), O1, process),
      Spec_ =.. [Project,ap],
      create_nested_directory(Spec_, AP_Dir),
      atomic_list_concat([Project,ap], '_', AP_Alias),
      db_add_novel(user:file_search_path(AP_Alias, AP_Dir)),
      Spec =.. [AP_Alias,Process]
    ;
      % Non-root cases.
      Spec =.. [ProcessAlias,AliasSubdir2]
    ),
    create_nested_directory(Spec, AbsoluteDir),
    db_add_novel(user:file_search_path(Alias, AbsoluteDir))
  ).

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

ap_process_alias(O1, ProcessAlias):-
  option(project(Project), O1, project),
  option(process(Process), O1, process),
  atomic_list_concat([Project,ap,Process], '_', ProcessAlias).

%! ap_stage_alias(
%!   +Options:list(nvpair),
%!   +Stage:or([nonneg,oneof([input,output])]),
%!   -StageAlias:atom
%! ) is det.

ap_stage_alias(O1, Stage, StageAlias):-
  ap_process_alias(O1, ProcessAlias),
  ap_stage_name(Stage, StageName),
  atomic_list_concat([ProcessAlias,StageName], '_', StageAlias).

ap_stage_name(0, input):- !.
ap_stage_name(Stage, StageName):-
  positive_integer(Stage), !,
  format(atom(StageName), 'stage~w', [Stage]).
ap_stage_name(Stage, Stage).

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
  ap_dir(O1, StageNumber, H),
  NextStageNumber is StageNumber + 1,
  ap_stage_directories(T, NextStageNumber).
ap_stage_directories(_O1, [], _StageNumber):- !.

