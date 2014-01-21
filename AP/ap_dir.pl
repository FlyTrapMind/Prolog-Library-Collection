:- module(
  ap_dir,
  [
    ap_clean/1, % +Alias:atom
    ap_dir/4, % +Alias:atom
              % +Mode:oneof([read,write])
              % +Subdir:atom
              % -AbsoluteDir:atom
    ap_last_stage_directory/2, % +Alias:atom
                               % -LastStageDirectory:atom
    ap_stage_directories/2, % +Alias:atom
                            % -Directories:list(atom)
    ap_stage_name/2 % +Stage:nonneg
                    % -StageName:atom
  ]
).

/** <module> Auto-processing directories

Directory management for running automated processes.

@author Wouter Beek
@version 2013/11, 2014/01
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(os(dir_ext)).



%! ap_clean(+Alias:atom) is det.
% This is run after results have been saved to the `Output` directory.

ap_clean(Alias):-
  ap_stage_directories(Alias, StageDirs),
  maplist(safe_delete_directory_contents([]), StageDirs).


%! ap_dir(
%!   +Alias:atom,
%!   +Mode:oneof([read,write]),
%!   +Subdir:atom,
%!   -AbsoluteDir:atom
%! ) is det.

% The root is a special case.
% It has no subdirectory alias.
ap_dir(Alias, Mode, Subdir1, AbsoluteDir):-
  to_atom(Subdir1, Subdir2),
  Spec =.. [Alias,Subdir2],
  (
    absolute_file_name(
      Spec,
      AbsoluteDir,
      [access(Mode),file_errors(fail),file_type(directory)]
    ), !
  ;
    Mode = write,
    % If the AP subdirectory is not found and the mode is `write`,
    % then we create it.
    create_nested_directory(Spec, AbsoluteDir)
  ).


%! ap_last_stage_directory(+Alias:atom, -LastStageDirectory:atom) is semidet.
% Returns the last stage directory, if it exists.

ap_last_stage_directory(Alias, LastStageDir):-
  ap_stage_directories(Alias, StageDirs),
  StageDirs \== [],
  last(StageDirs, LastStageDir).


%! ap_stage_name(+StageIndicator, -StageName:atom) is det.
% Returns the stage name that corresponds to the given indicator.
%
% @arg StageIndicator One of the following:
%   * `0` is converted to name `input`.
%   * Positive integers `N` are converted to names `stage_N`.
%   * Other names are unchanged.
% @arg StageName An atomic name.

ap_stage_name(0, input):- !.
ap_stage_name(Stage, StageName):-
  must_be(positive_integer, Stage), !,
  format(atom(StageName), 'stage~w', [Stage]).
ap_stage_name(Stage, Stage).


%! ap_stage_directories(+Alias:atom, -StageDirectories:list(atom)) is det.

ap_stage_directories(Alias, Dirs):-
  ap_stage_directories(Alias, 1, Dirs).

ap_stage_directories(Alias, Stage1, [H|T]):-
  ap_stage_name(Stage1, Stage1Name),
  ap_dir(Alias, read, Stage1Name, H), !,
  Stage2 is Stage1 + 1,
  ap_stage_directories(Alias, Stage2, T).
ap_stage_directories(_, _, []).

