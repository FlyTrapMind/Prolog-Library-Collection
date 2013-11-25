:- module(
  ap_generic,
  [
    ap_clean/1, % +Options:list(nvpair)
    ap_debug/2, % +Options:list(nvpair)
                % +Message:atom
    ap_debug/3, % +Options:list(nvpair)
                % +Message:atom
                % +Arguments:list
    ap_stage_name/2 % +StageNumber:positive_integer
                    % -StageName:atom
  ]
).

/** <module> Auto-processes generics

Geneirc predicates that are used with auto-processes.

@author Wouter Beek
@version 2013/11
*/

:- use_module(autoproc(ap_dir)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(os(dir_ext)).



%! ap_clean(+Options:list(nvpair)) is det.
% This is run after results have been saved to the `Output` directory.

ap_clean(O1):-
  ap_find_stage_directories(O1, StageDirs),
  maplist(safe_delete_directory_contents([]), StageDirs).

%! ap_debug(+Options:list(nvpair), +Message:atom) is det.
% @see Wrapper to ap_debug/3

ap_debug(O1, Msg):-
  ap_debug(O1, Msg, []).

%! ap_debug(+Options:list(nvpair), +Format, :Arguments) is det.

ap_debug(O1, Msg1, Args):-
  date_time(Time),
  option(project(Project), O1, project),
  option(process(Process), O1, process),
  format(atom(Msg2), Msg1, Args),
  debug(
    script_ext,
    '[Time:~w][Project:~w][Process:~w] ~w.',
    [Time,Project,Process,Msg2]
  ).

%! ap_stage_name(+StageNumber:positive_integer, -StageName:atom) is det.
% Returns the stage name for the stage with the given number.
%
% This makes sure we name stages in a consistent way throguhout the codebase.

ap_stage_name(StageNumber, StageName):-
  format(atom(StageName), 'stage~w', [StageNumber]).

