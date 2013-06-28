:- module(
  os_ext,
  [
    is_mac/0,
    is_unix/0,
    is_windows/0,
    os_dependent_call/1, % :Goal
    mac_y_pixel/2, % +YPixel:number
                   % -MacYPixel:number
    set_os_flag/0
  ]
).

/** <module> OS_EXT



@author Wouter Beek
@version 2013/06
*/

:- use_module(library(debug)).

:- meta_predicate(os_dependent_call(:)).

:- debug(os_ext).



%! is_mac is semidet.
% Succeeds if the running OS is Mac OS-X.
%
% This presupposes that set_os_flag/0 has been run, in order to create the
% Prolog flag that is accessed here.

is_mac:-
  current_prolog_flag(mac, true).

%! is_unix is semidet.
% Succeeds if the running OS is Unix.

is_unix:-
  current_prolog_flag(unix, true).

%! is_windows is semidet.
% Succeeds if the running OS is Windows.

is_windows:-
  current_prolog_flag(windows, true).

%! mac_y_pixel(+YPixel:integer, -MacYPixel:integer) is det.
% Returns the Mac OS-X equivalent of the y-position of an onscreen pixel.
%
% Mac OS-X uses 21 pixels for the title bar.
%
% @arg YPixel The normal y-position of a pixel.
% @arg MacYPixel The y-position of a pixel adapted for display
%        on a Mac OS-X system.

mac_y_pixel(YPixel, MacYPixel):-
  MacYPixel is YPixel + 21.

%! os_dependent_call(:Goal)
% Allows goals to be carried out without the caller having to paying
% attention to OS-specific considerations.
%
% The convention is that one can check for an OS by calling =is_OS=.
%
% The convention is that a predicate =pred= has variants =pred_OS= for
% every supported OS.
%
% The supported operating systems are registered with supported_os/1.

os_dependent_call(Goal):-
  supported_os(OS),
  format(atom(Check), 'is_~w', [OS]),
  call(Check),
  !,
  strip_module(Goal, _Module, Call),
  Call =.. [Pred | Args],
  format(atom(Pred0), '~w_~w', [Pred, OS]),
  Call0 =.. [Pred0 | Args],
  call(Call0).
os_dependent_call(Goal):-
  debug(os_ext, 'The call ~w is not supported on your OS.', [Goal]).

%! set_os_flag is det.
% Distinguish between Unix and MacOSX.
%
% @author Inspired by Jochem Liem, who set this flag for the first time
%         in the context of the DynaLearn project.
% @see Since this is no [[current_prolog_flag/2]] for Mac OS-X yet.

set_os_flag:-
  is_mac,
  !,
  create_prolog_flag(mac, true, [access(read_only), type(boolean)]).
set_os_flag.

%! supported_os(?OS_Name:atom) is nondet.
% The names of supported OS-es.
% The name for Mac-OS is made up since it is not supported by SWI-Prolog.

supported_os(mac).
supported_os(unix).
supported_os(windows).

