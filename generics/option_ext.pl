:- module(
  option_ext,
  [
    add_option/4, % +FromOptions:list(nvpair)
                  % +Name:atom
                  % +Value:atom
                  % +ToOptions:list(nvpair)
    add_default_option/4, % +Os1:list(nvpair)
                          % +Name:atom
                          % +DefaultValue
                          % -Os2:list(nvpair)
    add_default_option/5, % +Os1:list(nvpair)
                          % +Name:atom
                          % +DefaultValue
                          % -StoredValue
                          % -Os2:list(nvpair)
    if_option/3, % +Option:nvpair
                 % +Options:list(nvpair)
                 % :Goal
    if_select_option/4, % +Option:nvpair
                        % +Options:list(nvpair)
                        % -RestOptions:list(nvpair)
                        % :Goal
    merge_options/2, % +FromOptions:list(nvpair)
                     % -ToOptions:list(nvpair)
    nvpair/3, % ?NameValuePair:compound
              % ?Name:atom
              % ?Value
    remove_option/4, % +OldOptions:list(nvpair)
                     % +Name:atom
                     % ?Value
                     % -NewOptions:list(nvpair)
    replace_option/5, % +OldOptions:list(nvpair)
                      % +Name:atom
                      % +NewValue
                      % -OldValue
                      % -NewOptions:list(nvpair)
    update_option/4, % +OldOptions:list(nvpair)
                     % +Name:atom
                     % :Predicate
                     % -NewOptions:list(nvpair)
    update_option/5 % +OldOptions:list(nvpair)
                    % +Name:atom
                    % :Predicate
                    % -OldValue
                    % -OldOptions:list(nvpair)
  ]
).

/** <module> Option list handling extension

Extensions to the swipl buitin handling of option lists.

This module allows the use of default option values in option/3 that have
arbitrary arity. The swipl builtin only handles default values for the
first argument position in the given option term (probably under the
assumption that the option term will always be unary).

@author Wouter Beek
@version 2013/01, 2013/07-2013/08, 2013/11-2013/12, 2014/04, 2014/06-2014/07
*/

:- use_module(library(option)).

:- meta_predicate(if_option(+,+,0)).
:- meta_predicate(if_select_option(+,+,-,0)).
:- meta_predicate(update_option(+,+,2,-)).
:- meta_predicate(update_option(+,+,2,-,-)).



%! add_option(
%!   +FromOptions:list(nvpair),
%!   +Name:atom,
%!   ?Value:atom,
%!   +ToOptions:list(nvpair)
%! ) is det.
% Adds an option with the given name and value (i.e. `Name(Value)`),
%  and ensures that old options are overwritten and
%  that the resultant options list is sorted.
%
% @arg Options1
% @arg Name
% @arg Value If `Value` is not instantiated, `Options1 = Options2`.
% @arg Options2

add_option(O1, _, X, O1):-
  var(X), !.
add_option(O1, N, V, O2):-
  nvpair(O, N, V),
  merge_options([O], O1, O2).


%! add_default_option(
%!   +Os1:list(nvpair),
%!   +Name:atom,
%!   +DefaultValue,
%!   -Os2:list(nvpair)
%! ) is det.
% @see add_default_option/5

add_default_option(Os1, N, DefaultV, Os2):-
  add_default_option(Os1, N, DefaultV, _StoredV, Os2).


%! add_default_option(
%!   +Os1:list(nvpair),
%!   +Name:atom,
%!   +DefaultValue,
%!   -StoredValue,
%!   -Os2:list(nvpair)
%! ) is det.
% Gives either the stored value, if it is available,
%   or the given default value.
% Also returns the resultant options list.

add_default_option(Os1, N, _DefaultV, StoredV, Os1):-
  nvpair(O, N, StoredV),
  option(O, Os1), !.
add_default_option(Os1, N, DefaultV, DefaultV, Os2):-
  add_option(Os1, N, DefaultV, Os2).


%! if_option(+Option:nvpair, +Options:list(nvpair), :Goal) is det.

if_option(Option, Options, Goal):-
  option(Option, Options), !,
  Goal.
if_option(_, _, _).


%! if_select_option(
%!   +Option:nvpair,
%!   +Options:list(nvpair),
%!   -RestOptions:list(nvpair),
%!   :Goal
%! ) is det.

if_select_option(Option, Options1, Options2, Goal):-
  select_option(Option, Options1, Options2), !,
  Goal.
if_select_option(_, Options, Options, _).


%! merge_options(+FromOptions:list(nvpair), -ToOptions:list(nvpair)) is det.

merge_options([], []).
% Skip uninstantiated values.
merge_options([H|T1], T2):-
  merge_options(T1, T2),
  nvpair(H, _, Value),
  var(Value), !.
% Include instantiated values.
merge_options([H|T1], [H|T2]):-
  merge_options(T1, T2).


%! nvpair(+NameValuePair:compound, +Name:atom, +Value) is semidet.
%! nvpair(+NameValuePair:compound, -Name:atom, -Value) is det.
%! nvpair(-NameValuePair:compound, +Name:atom, +Value) is multi.

nvpair(Name=Value, Name, Value).
nvpair(NVPair, Name, Value):-
  NVPair =.. [Name,Value].


%! remove_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   ?Value,
%!   -NewOptions:list(nvpair)
%! ) is det.
% Removes at most one option (i.e., if at least one appears)
% with the given name and value from the options list.

remove_option(Os1, N, V, Os2):-
  nvpair(O, N, V),
  select_option(O, Os1, Os2), !.
remove_option(Os, _, _, Os).

%! replace_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   +NewValue,
%!   -OldValue,
%!   -NewOptions:list(nvpair)
%! ) is det.

replace_option(Os1, N, V2, V1, Os3):-
  remove_option(Os1, N, V1, Os2),
  add_option(Os2, N, V2, Os3).


%! update_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   :Predicate,
%!   -NewOptions:list(nvpair)
%! ) is det.
% @see Wrapper around update_option/5, not returning the old value.

update_option(Os1, N, Predicate, Os2):-
  update_option(Os1, N, Predicate, _OldV, Os2).

%! update_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   :Predicate,
%!   -OldValue,
%!   -NewOptions:list(nvpair)
%! ) is det.

update_option(Os1, N, Predicate, V1, Os3):-
  remove_option(Os1, N, V1, Os2),
  call(Predicate, V1, V2),
  add_option(Os2, N, V2, Os3).

