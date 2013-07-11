:- module(
  option_ext,
  [
    option_dep/2, % +AnyOption
                  % -NondepOption
    option_ext/3, % ?Option
                  % +Options:list
                  % +Default
    subtract_option/3 % +OldOptions:list(nvpair)
                      % +Delete:list(nvpair)
                      % -NewOptions:list(nvpair)
  ]
).

/** <module> Option list handling extension

Extensions to the swipl buitin handling of option lists.

This module allows the use of default option values in option/3 that have
arbitrary arity. The swipl builtin only handles default values for the
first argument position in the given option term (probably under the
assumption that the option term will always be unary).

@author Wouter Beek
@version 2013/01, 2013/07
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).



%! option_dep(+AnyOption, -NondepOption) is det.
% Ensures that an option uses the non-depracated format:
% ~~~
% Name(Value)
% ~~~
%
% Also allows the deprecated format as input:
% ~~~
% Name=Value
% ~~~

option_dep(Name=Value, Name=Value):- !.
option_dep(Option, Name=Value):-
  Option =.. [Name, Value].

option_ext(Option, Options, Default):-
  functor(Option, Name, Arity),
  functor(MatchOption, Name, Arity),
  (
    % Case 1: The option can be matched in the options list.
    % For this case we use the swipl builtin get_option/3.
    swi_option:get_option(MatchOption, Options)
  ->
    Option = MatchOption
  ;
    % Case 2: The option cannot be matched in the options list,
    % but the default option has the same functor and arity.
    % This is the extension to the swipl builin predicate, allowing
    % the formulation of default values of arbitrary arity.
    functor(Default, Name, Arity)
  ->
    Option = Default
  ;
    % Case 3: The option cannot be matched in the options list and the
    % defualt value is assumed to unify with the first argument position
    % of the given option term.
    arg(1, Option, Default)
  ).

subtract_option(Old1, Del1, New):-
  maplist(option_dep, Old1, Old2),
  maplist(option_dep, Del1, Del2),
  subtract(Old2, Del2, New).
