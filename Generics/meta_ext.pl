:- module(
  meta_ext,
  [
% CACHING
    reset_memo/0,
    memo/1, % :Goal

% DEFAULTS
    default/3, % ?Value
               % +Default:term
               % +SetValue:term

% GENERIC CALLS
    generic/3, % :GenericPredicate
               % :Context
               % +Arguments:list

% FINDALL RELATED PREDICATES
    setoff/3, % +Format:compound
              % :Goal
              % -Set:ordset

% MAPLIST RELATED PREDICATES
    app_list/3, % +Preds:list
                % +Args:list
                % -Results:list
    maplist_pairs/3, % :Goal
                     % +List1:list
                     % -List2:list
    mapset/3, % :Goal
              % +List:list
              % -Set:ordset

% MODULES
    modules/1, % -Modules:list(atom)

    update_datastructure/4 % :Call
                           % +OldDatastructure
                           % +Arguments:list
                           % -NewDatastructurte
  ]
).

/** <module> Meta extensions

Extensions to the SWI-Prolog meta predicates.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03-2013/04, 2013/09-2013/10, 2013/12
*/

:- use_module(generics(error_ext)).
:- use_module(generics(list_ext)).
:- use_module(programming(prolog_control)).

:- meta_predicate(generic(:,:,+)).
:- meta_predicate(maplist_pairs(3,+,-)).
:- meta_predicate(mapset(2,+,-)).
:- meta_predicate(memo(0)).
:- meta_predicate(setoff(+,0,-)).
:- meta_predicate(setoff_alt(+,0,-)).
:- meta_predicate(update_datastructure(3,+,+,-)).

:- dynamic(memo_/1).
:- dynamic(tmp/1).



% CACHING %

%! memo(:Goal) is nondet.
% Memo goals that take relatively long to compute and that
% are likely to be recomputed in the future.
% This is achieved by storing the result along with the call,
% i.e. the fully instantiated goal.
% There are no restrictions on the determinism of the goal.

memo(Goal):-
  memo_(Goal), !.
memo(Goal):-
  call(Goal),
  assertz(memo_(Goal)).

reset_memo:-
  retractall(memo_(_)).



% DEFAULTS %

%! default(?Value, +Default:term, -SetValue:term) is det.
% Returns either the given value or the default value in case there is no
% value given.
%
% @arg Value A term or a variable.
% @arg Default A term.
% @arg SetValue A term.

default(Value, Default, Default):-
  var(Value), !.
default(Value, _Default, Value).



% GENERIC CALLS %

%! generic(:GenericPredicate, :Context, +Arguments:list)
% This uses the naming convention that similar predicates share
% the same prefix.
%
% @arg GenericPredicate The predicate prefix,
%        denoting the generic part of the operation.
% @arg Context The predicate suffix,
%        denoting the specific part of the operation.
% @arg Arguments An argitrary number of arguments.

generic(P1, Context, Args):-
  % Make sure the calling module prefix is discarded.
  strip_module(P1, M, P0),
  strip_module(Context, M, Context0),
  atomic_list_concat([P0, Context0], '_', P2),
  length(Args, Arity),
  if_then(
    current_predicate(M:P2/Arity),
    apply(M:P2, Args)
  ).



% FINDALL RELATED PREDICATES %

%! setoff(+Format, :Goal, -Set:ordset) is det.
% The sorted version of forall/2.
%
% @arg Format A compound term.
% @arg Goal A predicate name.
% @arg Set An ordered set.
% @see forall/2

setoff(Format, Goal, Set):-
  findall(Format, Goal, List),
  sort(List, Set).

% @tbd Run this with help_web/1!
setoff_alt(Format, Goal, _Set):-
  call(Goal),
  (tmp(Format) -> true ; assertz(tmp(Format))),
  fail.
setoff_alt(_Format, _Goal, Set):-
  findall(Format, tmp(Format), Set0),
  retractall(tmp(_)),
  sort(Set0, Set).



% MAPLIST RELATED PREDICATES %

%! app_list(+Preds:list, +Args:list, -Results:list) is det.
% Applies multiple predicates to a static list of arguments.
% Returns the results of applying the given predicates to the given argument
% list. The number of results is the number of predicates. The arguments are
% the same for every predicate call.

app_list([], _Args, []).
app_list([Module:Pred | Preds], Args, [Result | Results]):-
  append(Args, [Result], Args0),
  Call =.. [Pred | Args0],
  call(Module:Call),
  app_list(Preds, Args, Results).

%! maplist_pairs(:Goal, +List1:list, -List2:list) is det.
% Applies the given goal to all pairs of elements occuring in `List1`.

maplist_pairs(Goal, List1, List2):-
  findall(
    Result,
    (
      member(Element1, Element2, List1),
      call(Goal, Element1, Element2, Result)
    ),
    List2
  ).

%! mapset(:Goal, +List:list(term), -Set:ordset(term)) is det.
% The sorted version of maplist/3.
%
% @arg Goal A goal.
% @arg List A list of terms.
% @arg Set An ordered set of terms.

mapset(Goal, List, Set):-
  maplist(Goal, List, NewList),
  sort(NewList, Set).



% MODULES %

%! modules(-Modules:list(atom)) is det.
% Returns a list of the names of all the loaded modules.
%
% @arg Modules A list of atomic module names.

modules(Modules):-
  findall(
    Module,
    current_module(Module),
    Modules
  ).



%! update_datastructure(
%!   :Call,
%!   +OldDatastructure,
%!   +Arguments:list,
%!   -NewDatastructurte
%! ) is det.
% Prolog cannot do pass-by-reference.
% This means that when a datastructure has to be repeatedly updated,
% both its old and its new version have to be passed around in full.
% Examples of these are in the SWI-Prolog libraries for association lists
% and ordered sets.

update_datastructure(_Call, Datastructure, [], Datastructure).
update_datastructure(Call, Datastructure1, [H|T], Datastructure3):-
  call(Call, Datastructure1, H, Datastructure2),
  update_datastructure(Call, Datastructure2, T, Datastructure3).

