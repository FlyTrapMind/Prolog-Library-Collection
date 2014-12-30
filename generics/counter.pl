:- module(
  counter,
  [
    decrement_counter/1, % +Term
    enumerate_counter/2, % -Term
                         % -Count:integer
    get_count/2, % +Term
                 % ?Count:integer
    increment_counter/1, % +Term
    increment_counter/2 % +Term
                        % +Increment:integer
  ]
).

/** <module> Counter

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(error)).

:- dynamic(counter0/2).





%! decrement_counter(+Term) is det.
% @throws instantiation_error if Term is uninstantiated.

decrement_counter(Term):-
  increment_counter(Term, -1).



%! enumerate_counter(-Term, -Count:integer) is nondet.

enumerate_counter(Term, Count):-
  counter0(Term, Count).



%! get_count(+Term, +Count:integer) is semidet.
%! get_count(+Term, -Count:integer) is det.
% @throws instantiation_error if Term is uninstantiated.

get_count(Term, _):-
  var(Term), !,
  instantiation_error(Term).
get_count(Term, Count):-
  counter0(Term, Count), !.
get_count(_, 0).



%! increment_counter(+Term) is det.
% @throws instantiation_error if Term is uninstantiated.

increment_counter(Term):-
  increment_counter(Term, 1).



%! increment_counter(+Term, +Increment:integer) is det.
% @throws instantiation_error if Term is uninstantiated.

increment_counter(Term, _):-
  var(Term), !,
  instantiation_error(Term).
increment_counter(Term, Increment):-
  retract(counter0(Term, Count0)), !,
  Count is Count0 + Increment,
  assert(counter0(Term, Count)).
increment_counter(Term, Increment):-
  assert(counter0(Term, Increment)).
