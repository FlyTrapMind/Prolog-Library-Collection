:- module(
  counter,
  [
    counter/2, % @Term
               % ?Count:integer
    decrement_counter/1, % @Term
    increment_counter/1, % @Term
    increment_counter/2 % @Term
                        % +Increment:integer
  ]
).

/** <module> Counter

@author Wouter Beek
@version 2014/12
*/

:- dynamic(counter0/2).





%! counter(@Term, +Count:integer) is semidet.
%! counter(@Term, -Count:integer) is det.

counter(Term, Count):-
  counter0(Term, Count), !.
counter(_, 0).



%! decrement_counter(@Term) is det.

decrement_counter(Term):-
  increment_counter(Term, -1).



%! increment_counter(@Term) is det.

increment_counter(Term):-
  increment_counter(Term, 1).



%! increment_counter(@Term, +Increment:integer) is det.

increment_counter(Term, Increment):-
  retract(counter0(Term, Count0)), !,
  Count is Count0 + Increment,
  assert(counter0(Term, Count)).
increment_counter(Term, Increment):-
  assert(counter0(Term, Increment)).
