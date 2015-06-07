:- module(
  positional,
  [
    positional/2, % ?Number:nonneg
                  % ?DecimalDigits:list(between(0,9))
    positional/3 % ?Number:nonneg
                 % ?Base:nonneg
                 % ?DecimalDigits:list(between(0,9))
  ]
).

/** <module> Positional notation

Support for positional number notation.

@author Wouter Beek
@version 2015/06
*/

:- use_module(library(clpfd)).
:- use_module(library(error)).





%% positional(+N:nonneg, +Ds:list(between(0,9))) is semidet.
%% positional(+N:nonneg, -Ds:list(between(0,9))) is det.
%% positional(-N:nonneg, +Ds:list(between(0,9))) is det.

positional(N, Ds):-
  positional(N, 10, Ds).

%! positional(+N:nonneg, +Base:nonneg, +Ds:list(between(0,9))) is semidet.
%! positional(+N:nonneg, +Base:nonneg, -Ds:list(between(0,9))) is multi.
%! positional(-N:nonneg, +Base:nonneg, +Ds:list(between(0,9))) is multi.
% @see http://stackoverflow.com/questions/4192063/reversible-binary-to-number-predicate/28442760#28442760

positional(N, Base, Ds):-
  (nonvar(N), nonvar(Base) ; nonvar(Ds)), !,
  positional(Ds, Base, 0, N, N).
positional(_, _, _):-
  instantiation_error(_).

positional([], _, N, N, _).
positional([D|Ds], Base, N0, N, M):-
  in_base(D, Base),
  N1 #= D + Base * N0,
  M #>= N1,
  positional(Ds, Base, N1, N, M).

in_base(X, Base):- succ(Max, Base), X in 0..Max.

:- begin_tests(positional).

test('positional(+,+,+)', [forall(positional_test(N, Base, L))]):-
  positional(N, Base, L).
test('positional(+,+,-)', [forall(positional_test(N, Base, L)),nondet]):-
  positional(N, Base, L0), L0 = L.
test('positional(-,+,+)', [forall(positional_test(N, Base, L)),nondet]):-
  positional(N0, Base, L), N0 = N.

positional_test(1226, 10, [1,2,2,6]).
positional_test(120, 60, [2,0]).

:- end_tests(positional).
