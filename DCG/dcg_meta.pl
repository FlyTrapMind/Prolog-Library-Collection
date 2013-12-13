:- module(
  dcg_meta,
  [
    ';'//2, % :DCG_Body1
            % :DCG_Body2
    ';'//3, % :DCG_Body1
            % :DCG_Body2
            % :DCG_Body3
    dcg_apply//2, % :DCG_Body
                  % +Arguments:list
    dcg_between//2, % :Between
                    % :DCG
    dcg_between//3, % :Begin
                    % :DCG
                    % :End
    dcg_call//1,
    dcg_call//2,
    dcg_call//3,
    dcg_call//4,
    dcg_call//5,
    dcg_calls//2 % :DCG_Rules:list
                 % :Separator
  ]
).

/** <module> DCG meta

Meta-DCG rules.

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- meta_predicate(';'(2,2,?,?)).
:- meta_predicate(';'(2,2,2,?,?)).
:- meta_predicate(dcg_apply(//,+,?,?)).
:- meta_predicate(dcg_between(//,//,?,?)).
:- meta_predicate(dcg_between(//,//,//,?,?)).
:- meta_predicate(dcg_call(2,?,?)).
:- meta_predicate(dcg_call(3,?,?,?)).
:- meta_predicate(dcg_call(4,?,?,?,?)).
:- meta_predicate(dcg_call(5,?,?,?,?,?)).
:- meta_predicate(dcg_call(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_calls(+,//,?,?)).

';'(A, _B, C, D):-
  dcg_call(A, C, D).
';'(_A, B, C, D):-
  dcg_call(B, C, D).

';'(A, _B, _C, D, E):-
  dcg_call(A, D, E).
';'(_A, B, _C, D, E):-
  dcg_call(B, D, E).
';'(_A, _B, C, D, E):-
  dcg_call(C, D, E).

dcg_apply(DCG_Body, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(DCG_Body, Args2).

dcg_between(Between, DCG) -->
  dcg_between(Between, DCG, Between).

dcg_between(Begin, DCG, End) -->
  phrase(Begin),
  phrase(DCG),
  phrase(End).

%! dcg_call(:DCG)//
% Included for consistency with dcg_call//[1,2,3,4].
% @see Same effect as phrase/3.

dcg_call(DCG_Body, X, Y):-
  call(DCG_Body, X, Y).

dcg_call(DCG_Body, A1, X, Y):-
  call(DCG_Body, A1, X, Y).

dcg_call(DCG_Body, A1, A2, X, Y):-
  call(DCG_Body, A1, A2, X, Y).

dcg_call(DCG_Body, A1, A2, A3, X, Y):-
  call(DCG_Body, A1, A2, A3, X, Y).

dcg_call(DCG_Body, A1, A2, A3, A4, X, Y):-
  call(DCG_Body, A1, A2, A3, A4, X, Y).
