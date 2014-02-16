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
    dcg_call//1,
    dcg_call//2,
    dcg_call//3,
    dcg_call//4,
    dcg_call//5,
    dcg_calls//2, % :DCG_Rules:list
                  % :Separator
    dcg_maplist//2, % :DCG
                    % +Args:list
    dcg_maplist//3 % :DCG
                   % +Args1:list
                   % +Args2:list
  ]
).

/** <module> DCG meta

Meta-DCG rules.

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/09, 2013/11-2013/12, 2014/02
*/



:- meta_predicate(';'(2,2,?,?)).
';'(A, _B, C, D):-
  dcg_call(A, C, D).
';'(_A, B, C, D):-
  dcg_call(B, C, D).


:- meta_predicate(';'(2,2,2,?,?)).
';'(A, _B, _C, D, E):-
  dcg_call(A, D, E).
';'(_A, B, _C, D, E):-
  dcg_call(B, D, E).
';'(_A, _B, C, D, E):-
  dcg_call(C, D, E).


:- meta_predicate(dcg_apply(//,+,?,?)).
dcg_apply(DCG_Body, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(DCG_Body, Args2).


%! dcg_call(:DCG)//
% Included for consistency with dcg_call//[1,2,3,4].
% @see Same effect as phrase/3.

:- meta_predicate(dcg_call(2,?,?)).
dcg_call(DCG_Body, X, Y):-
  call(DCG_Body, X, Y).


:- meta_predicate(dcg_call(3,?,?,?)).
dcg_call(DCG_Body, A1, X, Y):-
  call(DCG_Body, A1, X, Y).


:- meta_predicate(dcg_call(4,?,?,?,?)).
dcg_call(DCG_Body, A1, A2, X, Y):-
  call(DCG_Body, A1, A2, X, Y).


:- meta_predicate(dcg_call(5,?,?,?,?,?)).
dcg_call(DCG_Body, A1, A2, A3, X, Y):-
  call(DCG_Body, A1, A2, A3, X, Y).


:- meta_predicate(dcg_call(6,?,?,?,?,?,?)).
dcg_call(DCG_Body, A1, A2, A3, A4, X, Y):-
  call(DCG_Body, A1, A2, A3, A4, X, Y).


:- meta_predicate(dcg_calls(+,//,?,?)).
dcg_calls(_Mod:[], _Separator) --> [].
dcg_calls(Mod:DCG_Rules, Separator) -->
  {DCG_Rules = [H|T]},
  dcg_call(Mod:H),
  (
    {T == []}, !
  ;
    dcg_call(Separator)
  ),
  dcg_calls(Mod:T, Separator).


%! dcg_maplist(:DCG, +Args:list)// .

:- meta_predicate(dcg_maplist(3,+,?,?)).
dcg_maplist(_, []) --> [].
dcg_maplist(DCG, [H|T]) -->
  dcg_apply(DCG, H),
  dcg_maplist(DCG, T).


%! dcg_maplist(:DCG, +Args1:list, +Args2:list)// .

:- meta_predicate(dcg_maplist(4,+,+,?,?)).
dcg_maplist(_, [], []) --> [].
dcg_maplist(DCG, [H1|T1], [H2|T2]) -->
  dcg_call(DCG, H1, H2),
  dcg_maplist(DCG, T1, T2).

