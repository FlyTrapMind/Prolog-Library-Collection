:- module(
  dcg_meta,
  [
    dcg_apply//2, % :Dcg
                  % +Arguments:list
    dcg_apply_cp//2, % :Dcg
                     % +Arguments:list
    dcg_atom_codes//2, % :Dcg
                       % ?Atom:atom
    dcg_call//1, dcg_call//2, dcg_call//3, dcg_call//4, dcg_call//5, dcg_call//6,
    dcg_call_cp//1, dcg_call_cp//2, dcg_call_cp//3, dcg_call_cp//4, dcg_call_cp//5, dcg_call_cp//6,
    dcg_maplist//2, dcg_maplist//3, dcg_maplist//4, dcg_maplist//5, dcg_maplist//6,
    dcg_once//1, % :Dcg
    dcg_repeat//0,
    dcg_sequence//2 % :Dcgs:list
                    % :Separator
  ]
).

/** <module> DCG: Meta-rules

Meta-DCG rules.

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/09, 2013/11-2013/12, 2014/02-2014/03, 2014/05,
         2014/08-2014/09
*/

:- use_module(library(lists), except([delete/3])).

:- use_module(generics(list_ext)).

:- use_module(plDcg(dcg_generics)).

:- meta_predicate(dcg_apply(//,+,?,?)).
:- meta_predicate(dcg_apply_cp(//,+,?,?)).
:- meta_predicate(dcg_atom_codes(3,?,?,?)).
:- meta_predicate(dcg_call(//,?,?)).
:- meta_predicate(dcg_call(3,?,?,?)).
:- meta_predicate(dcg_call(4,?,?,?,?)).
:- meta_predicate(dcg_call(5,?,?,?,?,?)).
:- meta_predicate(dcg_call(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_call(7,?,?,?,?,?,?,?)).
:- meta_predicate(dcg_call_cp(//,?,?)).
:- meta_predicate(dcg_call_cp(3,?,?,?)).
:- meta_predicate(dcg_call_cp(4,?,?,?,?)).
:- meta_predicate(dcg_call_cp(5,?,?,?,?,?)).
:- meta_predicate(dcg_call_cp(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_call_cp(7,?,?,?,?,?,?,?)).
:- meta_predicate(dcg_maplist(3,+,?,?)).
:- meta_predicate(dcg_maplist(4,+,+,?,?)).
:- meta_predicate(dcg_maplist(5,+,+,+,?,?)).
:- meta_predicate(dcg_maplist(6,+,+,+,+,?,?)).
:- meta_predicate(dcg_maplist(7,+,+,+,+,+,?,?)).
:- meta_predicate(dcg_once(//,?,?)).
:- meta_predicate(dcg_sequence(:,//,?,?)).



%! dcg_apply(:Dcg, +Arguments:list)// .
% @see Variant of apply/2 for DCGs.

dcg_apply(Dcg, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(Dcg, Args2).

dcg_apply_cp(Dcg1, Args1, X, Y):-
  copy_term(Dcg1, Dcg2),
  append(Args1, [X,Y], Args2),
  apply(Dcg2, Args2).



%! dcg_atom_codes(:Dcg, ?Atom:atom)// .
% This meta-DCG rule handles the translation
% between the word and the character level of parsing/generating.
%
% Typically, grammar *A* specifies how words can be formed out of characters.
% A character is a code, and a word is a list of codes.
% Grammar *B* specifies how sentences can be built out of words.
% Now the word is an atom, and the sentences in a list of atoms.
%
% This means that at some point,
% words in grammar *A*, i.e. lists of codes,
% need to be translated to words in grammar *B*, i.e. atoms.
%
% This is where dcg_atom_codes//2 comes in.
% We illustrate this with a schematic example:
% ```prolog
% sentence([W1,...,Wn]) -->
%   word2(W1),
%   ...,
%   word2(Wn).
%
% word2(W) -->
%   dcg_atom_codes(word1, W).
%
% word1([C1, ..., Cn]) -->
%   char(C1),
%   ...,
%   char(Cn).
% ```

dcg_atom_codes(Dcg, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_call_cp(Dcg, Codes).
dcg_atom_codes(Dcg, Atom) -->
  {var(Atom)},
  dcg_call_cp(Dcg, Codes),
  {atom_codes(Atom, Codes)}.


%! dcg_call(:Dcg)// .
%! dcg_call(:Dcg, +Args1:list)// .
% @see Variants of dcg_call//[1-6], where `Dcg` is called directly
%      (i.e., not copied). This means that multiple calls of the same `Dcg`
%      share uninstantiated variables.

dcg_call(Dcg, X, Y):-
  call(Dcg, X, Y).

dcg_call(Dcg, A1, X, Y):-
  call(Dcg, A1, X, Y).

dcg_call(Dcg, A1, A2, X, Y):-
  call(Dcg, A1, A2, X, Y).

dcg_call(Dcg, A1, A2, A3, X, Y):-
  call(Dcg, A1, A2, A3, X, Y).

dcg_call(Dcg, A1, A2, A3, A4, X, Y):-
  call(Dcg, A1, A2, A3, A4, X, Y).

dcg_call(Dcg, A1, A2, A3, A4, A5, X, Y):-
  call(Dcg, A1, A2, A3, A4, A5, X, Y).


%! dcg_call_cp(:Dcg)// .
% @see Included for consistency with dcg_call//[2,3,4].
% @see This has the same effect as phrase/3.
%! dcg_call_cp(:Dcg, +Args1:list)// .
% @see Variants of call/[1-5] for DCGs.

dcg_call_cp(Dcg1, X, Y):-
  copy_term(Dcg1, Dcg2),
  call(Dcg2, X, Y).

dcg_call_cp(Dcg1, A1, X, Y):-
  copy_term(Dcg1, Dcg2),
  call(Dcg2, A1, X, Y).

dcg_call_cp(Dcg1, A1, A2, X, Y):-
  copy_term(Dcg1, Dcg2),
  call(Dcg2, A1, A2, X, Y).

dcg_call_cp(Dcg1, A1, A2, A3, X, Y):-
  copy_term(Dcg1, Dcg2),
  call(Dcg2, A1, A2, A3, X, Y).

dcg_call_cp(Dcg1, A1, A2, A3, A4, X, Y):-
  copy_term(Dcg1, Dcg2),
  call(Dcg2, A1, A2, A3, A4, X, Y).

dcg_call_cp(Dcg1, A1, A2, A3, A4, A5, X, Y):-
  copy_term(Dcg1, Dcg2),
  call(Dcg2, A1, A2, A3, A4, A5, X, Y).


%! dcg_maplist(:Dcg, +Args1:list)// .
% @see Variants of maplist/[1-5] for DCGs.

dcg_maplist(_, []) --> [].
dcg_maplist(Dcg, [H|T]) -->
  dcg_apply(Dcg, H),
  dcg_maplist(Dcg, T).

dcg_maplist(_, [], []) --> [].
dcg_maplist(Dcg, [H1|T1], [H2|T2]) -->
  dcg_call_cp(Dcg, H1, H2),
  dcg_maplist(Dcg, T1, T2).

dcg_maplist(_, [], [], []) --> [].
dcg_maplist(Dcg, [H1|T1], [H2|T2], [H3|T3]) -->
  dcg_call_cp(Dcg, H1, H2, H3),
  dcg_maplist(Dcg, T1, T2, T3).

dcg_maplist(_, [], [], [], []) --> [].
dcg_maplist(Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) -->
  dcg_call_cp(Dcg, H1, H2, H3, H4),
  dcg_maplist(Dcg, T1, T2, T3, T4).

dcg_maplist(_, [], [], [], [], []) --> [].
dcg_maplist(Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) -->
  dcg_call_cp(Dcg, H1, H2, H3, H4, H5),
  dcg_maplist(Dcg, T1, T2, T3, T4, T5).


%! dcg_once(:Dcg)// .
% Calls the given DCG at most one time.
%
% @see DCG version of once/1.

dcg_once(Dcg, X, Y):-
  once(phrase(Dcg, X, Y)).


%! dcg_repeat// .
% @see DCG version of repeat/1.

dcg_repeat(X, X):-
  repeat.


%! dcg_sequence(:Dcgs:list, :Separator)// .
% Parse/generate a number of DCGs in sequence, separated by `Separator`.

dcg_sequence(_:[], _) --> [].
dcg_sequence(Mod:[H|T], Sep) -->
  Mod:H,
  dcg_yn_separator(T, Sep),
  dcg_sequence(Mod:T, Sep).

