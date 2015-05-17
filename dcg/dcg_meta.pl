:- module(
  dcg_meta,
  [
    dcg_apply//2, % :Dcg
                  % +Arguments:list
    dcg_apply_cp//2, % :Dcg
                     % +Arguments:list
    dcg_atom//2, % :Dcg
                 % ?Atom:atom
    dcg_between//2, % :Between
                    % :Dcg
    dcg_between//3, % :Begin
                    % :Dcg
                    % :End
    dcg_call//1, dcg_call//2, dcg_call//3, dcg_call//4, dcg_call//5, dcg_call//6,
    dcg_call_cp//1, dcg_call_cp//2, dcg_call_cp//3, dcg_call_cp//4, dcg_call_cp//5, dcg_call_cp//6,
    dcg_number//2, % :Dcg
                         % ?Number:number
    dcg_once//1, % :Dcg
    dcg_repeat//0,
    dcg_string//2 % :Dcg
                  % ?String:string
  ]
).

/** <module> DCG: Meta-rules

Meta-DCG rules.

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/09, 2013/11-2013/12, 2014/02-2014/03, 2014/05,
         2014/08-2014/09, 2015/05
*/

:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/list_ext)).
:- use_module(plc(math/radix)).

:- meta_predicate(dcg_apply(//,+,?,?)).
:- meta_predicate(dcg_apply_cp(//,+,?,?)).
:- meta_predicate(dcg_atom(3,?,?,?)).
:- meta_predicate(dcg_between(//,//,?,?)).
:- meta_predicate(dcg_between(//,//,//,?,?)).
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
:- meta_predicate(dcg_number(3,?,?,?)).
:- meta_predicate(dcg_once(//,?,?)).
:- meta_predicate(dcg_string(//,?,?,?)).





%! dcg_apply(:Dcg, +Arguments:list)// .
% @see Variant of apply/2 for DCGs.

dcg_apply(Dcg, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(Dcg, Args2).

dcg_apply_cp(Dcg1, Args1, X, Y):-
  copy_term(Dcg1, Dcg2),
  append(Args1, [X,Y], Args2),
  apply(Dcg2, Args2).



%! dcg_atom(:Dcg, ?Atom:atom)// .
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
% This is where dcg_atom//2 comes in.
% We illustrate this with a schematic example:
% ```prolog
% sentence([W1,...,Wn]) -->
%   word2(W1),
%   ...,
%   word2(Wn).
%
% word2(W) -->
%   dcg_atom(word1, W).
%
% word1([C1, ..., Cn]) -->
%   char(C1),
%   ...,
%   char(Cn).
% ```

dcg_atom(Dcg, Atom) -->
  {var(Atom)}, !,
  dcg_call(Dcg, Codes),
  {atom_codes(Atom, Codes)}.
dcg_atom(Dcg, Atom) -->
  {atom(Atom)}, !,
  {atom_codes(Atom, Codes)},
  dcg_call(Dcg, Codes).
dcg_atom(_, Atom) -->
  {type_error(atom, Atom)}.



%! dcg_between(:Between, :Dcg)// .

dcg_between(Between, Dcg) -->
  dcg_between(Between, Dcg, Between).

%! dcg_between(:Begin, :Dcg, :End)// .

dcg_between(Begin, Dcg, End) -->
  Begin,
  Dcg,
  End.



%! dcg_call(:Dcg)// .
%! dcg_call(:Dcg, ?Arg1)// .
%! dcg_call(:Dcg, ?Arg1, ?Arg2)// .
%! dcg_call(:Dcg, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call(:Dcg, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call(:Dcg, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
% `Dcg` is called directly (i.e., not copied).
% This means that multiple calls of the same Dcg share uninstantiated
% variables.
%
% @see DCG variants of call//[1-6].

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
%! dcg_call_cp(:Dcg, ?Arg1)// .
%! dcg_call_cp(:Dcg, ?Arg1, ?Arg2)// .
%! dcg_call_cp(:Dcg, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call_cp(:Dcg, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call_cp(:Dcg, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
% dcg_call_cp//1 is included for consistency.
% It is operationally equivalent with dcg_call//1.
%
% @see This is a syntactic variant of phrase/3.

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



dcg_number(Dcg, Number) -->
  {var(Number)}, !,
  '*'(dcg_call(Dcg), Weights, []),
  {weights_radix(Weights, Number)}.
dcg_number(Dcg, Number) -->
  {weights_radix(Weights, Number)},
  '*'(dcg_call(Dcg), Weights, []).



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



dcg_string(Dcg, String) -->
  {var(String)}, !,
  dcg_call(Dcg, Codes),
  {string_codes(String, Codes)}.
dcg_string(Dcg, String) -->
  {string(String)}, !,
  {string_codes(String, Codes)},
  dcg_call(Dcg, Codes).
dcg_string(_, String) -->
  {type_error(string, String)}.
