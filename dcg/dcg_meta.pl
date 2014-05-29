:- module(
  dcg_meta,
  [
    dcg_apply//2, % :Dcg
                  % +Arguments:list
    dcg_atom_codes//2, % :Dcg
                       % ?Atom:atom
    dcg_call//1,
    dcg_call//2,
    dcg_call//3,
    dcg_call//4,
    dcg_call//5,
    dcg_call//6,
    dcg_maplist//2, % :Dcg
                    % +Arguments:list
    dcg_maplist//3, % :Dcg
                    % +Arguments1:list
                    % +Arguments2:list
    dcg_nth0_call//3, % :Goal
                      % +Index:nonneg
                      % +Argument
    dcg_nth0_call//4, % +Options:list(nvpair)
                      % :Goal
                      % +Index:nonneg
                      % +Argument
    dcg_repeat//2, % :Dcg
                   % +NumberOfRepeats:nonneg
    dcg_sequence//2 % :Dcgs:list
                    % :Separator
  ]
).

/** <module> DCG meta

Meta-DCG rules.

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/09, 2013/11-2013/12, 2014/02-2014/03, 2014/05
*/

:- use_module(library(error)).
:- use_module(library(lists)).

:- use_module(dcg(dcg_generic)).
:- use_module(generics(list_ext)).

:- meta_predicate('?'(//,?,?)).
:- meta_predicate('?_det'(//,?,?)).
:- meta_predicate(dcg_apply(//,+,?,?)).
:- meta_predicate(dcg_atom_codes(//,?,?,?)).
:- meta_predicate(dcg_call(2,?,?)).
:- meta_predicate(dcg_call(3,?,?,?)).
:- meta_predicate(dcg_call(4,?,?,?,?)).
:- meta_predicate(dcg_call(5,?,?,?,?,?)).
:- meta_predicate(dcg_call(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_call(7,?,?,?,?,?,?,?)).
:- meta_predicate(dcg_maplist(3,+,?,?)).
:- meta_predicate(dcg_maplist(4,+,+,?,?)).
:- meta_predicate(dcg_nth0_call(3,+,+,?,?)).
:- meta_predicate(dcg_nth0_call(+,3,+,+,?,?)).
:- meta_predicate(dcg_repeat(//,+,?,?)).
:- meta_predicate(dcg_sequence(:,//,?,?)).


%! '+'(:Dcg)// .

'*+'(Dcg) --> Dcg, '*'(Dcg).
'+'(_) --> [].


%! '+'(:Dcg, ?Args1, ...)// .

'+'(Dcg, [H1|T1]) -->
  dcg_call(Dcg, H1),
  '+'(Dcg, T1).
'+'(Dcg, [H1]) -->
  dcg_call(Dcg, H1).

'+'(Dcg, [H1|T1], [H2|T2]) -->
  dcg_call(Dcg, H1, H2),
  '+'(Dcg, T1, T2).
'+'(Dcg, [H1], [H2]) -->
  dcg_call(Dcg, H1, H2).

'+'(Dcg, [H1|T1], [H2|T2], [H3|T3]) -->
  dcg_call(Dcg, H1, H2, H3),
  '+'(Dcg, T1, T2, T3).
'+'(Dcg, [H1], [H2], [H3]) -->
  dcg_call(Dcg, H1, H2, H3).

'+'(Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) -->
  dcg_call(Dcg, H1, H2, H3, H4),
  '+'(Dcg, T1, T2, T3, T4).
'+'(Dcg, [H1], [H2], [H3], [H4]) -->
  dcg_call(Dcg, H1, H2, H3, H4).

'+'(Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) -->
  dcg_call(Dcg, H1, H2, H3, H4, H5),
  '+'(Dcg, T1, T2, T3, T4, T5).
'+'(Dcg, [H1], [H2], [H3], [H4], [H5]) -->
  dcg_call(Dcg, H1, H2, H3, H4, H5).


%! '+'(:Dcg)// .

'+'(Dcg) --> Dcg, '+'(Dcg).
'+'(Dcg) --> Dcg.


%! '+'(:Dcg, ?Args1, ...)// .

'+'(Dcg, [H1|T1]) -->
  dcg_call(Dcg, H1),
  '+'(Dcg, T1).
'+'(Dcg, [H1]) -->
  dcg_call(Dcg, H1).

'+'(Dcg, [H1|T1], [H2|T2]) -->
  dcg_call(Dcg, H1, H2),
  '+'(Dcg, T1, T2).
'+'(Dcg, [H1], [H2]) -->
  dcg_call(Dcg, H1, H2).

'+'(Dcg, [H1|T1], [H2|T2], [H3|T3]) -->
  dcg_call(Dcg, H1, H2, H3),
  '+'(Dcg, T1, T2, T3).
'+'(Dcg, [H1], [H2], [H3]) -->
  dcg_call(Dcg, H1, H2, H3).

'+'(Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) -->
  dcg_call(Dcg, H1, H2, H3, H4),
  '+'(Dcg, T1, T2, T3, T4).
'+'(Dcg, [H1], [H2], [H3], [H4]) -->
  dcg_call(Dcg, H1, H2, H3, H4).

'+'(Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) -->
  dcg_call(Dcg, H1, H2, H3, H4, H5),
  '+'(Dcg, T1, T2, T3, T4, T5).
'+'(Dcg, [H1], [H2], [H3], [H4], [H5]) -->
  dcg_call(Dcg, H1, H2, H3, H4, H5).


%! '?'(:Dcg)// .
% Implements the Regular Expression operator `?`,
% generating *both* the case of 0 occurrences *and* the case of 1 occurrence.

'?'(Dcg) --> Dcg.
'?'(_) --> [].


%! '?_det'(:Dcg)// .
% Implements the Regular Expression operator `?`,
% generating *either* the case of 0 occurrences *or* the case of 1 occurrence.

'?_det'(Dcg) --> Dcg -> [] ; [].


%! dcg_apply(:Dcg, +Arguments:list)// .
% @see Variant of apply/2 for DCGs.

dcg_apply(Dcg, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(Dcg, Args2).


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
% ~~~{.pl}
% sentence([W1,...,Wn]) -->
%   word2(W1),
%   ...,
%   word2(Wn).
% 
% word2(W) -->
%   dcg_atom_codes(word1, W).
% 
% word1(C1, ..., Cn) --> 
%   char(C1),
%   ...,
%   char(Cn).
% ~~~

dcg_atom_codes(Dcg, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_call(Dcg, Codes).
dcg_atom_codes(Dcg, Atom) -->
  {var(Atom)},
  dcg_call(Dcg, Codes),
  {atom_codes(Atom, Codes)}.


%! dcg_call(:Dcg)//
% @see Included for consistency with dcg_call//[2,3,4].
% @see This has the same effect as phrase/3.

dcg_call(Dcg, X, Y):-
  call(Dcg, X, Y).

%! dcg_call(:Dcg, +ExtraArg1, ...)// .
% Currently only dcg_call//[2-6] are defined.
%
% @see Variant of call/[1-5] for DCGs.

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


%! dcg_maplist(:Dcg, +Args:list)// .

dcg_maplist(_, []) --> [].
dcg_maplist(Dcg, [H|T]) -->
  dcg_apply(Dcg, H),
  dcg_maplist(Dcg, T).


%! dcg_maplist(:Dcg, +Args1:list, +Args2:list)// .

dcg_maplist(_, [], []) --> [].
dcg_maplist(Dcg, [H1|T1], [H2|T2]) -->
  dcg_call(Dcg, H1, H2),
  dcg_maplist(Dcg, T1, T2).


%! dcg_nth0_call(:Dcg, +Index:nonneg, +Element)// .
%! dcg_nth0_call(+Options:list(nvpair), :Dcg, +Index:nonneg, +Element)// .
% The following options are supported:
%   * =|minus(+UseMinus:boolean)|=
%     When `true` (default `false`), uses nth0_minus/4
%     instead of nth0/4. See module [list_ext].
%
% @see This meta-DCG is based on nth0_call/[3,4] in module [meta_ext].

dcg_nth0_call(Dcg, I, X) -->
  dcg_nth0_call([], Dcg, I, X).

dcg_nth0_call(O1, Dcg1, I, X) -->
  {
    strip_module(Dcg1, Mod, Dcg2),
    Dcg2 =.. [Pred|Args1],

    % Insert the extra argument.
    (
      option(minus(true), O1, false)
    ->
      nth0_minus(I, Args2, X, Args1)
    ;
      nth0(I, Args2, X, Args1)
    )
  },
  dcg_apply(Mod:Pred, Args2).


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


%! dcg_repeat(:Dcg, +NumberOfRepeats:nonneg)// .

dcg_repeat(_, N) -->
  {N < 0}, !,
  {domain_error(nonneg, N)}.
dcg_repeat(_, 0) --> !, [].
dcg_repeat(Dcg, N1) -->
  Dcg,
  {N2 is N1 - 1},
  dcg_repeat(Dcg, N2).


%! dcg_sequence(:Dcgs:list, :Separator)// .
% Parse/generate a number of DCGs in sequence, separated by `Separator`.

dcg_sequence(_:[], _) --> [].
dcg_sequence(Mod:[H|T], Sep) -->
  Mod:H,
  dcg_yn_separator(T, Sep),
  dcg_sequence(Mod:T, Sep).

