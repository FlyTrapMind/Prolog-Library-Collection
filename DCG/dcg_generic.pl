:- module(
  dcg_generic,
  [
    dcg_end//0,
    dcg_separated_list//2, % :Separator:dcg
                           % ?Codess:list(list(codes))
    dcg_phrase/2, % :DCG
                  % ?AtomicOrCodes:or([atom,list(code),number])
    dcg_phrase/3 % :DCG
                 % ?AtomicOrCodes1:or([atom,list(code),number])
                 % ?AtomicOrCodes2:or([atom,list(code),number])
  ]
).

/** <module> DCG generics.

Generic support for DCG rules.

## Concepts

  * *|Lexical analysis|*
    *Tokenization*
    The process of converting characters to tokens
    (i.e., strings of characters).

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2014/01
*/

:- use_module(dcg(dcg_control)).
:- use_module(library(apply)).

:- meta_predicate(dcg_separated_list(//,?,?,?)).
:- meta_predicate(dcg_separated_list_nonvar(//,+,?,?)).
:- meta_predicate(dcg_separated_list_var(//,-,?,?)).
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).



dcg_end([], []).

%! dcg_separated_list(
%!   +Separator:dcg_rule,
%!   ?CodeLists:list(list(code))
%! )// is det.
% @tbd This does not work for the following string:
% ~~~
% "error(permission_error(delete,file,\'c:/users/quirinus/.webqr/export.svg\'),context(system:delete_file/1,\'Permission denied\'))"
% ~~~

dcg_separated_list(Sep, L) -->
  {nonvar(L)}, !,
  dcg_separated_list_nonvar(Sep, L).
dcg_separated_list(Sep, L) -->
  {var(L)}, !,
  dcg_separated_list_var(Sep, L).

dcg_separated_list_nonvar(_Sep, [H]) --> !,
  H.
dcg_separated_list_nonvar(Sep, [H|T]) -->
  H,
  Sep,
  dcg_separated_list_nonvar(Sep, T).

dcg_separated_list_var(Sep, [H|T]) -->
  dcg_until([end_mode(exclusive),output_format(codes)], Sep, H),
  Sep, !,
  dcg_separated_list_var(Sep, T).
dcg_separated_list_var(_Sep, [H]) -->
  dcg_all(H), !.


%! dcg_phrase(:DCG, ?AtomicOrCodes:or([atom,list(code),number]))// is nondet.
%! dcg_phrase(
%!   :DCG,
%!   ?AtomicOrCodes1:or([atom,list(code),number]),
%!   ?AtomicOrCodes2:or([atom,list(code),number])
%! )// is nondet.

dcg_phrase(DCG, AtomicOrCodes):-
  dcg_phrase(DCG, AtomicOrCodes, []).
dcg_phrase(DCG, Atomic1, Atomic2):-
  atom(Atomic1), !,
  atom_codes(Atomic1, Codes1),
  dcg_phrase(DCG, Codes1, Codes2),
  atom_codes(Atomic2, Codes2).
dcg_phrase(DCG, Atomic1, Atomic2):-
  atom(Atomic2), !,
  atom_codes(Atomic2, Codes2),
  dcg_phrase(DCG, Codes1, Codes2),
  atom_codes(Atomic1, Codes1).
dcg_phrase(DCG, Atomic1, Atomic2):-
  number(Atomic1), !,
  number_codes(Atomic1, Codes1),
  dcg_phrase(DCG, Codes1, Codes2),
  number_codes(Atomic2, Codes2).
dcg_phrase(DCG, Atomic1, Atomic2):-
  number(Atomic2), !,
  number_codes(Atomic2, Codes2),
  dcg_phrase(DCG, Codes1, Codes2),
  number_codes(Atomic1, Codes1).
dcg_phrase(DCG, Atomic1, Atomic2):-
  maplist(var, [Atomic1,Atomic2]), !,
  dcg_phrase(DCG, Codes1, Codes2),
  maplist(atom_codes, [Atomic1,Atomic2], [Codes1,Codes2]).
dcg_phrase(DCG, Codes1, Codes2):-
  phrase(DCG, Codes1, Codes2).

