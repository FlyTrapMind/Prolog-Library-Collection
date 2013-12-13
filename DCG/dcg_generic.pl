:- module(
  dcg_generic,
  [
    dcg_end//0,
    dcg_separated_list//2, % :Separator:dcg
                           % ?Codess:list(list(codes))
    dcg_phrase/2, % :DCG_Body:dcg
                  % ?In:atom
    dcg_phrase/3 % :DCG_Body:dcg
                 % +In:atom
                 % -Out:atom
  ]
).

/** <module>

Generic DCG clauses. DCGs allow the definition of a complex grammar in
a modular way.

## Concepts

  * *|Lexical analysis|*
    *Tokenization*
    The process of converting characters to tokens
    (i.e., strings of characters).

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- use_module(dcg(dcg_control)).

:- meta_predicate(dcg_separated_list(//,?,?,?)).
:- meta_predicate(dcg_separated_list_nonvar(//,+,?,?)).
:- meta_predicate(dcg_separated_list_var(//,-,?,?)).
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,+,-)).



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

dcg_phrase(DCG_Body, Atom):-
  var(Atom), !,
  phrase(DCG_Body, [H|T]),
  (
    number(H)
  ->
    atom_codes(Atom, [H|T])
  ;
    atom(H)
  ->
    atomic_list_concat([H|T], Atom)
  ).
dcg_phrase(DCG_Body, Atom):-
  atom_codes(Atom, Codes),
  phrase(DCG_Body, Codes).

dcg_phrase(DCG_Body, InAtom, OutAtom):-
  atom(InAtom),
  atom_codes(InAtom, InCodes),
  phrase(DCG_Body, InCodes, OutCodes),
  atom_codes(OutAtom, OutCodes).
