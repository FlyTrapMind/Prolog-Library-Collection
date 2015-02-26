:- module(
  dcg_atom,
  [
    atom//1, % ?Atom:atom
    atom_ci//1, % ?Atom:atom
    atom_ellipsis//2, % +Atom:atom
                      % +Ellipsis:positive_integer
    atom_lower//1, % ?Atom:atom
    atom_title//1, % ?Atom:atom
    atom_upper//1 % ?Atom:atom
  ]
).

/** <module> DCG atom

Grammar rules for processing atoms.

@author Wouter Beek
@version 2014/11
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(code_ext)). % Meta-option.

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_code)).



%! atom(?Atom:atom)// .

atom(Atom, Head, Tail):-
  atom(Atom), !,
  format(codes(Head,Tail), '~w', [Atom]).
atom(Atom) -->
  '*'(code, Atom, [convert1(codes_atom)]).



%! atom_ci(?Atom:atom)// .

atom_ci(Atom) -->
  '*'(code_ci, Atom, [convert1(codes_atom)]).



%! atom_ellipsis(+Atom:atom, +Ellipsis:positive_integer)// .

atom_ellipsis(Atom1, Ellipsis) -->
  {atom_truncate(Atom1, Ellipsis, Atom2)},
  atom(Atom2).



%! atom_lower(?Atom:atom)// .

atom_lower(Atom) -->
  '*'(code_lower, Atom, [convert1(codes_atom)]).



%! atom_title(?Atom:atom) // .

atom_title(Atom) -->
  {var(Atom)}, !,
  letter_uppercase(H),
  '*'(letter_lowercase, T, []),
  {atom_codes(Atom, [H|T])}.
atom_title('') --> "".
atom_title(Atom) -->
  {atom_codes(Atom, [H|T])},
  letter_uppercase(H),
  '*'(letter_lowercase, T, []).



%! atom_upper(?Atom:atom)// .

atom_upper(Atom) -->
  '*'(code_lower, Atom, [convert1(codes_atom)]).
