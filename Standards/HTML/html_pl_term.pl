:- module(
  html_pl_term,
  [
    html_pl_term//1 % +PL_Term
  ]
).

/** <module> HTML Prolog term

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(http/html_write)).



html_pl_term(PL_Term) -->
  {term_to_atom(PL_Term, Atom)},
  html(Atom).
