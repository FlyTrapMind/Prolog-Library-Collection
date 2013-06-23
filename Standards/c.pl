:- module(
  c,
  [
    c_convert//0
  ]
).

/** <module> C

Support for the C programming language.

@author Wouter Beek
@version 2013/02, 2013/06
*/

:- use_module(dcg(dcg_generic)).



c_convert -->
  dcg_replace(c_from(X), c_to(X)).

c_from(X) -->
  {c_trans(X,_Y)},
  X.

c_to(X) -->
  {c_trans(X,Y)},
  Y.

% Replace the bell character with '\b'.
c_trans([92, 98], [7]).
% Replace the line feed character with '\n'.
c_trans([92, 110], [10]).
% Replace the horizontal tab character with '\t'.
c_trans([92, 116], [9]).

