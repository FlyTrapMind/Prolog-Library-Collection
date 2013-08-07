:- module(
  int_ext,
  [
    int_div/3, % +In1:integer
               % +In2:integer
               % -Out:integer
    int_mod/3, % +In1:integer
               % +In2:integer
               % -Out:integer
    int_parts/3, % +Integer:integer
                 % ?IntegerPart:integer
                 % ?FractionalPart:between(0.0,1.0)
    int_plus/3 % ?X:integer
               % ?Y:integer
               % ?Z:integer
  ]
).

/** <module> INT_EXT

Support predicates for integer values.

@author Wouter Beek
@version 2013/08
*/



int_div(X, Y, Z):-
  Z is floor(X / Y).

int_mod(X, Y, Z):-
  Z is X mod Y.

int_parts(I, I, 0.0).

%! int_plus(?X:integer, ?Y:integer, ?Z:integer) is det.

int_plus(X, Y, Z):-
  plus(X, Y, Z).

