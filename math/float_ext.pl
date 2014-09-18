:- module(
  float_ext,
  [
    float_between/3, % ?Low:float
                     % ?High:float
                     % +Number:float
    float_div/3, % +In1:float
                 % +In2:float
                 % -Out:float
    float_mod/3, % +In1:float
                 % +In2:float
                 % -Out:float
    float_plus/3, % ?X:float
                  % ?Y:float
                  % ?Z:float
    to_float/2 % +Atomic:or([atom,float])
               % -Float:float
  ]
).

/** <module> Floating-point number extensions

Support predicates for floating point values.

@author Wouter Beek
@version 2013/08, 2014/05, 2014/09
*/



%! float_between(?Low:float, ?High:float, +Number:float) is semidet.

float_between(Low, High, Number):-
  % Meet the lower boundary requirement.
  (   var(Low)
  ->  true
  ;   Low =< Number
  ),
  
  % Meet the higher boundary requirement.
  (   var(High)
  ->  true
  ;   Number =< High
  ).


float_div(X, Y, Z):-
  Z is X / Y.


float_mod(X, Y, Z):-
  float_div(X, Y, DIV),
  Z is X - DIV * Y.


%! float_plus(?X:number, ?Y:number, ?Z:number) is det.
% Calculates the sum Z = X + Y as long as at least two arguments are
% instantiated.
%
% @see The builin plus/3 only works for integers.

float_plus(X, Y, Z):-
  nonvar(X), nonvar(Y), !,
  Z is X + Y.
float_plus(X, Y, Z):-
  nonvar(X), nonvar(Z), !,
  Y is Z - X.
float_plus(X, Y, Z):-
  nonvar(Y), nonvar(Z), !,
  X is Z - Y.


%! to_float(+Atomic:or([atom,float]), -Float:float) is det.

to_float(Float, Float):-
  float(Float), !.
to_float(Atom, Float):-
  atom(Atom),
  atom_number(Atom, Number),
  to_float(Number, Float).

