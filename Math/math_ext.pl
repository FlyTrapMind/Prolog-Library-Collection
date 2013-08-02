:- module(
  math_ext,
  [
    average/2, % +Numbers:list(number)
               % -Average:number
    cartesian_distance/3, % +Coordinate1:coordinate
                          % +Coordinate2:coordinate
                          % -CartesianDistance:float
    circumfence/2, % +Radius:float
                   % -Circumfence:float
    combinations/3, % +NumberOfObjects:integer
                    % +CombinationLength:integer
                    % -NumberOfCombinations:integer
    cyclic_numlist/4, % +Min:integer
                      % +Max:integer
                      % +CycleLength:integer
                      % -NumList:list(integer)
    even/1, % +Integer:integer
    factorial/2, % +N:integer
                 % -F:integer
    fibonacci/2, % ?Index:integer
                 % ?Fibonacci:integer
    float_components/3, % ?Float:float
                        % ?IntegerComponent:integer
                        % ?FractionalComponent:integer
    float_fractional_component/2, % ?Float:float
                                  % ?FractionalComponent:integer
    float_integer_component/2, % ?Float:float
                               % ?IntegerComponent:integer
    log/3, % +Base:integer
           % +X:float
           % +Y:float
    minus/3, % ?X:number
             % ?Y:number
             % ?Z:number
    minus_list/3, % +N:number
                  % +Ms:list(number)
                  % -N_Minus_Ms:number
    multiply/3, % ?X:float
                % ?Y:float
                % ?Z:float
    multiply_list/2, % +Numbers:list(number)
                     % -Multiplication:number
    number_length/2, % +Number:number
                     % -Length:integer
    number_length/3, % +Number:number
                     % +Radix:integer
                     % -Length:integer
    odd/1, % +Integer:integer
    permutations/2, % +NumberOfObjects:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumbersOfObjects:list(integer)
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    permutations/3, % +NumberOfObjects:integer
                    % +PermutationLength:integer
                    % -NumberOfPermutations:integer
    plus_float/3, % ?X:number
                  % ?Y:number
                  % ?Z:number
    pred/2, % +X:integer
            % -Y:integer
    random_betwixt/2, % +UpperLimit:number
                      % -Random:number
    random_betwixt/3, % +LowerLimit:number
                      % +UpperLimit:number
                      % -Random:number
    random_coordinate/2, % +Size:size,
                         % -Coordinate:coordinate
    rbetween/3, % +Low:integer
                % +High:integer
                % ?Value:integer
    square/2 % +X:float
             % -Square:float
  ]
).

/** <module> Artihmetic extensions for SWI-Prolog

Extra arithmetic functions for use in SWI-Prolog.

# Issue with float_fractional_part/2

The fractional part of floats seems to off a bit:
~~~
3 ?- X = 23.3, X_F is float_fractional_part(X), X_I is float_integer_part(X), Y is X_I + X_F.
X = Y, Y = 23.3,
X_F = 0.3000000000000007,
X_I = 23.0.
~~~

In C I create a small test file:
~~~{.c}
#include <stdio.h>
#include <math.h>

int main() {
  double param, fractional_part, integer_part;

  param = 23.3;
  fractional_part = modf(param, &integer_part);
  printf("%f = %f + %f \n", param, integer_part, fractional_part);

  return 0;
}
~~~
Then I run the test file:
~~~
$ gcc -Wall test.c -o test
$ ./test
23.300000 = 23.000000 + 0.300000
~~~
To find out that the deviation I see in SWI-Prolog is not in C.

Here is the relevant code, collected from various spots in the SWI-Prolog
codebase:
~~~{.c}
// O_GMP
// Use GNU gmp library for infinite precision arthmetic
#define O_GMP 1

// the numtype enum requires total ordering.
typedef enum {
  V_INTEGER,   // integer (64-bit) value
#ifdef O_GMP
  // The C data type for multiple precision integers is mpz_t.
  V_MPZ,   // mpz_t
  // Rational number means a multiple precision fraction.
  // The C data type for these fractions is mpq_t.
  V_MPQ,   // mpq_t
#endif
  V_FLOAT   // Floating point number (double)
} numtype;

// X is float_integer_part(X) + float_fractional_part(X)
// If X < 0, both float_integer_part(X) and float_integer_part(X) are <= 0
static int
ar_float_fractional_part(Number n1, Number r) {
  switch(n1->type) {
    case V_INTEGER:
#ifdef O_GMP
    case V_MPZ:
#endif
      r->value.i = 0;
      r->type = V_INTEGER;
      succeed;
#ifdef O_GMP
    case V_MPQ:
      r->type = V_MPQ;
      mpq_init(r->value.mpq);
      mpz_tdiv_q(mpq_numref(r->value.mpq),
      mpq_numref(n1->value.mpq),
      mpq_denref(n1->value.mpq));
      mpz_set_ui(mpq_denref(r->value.mpq), 1);
      mpq_sub(r->value.mpq, n1->value.mpq, r->value.mpq);
      succeed;
#endif
    case V_FLOAT:
      {
        double ip;
        // The member `value` of the object pointed to by `r`.
        // The member `f` of object `(r->value)`.
        r->value.f = modf(n1->value.f, &ip);
        r->type = V_FLOAT;
      }
  }
  succeed;
}
~~~

@author Wouter Beek
@version 2011/08-2012/02, 2012/09-2012/10, 2012/12, 2013/07
*/

:- use_module(generics(meta_ext)).
:- use_module(library(lists)).



average(Numbers, Average):-
  sum_list(Numbers, Sum),
  length(Numbers, NumberOfNumbers),
  Average is Sum / NumberOfNumbers.

%! cartesian_distance(
%!   +Coordinate1:coordinate,
%!   +Coordinate2:coordinate,
%!   -CartesianDistance:float
%! ) is det.
% Returns the Cartesian distance between two coordinates.

cartesian_distance(
  coordinate(Dimension, Args1),
  coordinate(Dimension, Args2),
  CartesianDistance
):-
  maplist(minus, Args1, Args2, X1s),
  maplist(square, X1s, X2s),
  sum_list(X2s, X2),
  CartesianDistance is sqrt(X2).

%! circumfence(+Radius:float, -Circumfence:float) is det.
% Returns the circumfence of a circle with the given radius.

circumfence(Radius, Circumfence):-
  Circumfence is Radius * pi * 2.

%! combinations(
%!   +NumberOfObjects:integer,
%!   +CombinationLength:integer,
%!   -NumberOfCombinations:integer
%! ) is det.
% Returns the number of combinations from the given objects and
% of the given size.
%
% *Definition*: A combination is a permutation in which the order
%               neglected. Therefore, $r!$ permutations correspond to
%               one combination (with r the combination length).

combinations(NumberOfObjects, CombinationLength, NumberOfCombinations):-
  permutations(NumberOfObjects, CombinationLength, NumberOfPermutations),
  factorial(CombinationLength, F),
  NumberOfCombinations is NumberOfPermutations / F.

%! cyclic_numlist(
%!   +Min:integer,
%!   +Max:integer,
%!   +CycleLength:integer,
%!   -NumList:list(integer)
%! ) is det.
% Generates a number list for a cyclic list of numbers.
% This method works on a off-by-zero basis.
% We return the numbers in a sorted order.

cyclic_numlist(Min, Max, _CycleLength, NumList):-
  Min < Max, !,
  numlist(Min, Max, NumList).
cyclic_numlist(Min, Max, CycleLength, NumList):-
  Top is CycleLength - 1,
  numlist(Min, Top, HigherNumList),
  numlist(0, Max, LowerNumList),
  append(LowerNumList, HigherNumList, NumList).

%! even(+Integer:integer) is semidet.
% Succeeds if the integer is even.
%
% @arg Integer An integer.

even(Integer):-
  0 is Integer mod 2.

%! factorial(+N:integer, -F:integer) is det.
% Returns the factorial of the given number.
%
% The standard notation for the factorial of _|n|_ is _|n!|_.
%
% *Definition*: $n! = \prod_{i = 1}^n i$

factorial(N, F):-
  numlist(1, N, Numbers),
  !,
  multiply_list(Numbers, F).
% E.g., $0!$.
factorial(_N, 1).

fibonacci(0, 1):- !.
fibonacci(1, 1):- !.
fibonacci(N, F):-
  N1 is N - 1,
  N2 is N - 2,
  fibonacci(N1, F1),
  fibonacci(N2, F2),
  F is F1 + F2.

float_components(N, N_I, N_F):-
  var(N), !,
  number_length(N_F, N_F_Length),
  N is N_I + N_F / 10 ** N_F_Length.
float_components(N, N_I, N_F):-
  float(N),
  float_integer_component(N, N_I),
  float_fractional_component(N, N_F).

float_fractional_component(N, N_F):-
  atom_number(N_A, N),
  sub_atom(N_A, N_I_Length, 1, _, '.'),
  succ(N_I_Length, Skip),
  sub_atom(N_A, Skip, _, 0, N_F_A),
  atom_number(N_F_A, N_F).

float_integer_component(N, I):-
  I is integer(float_integer_part(N)).

%! log(+Base:integer, +X:integer, -Y:double) is det.
% Logarithm with arbitrary base =|Y = log_{Base}(X)|=.
%
% @arg Base An integer.
% @arg X An integer.
% @arg Y A double.

log(Base, X, Y):-
  Numerator is log(X),
  Denominator is log(Base),
  Y is Numerator / Denominator.

minus(X, Y, Z):-
  nonvar(X),
  nonvar(Y),
  !,
  Z is X - Y.
minus(X, Y, Z):-
  nonvar(X),
  nonvar(Z),
  !,
  Y is X - Z.
minus(X, Y, Z):-
  nonvar(Y),
  nonvar(Z),
  !,
  X is Y + Z.

%! minus_list(+N:number, +Ms:list(number), -N_Minus_Ms:number) is det.
% Subtracts the given numbers for the given start number
% and returns the result.

minus_list(N, Ms, N_Minus_Ms):-
  sum_list(Ms, M),
  N_Minus_Ms is N - M.

%! multiply(+X:number, +Y:number, -Z:number) is det.
% Predicate alternative for the builtin multiplication function.
%
% @arg X A number.
% @arg Y A number.
% @arg Z A number.

multiply(X, Y, Z):-
  Z is X * Y.

%! multiply_list(+List:list(number), -Multiplication:number) is det.
% Multiplies the numbers in the given list.
%
% @arg List A list of numbers.
% @arg Multiplication A number.
% @see Extends the builin list manipulators sum_list/2, max_list/2
%      and min_list/2.

multiply_list([], 0):- !.
multiply_list([Number], Number):- !.
multiply_list([Number | Numbers], Multiplication):-
  multiply_list(Numbers, Multiplication1),
  Multiplication is Number * Multiplication1.

%! number_length(+Number:number, -Length:integer) is det.
% @see number_length/3 with radix set to `10` (decimal).

number_length(M, L):-
  number_length(M, 10.0, L).

%! number_length(+Number:number, +Radix:integer, -Length:integer) is det.
% Returns the length of the given number 'before the dot'.
% The number is in decimal notation.
%
% @arg An integer representing a decimal number.
% @arg Radix An integer representing the radix used.
%      Common values are `2` (binary), `8` (octal),
%      `10` (decimal), and `16` (hexadecimal).
% @arg Length An integer representing the number of digits in
%      the given number.

number_length(N1, Radix, L1):-
  N2 is N1 / Radix,
  N2 >= 1.0, !,
  number_length(N2, Radix, L2),
  L1 is L2 + 1.
number_length(_N, _Radix, 1):- !.

%! odd(?Integer:integer) is semidet.
% Succeeds if the integer is odd.
%
% @arg Integer An integer.

odd(Integer):-
  1 is Integer mod 2.

%! permutations(
%!   +NumbersOfObjects:list(integer),
%!   -NumberOfPermutations:integer
%! ) is det.
%! permutations(
%!   +NumberOfObjects:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
% Returns the number of permutations that can be created with
% the given number of distinct objects.
%
% @see permutations/3

permutations(NumbersOfObjects, NumberOfPermutations):-
  is_list(NumbersOfObjects),
  !,
  sum_list(NumbersOfObjects, NumberOfObjects),
  permutations(NumbersOfObjects, NumberOfObjects, NumberOfPermutations).
permutations(NumberOfObjects, NumberOfPermutations):-
  permutations([NumberOfObjects], NumberOfPermutations).

%! permutations(
%!   +NumbersOfObjects:list(integer),
%!   +PermutationLength:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
%! permutations(
%!   +NumberOfObjects:integer,
%!   +PermutationLength:integer,
%!   -NumberOfPermutations:integer
%! ) is det.
% Returns the number of permutations that can be created with
% the given numbers of distinct objects and that have (exactly)
% the given length.
%
% *Definition*: The number of permutations of _|m|_ groups of unique objects
%               (i.e., types) and with _|n_i|_ group members or occurences
%               (i.e., tokens), for $0 \leq i \leq m$ and that are (exactly)
%               of length _|r|_ is $\frac{n!}{\mult_{i = 1}^m(n_i!)(n - r)!}$.
%
% @arg NumbersOfObject A list of numbers, each indicating the number of
%        objects in a certain group.
% @arg PermutationLength The (exact) number of objects that occur
%        in a permutation.
% @arg NumberOfPermutations The number of permutations that can be created.

permutations(NumbersOfObjects, PermutationLength, NumberOfPermutations):-
  is_list(NumbersOfObjects),
  !,

  % The objects.
  sum_list(NumbersOfObjects, NumberOfObjects),
  factorial(NumberOfObjects, F1),

  % The length compensation.
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F3),

  % The groups.
  maplist(factorial, NumbersOfObjects, F2s),
  multiply_list([F3 | F2s], F23),

  NumberOfPermutations is F1 / F23.
permutations(NumberOfObjects, PermutationLength, NumberOfPermutations):-
  factorial(NumberOfObjects, F1),
  Compensation is NumberOfObjects - PermutationLength,
  factorial(Compensation, F2),
  NumberOfPermutations is F1 / F2.

%! plus_float(?X:number, ?Y:number, ?Z:number) is det.
% Calculates the sum Z = X + Y as long as at least two arguments are
% instantiated.
%
% @see The builin plus/3 only works for integers.

plus_float(X, Y, Z):-
  nonvar(X), nonvar(Y), !,
  Z is X + Y.
plus_float(X, Y, Z):-
  nonvar(X), nonvar(Z), !,
  Y is Z - X.
plus_float(X, Y, Z):-
  nonvar(Y), nonvar(Z), !,
  X is Z - Y.

%! pred(?Integer:integer, ?Predecessor:integer)
% A integer and its direct predecessor integer.
%
% This is used by meta-predicates that require uniform instantiation patterns.
%
% @arg Integer An integer.
% @arg Predecessor An integer.
% @see This extends the builin succ/2.

pred(Integer, Predecessor):-
  succ(Predecessor, Integer).

%! random_betwixt(+UpperLimit:number, -Random:float) is det.
% @see random_betwixt/3

random_betwixt(UpperLimit, Random):-
  integer(UpperLimit), !,
  math_ext:random_betwixt(0, UpperLimit, Random).
random_betwixt(UpperLimit, Random):-
  float(UpperLimit), !,
  math_ext:random_betwixt(0.0, UpperLimit, Random).

%! random_betwixt(
%!   +LowerLimit:number,
%!   +UpperLimit:number,
%!   -Random:number
%! ) is det.
% Returns a random floating point number between the given lower and
% upper limits, inclusive.
%
% @arg LowerLimit A number.
% @arg UpperLimit A number.
% @arg Random In case the lower and upper limits are integers, the
%	 return value is an integer as well. Otherwise it is a floating
%	 point number.
% @tbd Because we take the floor for the random value between two integers,
%      the chance that =UpperLimit= comes out is very much lower than all
%      the other values, i.e. =|[LowerLimit, UpperLimit)|=.

random_betwixt(LowerLimit, UpperLimit, Random):-
  integer(LowerLimit), integer(UpperLimit), !,
  random_betwixt_(LowerLimit, UpperLimit, Random0),
  Random is floor(Random0).
random_betwixt(LowerLimit, _UpperLimit, _Random):-
  \+ number(LowerLimit), !,
  type_error(number, LowerLimit).
random_betwixt(_LowerLimit, UpperLimit, _Random):-
  \+ number(UpperLimit), !,
  type_error(number, UpperLimit).
random_betwixt(LowerLimit, UpperLimit, Random):-
  random_betwixt_(LowerLimit, UpperLimit, Random).

random_betwixt_(LowerLimit, UpperLimit, Random):-
  Random is LowerLimit + random_float * (UpperLimit - LowerLimit).

%! random_coordinate(+Size:size, -Coordinate:coord) is det.
% Returns a random coordinate of the indicates dimension, i.e.,
% the dimension argument of the given size compound term.
%
% @arg Size A compound term specifying the dimension of the generated
%        coordinate, as well as the ceiling limit values for each dimension.
%        The floor limit value for every dimension is 0.0.
% @arg Coordinate A coordinate that has the same dimension as the given
%        size. Its second argument is a list of lists of floating point
%        values.

random_coordinate(size(Dimension, Sizes), coordinate(Dimension, Args)):-
  maplist(random_betwixt, Sizes, Args).

%! rbetween(?Min:integer, +Max:integer, ?Value:integer) is semidet.
% If `Min` and `Max` are given, `Value` is instantiated with `Max` and
% whith predecessor integers upon backtracking, until `Value` is `Min`.
%
% If only `Max` is given there is no lowest value for `Value`.

rbetween(Min, Max, Value):-
  nonvar(Max), (nonvar(Min) -> Min =< Max ; true),
  rbetween(Min, Max, Max, Value).

rbetween(Min, Value, _Max, Value):-
  nonvar(Min), Min == Value, !.
rbetween(_Min, Value, _Max, Value).
rbetween(Min, Between, Max, Value):-
  NewBetween is Between - 1,
  rbetween(Min, NewBetween, Max, Value).

%! square(+X:float, -Square:float) is det.
% Returns the square of the given number.

square(X, Square):-
  Square is X ** 2.
