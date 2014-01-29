:- module(
  random_ext,
  [
    random_betwixt/2, % +UpperLimit:number
                      % -Random:number
    random_betwixt/3, % +LowerLimit:number
                      % +UpperLimit:number
                      % -Random:number
    random_coordinate/2 % +Size:size,
                        % -Coordinate:coordinate
  ]
).

/** <module> RANDOM_EXT

Extra support for random values.

@author Wouter Beek
@version 2011/08-2012/02, 2012/09-2012/10, 2012/12, 2013/07-2013/08
*/

:- use_module(library(apply)).
:- use_module(library(error)).



%! random_betwixt(+UpperLimit:number, -Random:float) is det.
% @see random_betwixt/3

random_betwixt(UpperLimit, Random):-
  integer(UpperLimit), !,
  random_betwixt(0, UpperLimit, Random).
random_betwixt(UpperLimit, Random):-
  float(UpperLimit), !,
  random_betwixt(0.0, UpperLimit, Random).

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
  maplist(integer, [LowerLimit,UpperLimit]), !,
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

