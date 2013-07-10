:- module(
  typecheck,
  [
    typecheck/2, % +Type:compound
                 % +Value
    is_uri/1
  ]
).

/** <module> Type checking

Predicates used for parsing and checking value-type conformance.

@author Wouter Beek
@version 2013/01
*/



%! typecheck(+Type:compound, +Value) is semidet.
% Succeeds if the given value is of the given type.
%
% @arg Type A compound term representing a type.
% @arg Value

typecheck(or(Types), Value):-
  member(Type, Types),
  typecheck(Type, Value), !.
% Open numeric interval: open to the right.
typecheck(between(Min, Var), Value):-
  var(Var), !,
  (
    float(Min)
  ->
    float(Value)
  ;
    integer(Min)
  ->
    integer(Value)
  ),
  Min =< Value.
% Open numeric interval: open to the left.
typecheck(between(Var, Max), Value):-
  var(Var), !,
  (
    float(Max)
  ->
    float(Value)
  ;
    integer(Max)
  ->
    integer(Value)
  ),
  Max >= Value.
typecheck(Type, Value):-
  must_be(Type, Value), !.
% DCG defined types.
typecheck(Type, Value):-
  atom_chars(Value, ValueChars),
  Call =.. [Type, ValueChars, []],
  call(Call).

%! is_uri(?Resource:uri) is semidet.

is_uri(Resource):-
  uri_components(
    Resource,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  nonvar(Scheme),
  nonvar(Authority),
  nonvar(Path).
