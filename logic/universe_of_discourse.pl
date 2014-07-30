:- module(
  universe_of_discourse,
  [
    add_object/1, % +Object
    add_relation/2, % +Relation:atom
                    % +Tuples:list(list)
    add_relation_tuple/2, % ?Relation:atom
                          % +Tuple:list
    object/1, % ?Object
    relation/1, % ?Relation:atom
    relation/2, % ?Relation:atom
                % ?Tuples:ordset(list)
    relation_tuple/2, % ?Relation:atom
                      % ?Tuple:list
    universe_of_discourse/1 % -Objects:ordset
  ]
).

/** <module> Universe of discourse

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists), except([subset/2])).
:- use_module(library(uuid)).

:- use_module(generics(db_ext)).
:- use_module(logic(formal_language)).
:- use_module(logic(set_theory)).

:- dynamic(object/1).

:- dynamic(relation_tuple/2).



%! add_object(+Object) is det.
%! add_object(-Object) is det.
% Adds an object to the domain of discource.
%
% Succeeds if the given object already exists.

add_object(Object):-
  var(Object), !,
  uuid(Object).
add_object(Object):-
  db_add_novel(object(Object)).


%! add_relation(+Relation:atom, +Tuples:list(list)) is det.

add_relation(Relation, Tuples):-
  maplist(add_relation(Relation), Tuples).

%! add_relation_tuple(+Relation:atom, +Tuple:list) is det.
%! add_relation_tuple(-Relation:atom, +Tuple:list) is det.
% Adds a tuple to a new or existing relation.
%
% Succeeds if the object tuple is already part of the predicate extension.
%
% @arg Relation A UUID, probably identifying the relation correctly.
% @arg Tuple A list of objects.
%      An object can be any syntactically correct Prolog expression.
%
% @throws arity_mismatch if the arity of `Relation`
%         and the length of `Tuple` are not the same.
% @throws domain_error if `Tuple` is the empty list.
% @throws instantiation_error if `Tuple` is uninstantiated.
% @throws type_error if `Tuple` is not a list.

% A new relation: generate a UUID for identifying it.
add_relation_tuple(Relation, Tuple):-
  var(Relation), !,
  uuid(Relation),
  add_relation_tuple(Relation, Tuple).
% Error: Tuple uninstantiated.
add_relation_tuple(_, Tuple):-
  var(Tuple), !,
  instantiation_error(Tuple).
% Error: Tuple tuple not a list.
add_relation_tuple(_, Tuple):-
  \+ is_list(Tuple), !,
  type_error(list, Tuple).
% Error: Mismatch between existing relation arity and object tuple length.
add_relation_tuple(Relation, Tuple):-
  relation(Relation, Objects0),
  \+ equinumerous(Tuple, Objects0), !,
  throw(arity_mismatch(Relation)).
% Only add if novel.
add_relation_tuple(Relation, Tuple):-
  % Make sure the objects are part of the universe of discourse.
  maplist(add_object, Tuple),
  db_add_novel(relation(Relation, Tuple)).


%! relation(+Relation:atom) is semidet.
%! relation(-Relation:atom) is nondet.

relation(Relation):-
  % Ensure that no duplicate results are returned.
  aggregate_all(
    set(Relation),
    relation(Relation, _),
    Relations
  ),
  % Choicepoint, iterating over the potentially multiple outputs.
  member(Relation, Relations).


%! relation(+Relation:atom, -Tuples:ordset(list)) is det.
%! relation(-Relation:atom, -Tuples:ordset(list)) is nondet.

relation(Relation, Tuples):-
  % Enumeration choicepoint for instantiation `(-,-)`.
  (
    var(Relation)
  ->
    relation(Relation)
  ;
    true
  ),
  
  % Collect the tuples for a given relation.
  aggregate_all(
    set(Tuple),
    relation_tuple(Relation, Tuple),
    Tuples
  ).


%! relation_tuple(+Relation:atom, +Tuple:list) is semidet.
%! relation_tuple(+Relation:atom, -Tuple:list) is nondet.
%! relation_tuple(-Relation:atom, +Tuple:list) is nondet.
%! relation_tuple(-Relation:atom, -Tuple:list) is nondet.
% Dynamic assertions of tuples comprising a relation.
%
% The order in which the tuples are returned is arbitrary.


% universe_of_discourse(-Objects:ordset) is det.

universe_of_discourse(Objects):-
  aggregate_all(
    set(Object),
    object(Object),
    Objects
  ).



% Messages.

:- multifile(prolog:message//1).

prolog:message(arity_mismatch(Relation)) -->
  ['The object tuple length is not relation ~a\'s arity.'-[Relation]].



% Debug.

load_uod1([O1,O2,O3], [R1,R2]):-
  maplist(add_object, [O1,O2,O3]),
  maplist(add_relation, [R1,R2], [[[O2]],[[O1,O2],[O1,O3],[O2,O2],[O3,O1]]]).

