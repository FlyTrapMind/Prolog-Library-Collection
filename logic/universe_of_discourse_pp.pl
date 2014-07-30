:- module(
  universe_of_discourse_pp,
  [
    print_universe/0,
    print_universe_of_discourse/0
  ]
).

/** <module> Universe of Discourse: pretty-print

Printing for module [universe_of_discourse].

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(lists)).

:- use_module(dcg(dcg_collection)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_list)).

:- use_module(generics(codes_ext)).
:- use_module(logic(universe_of_discourse)).



%! object(+ObjectMap:list(pair), +Object)// is det.

object(ObjectMap, Object) -->
  {memberchk(Object-Name, ObjectMap)},
  atom(Name), `: `, atom(Object).


%! object_map(+Objects:list, -ObjectMap:list(pair)) is det.

object_map(Objects, ObjectMap):-
  flag(object, Flag, Flag + 1),
  findall(
    Object-Name,
    (
      nth1(I, Objects, Object),
      atomic_list_concat([o,I], Name)
    ),
    ObjectMap
  ).


%! object_name(+ObjectMap:list(pair), +Object)// is det.

object_name(ObjectMap, Object) -->
  {memberchk(Object-Name, ObjectMap)},
  atom(Name).


%! print_universe is det.
% Prints a pretty representation of the current universe to current output.

print_universe:-
  phrase(universe, Codes),
  put_codes(Codes).


%! print_universe_of_discourse// is det.
% Prints a pretty representation fo the current universe of discourse
% to current output.

print_universe_of_discourse:-
  phrase(universe_of_discourse, Codes),
  put_codes(Codes).


%! relation(
%!   +Indent:nonneg,
%!   +ObjectMap:list(pair),
%!   +Relation:atom,
%!   +Tuples:list(list)
%! )// is det.

relation(Indent1, ObjectMap, Relation, Tuples) -->
  indent(Indent1), atom(Relation), nl,
  {Indent2 is Indent1 + 1},
  predicate_tuples(Indent2, ObjectMap, Tuples).

%! relations(
%!   +Indent:nonneg,
%!   +ObjectMap:list(pair),
%!   +Relations:list(pair(atom,list(list)))
%! )// is det.

relations(_, _, []) --> !, [].
relations(Indent, ObjectMap, [Relation-Tuples|Pairs]) -->
  relation(Indent, ObjectMap, Relation, Tuples), nl,
  relations(Indent, ObjectMap, Pairs).


%! predicate_tuple(
%!   +Indent:nonneg,
%!   +ObjectMap:list(pair),
%!   +Tuple:list
%! )// is det.

predicate_tuple(Indent, ObjectMap, Tuple) -->
  indent(Indent), tuple(ascii, object_name(ObjectMap), Tuple), nl.

%! predicate_tuples(
%!   +Indent:nonneg,
%!   +Object:list(pair),
%!   +Tuples:list(list)
%! )// is det.

predicate_tuples(_, _, []) --> !, [].
predicate_tuples(Indent, ObjectMap, [Tuple|Tuples]) -->
  predicate_tuple(Indent, ObjectMap, Tuple), nl,
  predicate_tuples(Indent, ObjectMap, Tuples).


%! universe// is det.

universe -->
  universe(0).

%! universe(+Indent:nonneg)// is det.

universe(Indent) -->
  {universe(Universe)},
  universe(Indent, Universe).

universe(Indent1, universe(Objects,RelationPairs)) -->
  {Indent2 is Indent1 + 1},
  indent(Indent1), `Universe:`, nl,
  universe_of_discourse(Indent2, ObjectMap, Objects),
  indent(Indent2), `Relations:`, nl,
  {Indent3 is Indent2 + 1},
  relations(Indent3, ObjectMap, RelationPairs).


%! universe_of_discourse// is det.

universe_of_discourse -->
  universe_of_discourse(0, _).

%! universe_of_discourse(+Indent:nonneg, +ObjectMap:list(pair))// is det.

universe_of_discourse(Indent, ObjectMap) -->
  {universe_of_discourse(Objects)},
  universe_of_discourse(Indent, ObjectMap, Objects).

%! universe_of_discourse(
%!   +Indent:nonneg,
%!   +ObjectMap:list(pair),
%!   +Objects:list
%! )// is det.

universe_of_discourse(Indent, ObjectMap, Objects) -->
  {object_map(Objects, ObjectMap)},
  indent(Indent),
  `Universe of discourse:`, nl,
  dcg_list(Objects, [element_writer(object(ObjectMap)),indent(Indent)]).

