:- module(
  json_ext,
  [
    json_boolean/2, % ?Prolog:boolean
                    % ?JSON:oneof([@(false),@(true)])
    json_rows/2, % +JSON:list
                 % -Rows:list
    json_to_prolog/3 % +Module:atom
                     % +JSON:compond
                     % -Term:compound
  ]
).

/** <module> JSON_EXT

@author Wouter Beek
@version 2013/07, 2013/11, 2014/01
*/

:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).



% JSON BOOLEAN %

json_boolean(false, @(false)).
json_boolean(true, @(true)).



% JSON TABLE %

json_header_row([json(L1)|_], L2):-
  maplist(json_name, L1, L2).

json_name(N=_, N).

json_row(json(L1), L2):-
  maplist(json_value, L1, L2).

%! json_rows(+JSON:list, -Rows:list) is det.
% Converts a list of JSON objects to (HTML) table rows.

json_rows(JSON, [HeaderRow|DataRows]):-
  json_header_row(JSON, HeaderRow),
  maplist(json_row, JSON, DataRows).

json_value(_=V, V).



% JSON TO PROLOG %

arg_spec_match(Args, ArgSpecs, Length):-
  maplist(arg_to_name, Args, Names1),
  maplist(arg_spec_to_name, ArgSpecs, Names2),
  ord_intersection(Names1, Names2, Shared),
  length(Shared, Length).
arg_to_name(Name=_, Name).
arg_spec_to_name(Name-_-_, Name).

json_to_prolog(_, JSON, Term):-
  JSON = @(Term), !.
json_to_prolog(_, Term, Term):-
  atom(Term), !.
json_to_prolog(_, Term, Term):-
  integer(Term), !.
json_to_prolog(Module, JSONs, Terms):-
  is_list(JSONs), !,
  findall(
    Term,
    (
      member(JSON, JSONs),
      json_to_prolog(Module, JSON, Term)
    ),
    Terms
  ).
json_to_prolog(Module, JSON, Term):-
  json_object_to_prolog(Module, JSON, Term).

json_object_to_prolog(Module, json(Args0), Term):-
  sort(Args0, Args),
  findall(
    Length-Legend,
    (
      Module:legend(Legend, _, ArgSpecs),
      arg_spec_match(Args, ArgSpecs, Length)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Values),
  debug(json_ext, 'Legend order found: ~w.', [Values]),
  last(Values, Legend),
  json_object_to_prolog(Module, Legend, json(Args), Term).

json_object_to_prolog(Module, Legend, json(Args1), Term):-
  Module:legend(Legend, _, Specs),
  maplist(json_pair_to_prolog(Module, Legend, Specs), Args1, Args2),
  Term =.. [Legend|Args2].

%! json_pair_to_prolog(
%!   +Module:atom,
%!   +Legend:atom,
%!   +ArgumentSpecification:compound,
%!   +JSON:pair(atom,term),
%!   -Prolog:pair(atom,term)
%! ) is det.

% JSON null value.
json_pair_to_prolog(_, _, Specs, Name=Null, _VAR):-
  Null = @(null), !,
  memberchk(Name-_-true, Specs).
% Empty string.
json_pair_to_prolog(_, _, Specs, Name='', _VAR):- !,
  memberchk(Name-_-true, Specs).
json_pair_to_prolog(Module, _, Specs, Name=Value1, Value2):-
  memberchk(Name-Type-_, Specs),
  catch(
    json_value_to_prolog(Module, Type, Value1, Value2),
    Exception,
    debug(email, '[pl2rdf] Exception: ~w', [Exception])
  ), !.
% DEB
json_pair_to_prolog(Graph, Legend, Type, Pair, Value):-
  gtrace,
  debug(json_ext, 'Legend: ~w\tType: ~w\tPair: ~w', [Legend,Type,Pair]),
  json_pair_to_prolog(Graph, Legend, Type, Pair, Value).

json_value_to_prolog(_, skip, _, _VAR):- !.
json_value_to_prolog(Module, Legend/_, Value1, Value2):-
  Value1 = json(_), !,
  (
    var(Legend)
  ->
    json_object_to_prolog(Module, Value1, Value2)
  ;
    json_object_to_prolog(Module, Legend, Value1, Value2)
  ).
% Internal links. These are only handled in the conversion to RDF.
json_value_to_prolog(Module, _/_, Value1, Value2):- !,
  json_value_to_prolog(Module, atom, Value1, Value2).
json_value_to_prolog(Module, or(Types), Value1, Value2):-
  member(Type, Types),
  json_value_to_prolog(Module, Type, Value1, Value2), !.
json_value_to_prolog(_, atom, Value, Value):-
  atom(Value), !.
json_value_to_prolog(_, boolean, Value1, Value2):-
  to_boolean(Value1, Value2), !.
json_value_to_prolog(_, url, Value1, Value2):- !,
  (
    is_of_type(iri, Value1)
  ->
    Value2 = Value1
  ;
    atomic_concat('http://', Value1, Value2),
    is_of_type(iri, Value2)
  ->
    true
  ;
    format(atom(Msg), 'Value ~w is not a URL.', [Value1]),
    syntax_error(Msg)
  ).
json_value_to_prolog(_, integer, Value1, Value2):-
  to_integer(Value1, Value2), !.
json_value_to_prolog(_, dateTime, Value1, Value2):- !,
  parse_time(Value1, iso_8601, Value2).
json_value_to_prolog(Module, list(Type), Value1, Value2):-
  is_list(Value1),
  maplist(json_value_to_prolog(Module, Type), Value1, Value2).

% Prolog native.
to_boolean(true, true).
to_boolean(false, false).
% Prolog DSL for JSON.
to_boolean(@(true), true).
to_boolean(@(false), false).
% Integer boolean.
to_boolean(1, true).
to_boolean(0, false).
% CKAN boolean.
to_boolean('True', true).
to_boolean('False', false).

to_integer(Value, Value):-
  integer(Value), !.
to_integer(Value1, Value2):-
  atom_number(Value1, Value3),
  to_integer(Value3, Value2).
