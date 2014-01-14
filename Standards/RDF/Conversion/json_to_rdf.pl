:- module(
  json_to_rdf,
  [
    json_to_rdf/4 % +Graph:atom
                  % +Module:atom
                  % +JSON:compound
                  % -Individual:iri
  ]
).

/** <module> JSON to RDF

Automated JSON to RDF conversion.

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(codes_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_lit_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).



arg_spec_match(Args, ArgSpecs, Length):-
  maplist(arg_to_name, Args, Names1),
  maplist(arg_spec_to_name, ArgSpecs, Names2),
  ord_intersection(Names1, Names2, Shared),
  length(Shared, Length).
arg_spec_to_name(Name-_-_, Name).
arg_to_name(Name=_, Name).

json_to_rdf(Graph, Module, JSONs, Individuals):-
  is_list(JSONs), !,
  findall(
    Individual,
    (
      member(JSON, JSONs),
      json_to_rdf(Graph, Module, JSON, Individual)
    ),
    Individuals
  ).
json_to_rdf(Graph, Module, JSON, Individual):-
  JSON = json(_), !,

  % Namespace.
  (
    xml_current_namespace(Module, _), !
  ;
    atomic_list_concat(['http://www.wouterbeek.com/',Module,'#'], '', URL),
    xml_register_namespace(Module, URL)
  ),

  json_object_to_rdf(Graph, Module, JSON, Individual).
json_to_rdf(_, _, JSON, _):-
  debug(json_to_rdf, 'No proposition for JSON term ~w.', [JSON]).

json_object_to_rdf(Graph, Module, json(Args0), Individual):-
  % Find the most popular legend.
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
  pairs_values(Pairs2, Legends),
  debug(json_to_rdf, 'Legend order found: ~w.', [Legends]),
  last(Legends, Legend),

  json_object_to_rdf(Graph, Module, Legend, json(Args), Individual).

create_resource(Graph, Module, Legend, Id, Individual):-
  once(dcg_phrase(capitalize, Legend, ClassName)),
  rdf_global_id(Module:ClassName, Class),
  rdfs_assert_class(Class, Graph),
  (
    var(Id)
  ->
    rdf_bnode(Individual)
  ;
    atomic_list_concat([ClassName,Id], '/', IndividualName),
    rdf_global_id(ckan:IndividualName, Individual)
  ),
  rdf_assert_individual(Individual, Class, Graph).

json_object_to_rdf(Graph, Module, Legend, json(Args1), Individual):-
  Module:legend(Legend, PrimaryKey, Specs),

  (nonvar(PrimaryKey) -> memberchk(PrimaryKey=Id, Args1) ; true),

  % Class and individual.
  create_resource(Graph, Module, Legend, Id, Individual),

  % Propositions.
  maplist(json_pair_to_rdf(Graph, Module, Legend, Individual, Specs), Args1).

%! json_pair_to_rdf(
%!   +Graph:atom,
%!   +Module:atom,
%!   +Legend:atom,
%!   +Individual:iri,
%!   +ArgumentSpecification:compound,
%!   +JSON:pair(atom,term),
%!   -Prolog:pair(atom,term)
%! ) is det.

json_pair_to_rdf(_, _, _, _, Specs, Name=Null):-
  Null = @(null), !,
  memberchk(Name-_-true, Specs).
json_pair_to_rdf(Graph, Module, _, Individual, Specs, Name=Value):-
  memberchk(Name-Type-_, Specs),
  rdf_global_id(Module:Name, Predicate),
  catch(
    json_value_to_rdf(Graph, Module, Individual, Predicate, Type, Value),
    Exception,
    debug(email, '[json2rdf] Exception: ~w', [Exception])
  ), !.
% DEB
json_pair_to_rdf(Graph, Module, Legend, Type, Specs, Pair):-
  gtrace,
  debug(json_to_rdf, 'Legend: ~w\tType: ~w\tPair: ~w', [Legend,Type,Pair]),
  json_pair_to_rdf(Graph, Module, Legend, Type, Specs, Pair).

% Type `skip`.
json_value_to_rdf(_, _, _, _, skip, _):- !.
% The empty atom.
json_value_to_rdf(_, _, _, _, _, ''):- !.
% A JSON object occurs that should be asserted
%  according to the given legend.
json_value_to_rdf(Graph, Module, Individual1, Predicate, Legend/_, Value):-
  Value = json(_), !,
  (
    var(Legend)
  ->
    json_object_to_rdf(Graph, Module, Value, Individual2)
  ;
    json_object_to_rdf(Graph, Module, Legend, Value, Individual2)
  ),
  rdf_assert(Individual1, Predicate, Individual2, Graph).
% An atom occurs that is the identifier of an object described by `Legend`.
json_value_to_rdf(Graph, Module, Individual1, Predicate, Legend/_, Value):- !,
  create_resource(Graph, Module, Legend, Value, Individual2),
  rdf_assert(Individual1, Predicate, Individual2, Graph).
json_value_to_rdf(Graph, Module, Individual, Predicate, or(Types), Value):-
  % Notice the choicepoint...
  member(Type, Types),
  json_value_to_rdf(Graph, Module, Individual, Predicate, Type, Value), !.
% Atom.
json_value_to_rdf(Graph, _, Individual, Predicate, atom, Value):-
  atom(Value), !,
  rdf_assert_literal(Individual, Predicate, Value, Graph).
% URL.
json_value_to_rdf(Graph, _, Individual, Predicate, url, Value1):-
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
  ),
  rdf_assert(Individual, Predicate, Value2, Graph).
% List.
json_value_to_rdf(Graph, Module, Individual, Predicate, list(Type), Value):-
  is_list(Value), !,
  maplist(
    json_value_to_rdf(Graph, Module, Individual, Predicate, Type),
    Value
  ).
% The value is already in XSD format: recognize that this is the case.
json_value_to_rdf(Graph, Module, Individual, Predicate, Type, Value1):-
  atom(Value1),
  to_codes(Value1, LEX1),
  xsd_datatype(Type, Datatype),
  xsd_lexicalMap(Datatype, LEX1, Value2), !,
  json_value_to_rdf(Graph, Module, Individual, Predicate, Type, Value2).
json_value_to_rdf(Graph, _, Individual, Predicate, Type, Value):-
  xsd_datatype(Type, Datatype), !,
  rdf_assert_datatype(Individual, Predicate, Datatype, Value, Graph).

