:- module(
  json_to_rdf,
  [
    json_to_rdf/5 % +Graph:atom
                  % +Module:atom
                  % +XML_Namespace:atom
                  % +JSON:compound
                  % -Individual:iri
  ]
).

/** <module> JSON to RDF

Automated JSON to RDF conversion.

This requires a Prolog module whose name is also registered as
 the XML namespace that is used for the RDF vocabulary.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)). % Meta-argument.
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_replace)). % Meta-argument.
:- use_module(generics(atom_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_image)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(json_ext)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).
:- use_module(xsd(xsd_clean)).



percent_encoding(space) -->
  percent_sign,
  integer(20).

arg_spec_match(Args, ArgSpecs, Length):-
  maplist(arg_to_name, Args, Names1),
  maplist(arg_spec_to_name, ArgSpecs, Names2),
  ord_intersection(Names1, Names2, Shared),
  length(Shared, Length).
arg_spec_to_name(Name-_-_, Name).
arg_to_name(Name=_, Name).


%! create_resource(
%!   +Graph:atom,
%!   +XML_Namespace:atom,
%!   +Legend:atom,
%!   ?Id:atom,
%!   -Individual:iri
%! ) is det.
% @arg Graph
% @arg Module The atomic name of a Prolog module containing
%      legend declarations and the name of a registered XML namespace.
% @arg Legend The atomic name of the legend.
%      This is used to construct the IRI that denotes the RDFS class.
% @arg Id If the id is not instantiated, then the individual is
%      denoted by a blank node; otherwise it is denoted by an IRI.
% @arg Individual

create_resource(Graph, XML_Namespace, Legend, Id, Individual):-
  once(dcg_phrase(capitalize, Legend, ClassName)),
  rdf_global_id(XML_Namespace:ClassName, Class),
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


%! json_to_rdf(
%!   +Graph:atom
%!   +Module:atom
%!   +XML_Namespace:atom,
%!   +JSON:compound
%!   -Individual:iri
%! ) is det.
% Automated conversion from JSON to RDF,
%  based on registered legends.
%
% # Conversion table
%
% | *JSON*     | *Prolog*      | * RDF*        |
% | Term       | Compound term | Resource      |
% | Array      | List          |               |
% | String     | Atom          | `xsd:string`  |
% |            | Atom          | Resource      |
% | Number     | Number        | `xsd:float`   |
% |            |               | `xsd:integer` |
% | `false`    | `@(false)`    | `xsd:boolean` |
% | `true`     | `@(true)`     | `xsd:boolean` |
% | `null`     | `@(null)`     | skip          |
%
% # Argument descriptions
%
% @arg Graph The atomic name of the RDF graph in which results are asserted.
% @arg LegendModule The atomic name of the Prolog module that contains
%      the legens to which JSON terms have to conform.
% @arg JSON A compound term representing a JSON term.
%      This will be converted to RDF.
% @arg Individual An IRI denoting the RDF version of the JSON term.

json_to_rdf(Graph, Module, XML_Namespace, JSONs, Individuals):-
  is_list(JSONs), !,
  maplist(json_to_rdf(Graph, Module, XML_Namespace), JSONs, Individuals).
json_to_rdf(Graph, Module, XML_Namespace, JSON, Individual):-
 % Namespace.
  (
    xml_current_namespace(XML_Namespace, _), !
  ;
    atomic_list_concat(['http://www.wouterbeek.com/',Module,'#'], '', URL),
    xml_register_namespace(XML_Namespace, URL)
  ),
  json_object_to_rdf(Graph, Module, XML_Namespace, JSON, Individual).

json_object_to_rdf(Graph, Module, XML_Namespace, JSON, Individual):-
  JSON = json(Args0),

  % Find the legend to which this JSON term conforms.
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

  json_object_to_rdf(Graph, Module, XML_Namespace, Legend, json(Args), Individual).


% Now we have a legend based on which we do the conversion.
json_object_to_rdf(Graph, Module, XML_Namespace, Legend, json(Args1), Individual):-
  Module:legend(Legend, PrimaryKey, Spec),

  (nonvar(PrimaryKey) -> memberchk(PrimaryKey=Id, Args1) ; true),

  % Class and individual.
  (
    var(Individual)
  ->
    create_resource(Graph, XML_Namespace, Legend, Id, Individual)
  ;
    true
  ),

  % Propositions.
  maplist(
    json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Spec),
    Args1
  ).


%! json_pair_to_rdf(
%!   +Graph:atom,
%!   +Module:atom,
%!   +XML_Namespace:atom,
%!   +Individual:iri,
%!   +ArgumentSpecification:compound,
%!   +JSON:pair(atom,term)
%! ) is det.
% Make sure a property with the given name exists.
% Also retrieve the type the value should adhere to.

json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Spec, Name=Value):-
  memberchk(Name-Type-_, Spec),
  json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Name, Type, Value), !.
% DEB
json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Spec, Name=Value):-
  gtrace, %DEB
  json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Spec, Name=Value).

% The value must match at least one of the given types.
json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Name, or(Types), Value):-
  % Notice the choicepoint.
  member(Type, Types),
  json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Name, Type, Value), !.
% We do not have an RDF equivalent for the JSON null value,
% so we do not assert pairs with a null value in RDF.
json_pair_to_rdf(_, _, _, _, _, _, Value):-
  Value = @(null), !.
% We do not believe that empty values -- i.e. the empty atom --
% are very usefull, so we do not assert pairs with this value.
json_pair_to_rdf(_, _, _, _, _, _, ''):- !.
% We have a specific type that is always skipped, appropriately called `skip`.
json_pair_to_rdf(_, _, _, _, _, skip, _):- !.
% There are two ways to realize legend types / create resources:
% 1. JSON terms (always).
json_pair_to_rdf(Graph, Module, XML_Namespace, Individual1, Name, Legend/_, Value):-
  Value = json(_), !,
  json_object_to_rdf(Graph, Module, XML_Namespace, Legend, Value, Individual2),
  rdf_global_id(XML_Namespace:Name, Predicate),
  rdf_assert(Individual1, Predicate, Individual2, Graph).
% There are two ways to realize legend types / create resources:
% 2. JSON strings (sometimes).
json_pair_to_rdf(Graph, Module, XML_Namespace, Individual1, Name, Legend/_, Value):-
  atom(Value), !,
  create_resource(Graph, Module, Legend, Value, Individual2),
  rdf_global_id(XML_Namespace:Name, Predicate),
  rdf_assert(Individual1, Predicate, Individual2, Graph).
% A JSON object occurs for which the legend it not yet known.
json_pair_to_rdf(Graph, Module, XML_Namespace, Individual1, Name, Type, Value):-
  Type \= _/_, Value = json(_), !,
  json_object_to_rdf(Graph, Module, XML_Namespace, Value, Individual2),
  rdf_global_id(XML_Namespace:Name, Predicate),
  rdf_assert(Individual1, Predicate, Individual2, Graph).
% Prolog list.
json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Name, list(Type), Values):-
  is_list(Values), !,
  maplist(
    json_pair_to_rdf(Graph, Module, XML_Namespace, Individual, Name, Type),
    Values
  ).
% RDF list.
json_pair_to_rdf(Graph, _, XML_Namespace, Individual, Name, rdf_list(Type), Values):-
  is_list(Values), !,
  rdf_global_id(XML_Namespace:Name, Predicate),
  rdf_global_id(xsd:Type, Datatype),
  rdf_assert_list([datatype(Datatype)], Values, RDF_List, Graph),
  rdf_assert(Individual, Predicate, RDF_List, Graph).
% XSD
json_pair_to_rdf(Graph, _, XML_Namespace, Individual, Name, DatatypeName, Value1):-
  rdf_global_id(XML_Namespace:Name, Predicate),
  % Convert the JSON value to an RDF object term.
  % This is where we validate that the value is of the required type.
  xsd_datatype(DatatypeName, Datatype),
  xsd_value(Datatype, Value1, Value2),
  rdf_assert_datatype(Individual, Predicate, Value2, Datatype, Graph).

