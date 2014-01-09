:- module(
  json_ext,
  [
    json_boolean/2, % ?Prolog:boolean
                    % ?JSON:oneof([@(false),@(true)])
    json_rows/2, % +JSON:list
                 % -Rows:list
    json_to_rdf/3 % +Graph:atom
                  % +Module:atom
                  % +Term:compound
  ]
).

/** <module> JSON_EXT

@author Wouter Beek
@version 2013/07, 2013/11, 2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_lit_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).

:- meta_predicate(json_to_rdf(+,0)).



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


% JSON TO RDF %

% JSON list.
json_to_rdf(Graph, Module, List):-
  is_list(List), !,
  maplist(json_to_rdf(Graph, Module), List).
% JSON object.
json_to_rdf(Graph, Module, Term):-
  % DEB
  flag(aap, Id, Id + 1),
  format(current_output, '~d\n', [Id]),
  (Id = 999 -> gtrace ; true),
  
  % Namespace.
  (
    xml_current_namespace(Module, _), !
  ;
    atomic_list_concat(['http://www.wouterbeek.com',Module,''], '/', URL),
    xml_register_namespace(Module, URL)
  ),
  
  % Class.
  Term =.. [Name|_],
  once(dcg_phrase(capitalize, Name, ClassName)),
  rdf_global_id(Module:ClassName, Class),
  rdfs_assert_class(Class, Graph),
  
  % Individual.
  rdf_bnode(Individual),
  rdf_assert_individual(Individual, Class, Graph),
  
  % Propositions.
  json_convert:current_json_object(Term, Module, Fields),
  maplist(json_pair_to_rdf(Graph, Module, Individual), Fields).

json_pair_to_rdf(
  Graph,
  Module,
  Individual,
  f(PredicateName, Type, _Default, Value)
):-
  rdf_global_id(Module:PredicateName, Predicate),
  json_pair_to_rdf(Type, Individual, Predicate, Value, Graph).


%! json_pair_to_rdf(
%!   +DatatypeName:atom,
%!   +Individual:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value:or([bnode,iri,literal]),
%!   +Graph:atom
%! ) is det.
% @tbd Add list and object values.
% @tbd Prolog types and XSD types do not overlap,
%      e.g. the date and time datatypes.

% JSON null.
json_pair_to_rdf(_, _, _, @(null), _):- !.
% JSON empty string.
json_pair_to_rdf(_, _, _, '', _):- !.
% JSON non-empty string.
json_pair_to_rdf(atom, Individual, Predicate, Value, Graph):- !,
  rdf_assert_literal(Individual, Predicate, Value, Graph).
% Datatypes: boolean, float, integer.
json_pair_to_rdf(DatatypeName, Individual, Predicate, Value, Graph):-
  xsd_datatype(DatatypeName, Datatype), !,
  rdf_assert_datatype(Individual, Predicate, Datatype, Value, Graph).
% Intended for: `atom` or `@(null)`.
json_pair_to_rdf(any, Individual, Predicate, Value, Graph):-
  atom(Value), !,
  rdf_assert_literal(Individual, Predicate, Value, Graph).
% Others.
json_pair_to_rdf(DatatypeName, Individual, Predicate, Value, Graph):-
  format(
    current_output,
    'Datatype: ~w\nIndividual: ~w\nPredicate: ~w\nValue: ~w\nGraph: ~w\n',
    [DatatypeName,Individual,Predicate,Value,Graph]
  ).

