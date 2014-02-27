:- module(
  opendefinition_licenses,
  [
    enrich_license/2, % +License:iri
                      % +Graph:atom
    enrich_licenses/1 % +Graph:atom
  ]
).

/** <module> OpenDefinition Licenses

Support for the OpenDefinition licenses and their descriptions.

@author Wouter Beek
@see http://licenses.opendefinition.org/
@version 2014/02
*/

:- use_module(ckan(ckan)). % Legen declarations.
:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_term)).
:- use_module(rdf_conv(json_to_rdf)).



enrich_license(Graph, License):-
  license_descriptions(JSON_Descriptions),
  enrich_license(JSON_Descriptions, Graph, License).

enrich_license(JSON_Descriptions, Graph, License):-
  rdf_global_id(_:LocalName, License),
  atomic_list_concat([_,Tmp], '/', LocalName),
  downcase_atom(Tmp, Key),
  memberchk(Key=JSON_Description, JSON_Descriptions), !,
  json_to_rdf(temp, ckan, ckan, JSON_Description, Dummy),
  forall(
    rdf(Dummy, P, O, temp),
    rdf_assert(License, P, O, Graph)
  ),
  rdf_unload_graph(temp).
enrich_license(_, Graph, License):-
  rdf_assert_datatype(
    License,
    rdf:comment,
    xsd:string,
    'Not described by OpenDefinition.',
    Graph
  ),
  debug(
    ckan,
    'Could not find license ~w in OpenDefinition descriptions.',
    [License]
  ).


enrich_licenses(Graph):-
  license_descriptions(JSON_Descriptions),
  setoff(
    License,
    (
      rdfs_individual_of(License, ckan:'License'),
      rdf_term(Graph, License)
    ),
    Licenses
  ),
  maplist(enrich_license(JSON_Descriptions, Graph), Licenses).


license_descriptions(Reply):-
  URL = 'http://licenses.opendefinition.org/licenses/groups/all.json',
  http_open(URL, Stream, []),
  json_read(Stream, json(Reply)).

