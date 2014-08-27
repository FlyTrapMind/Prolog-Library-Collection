:- module(bag,
  [
    test/0
  ]
).

/** <module> BAG

The Dutch base registration for buildings.

http://lod.geodan.nl/BAG/sparql?query=SELECT+DISTINCT+*+WHERE+{%0D%0A++%3Fs+%3Fp+%3Fo%0D%0A}%0D%0ALIMIT+1

info@geodan.nl

@author Wouter Beek
@version 2013/04, 2013/12-2014/01, 2014/06, 2014/08
*/

:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generic)).

:- use_module(plSparql(sparql_db)).
:- use_module(plSparql_query(sparql_query_api)).

:- initialization(init_bag).



test:-
  sparql_select(
    bag,
    [vocab],
    [s,o],
    [rdf(var(s),vocab:pand_status,var(o))],
    Rows,
    [distinct(true),limit(10)]
  ),
  maplist(\Row^dcg_with_output_to(current_output, list(pl_term, Row)), Rows).



% Initialization.

init_bag:-
  rdf_register_prefix(bag,  'http://lod.geodan.nl/BAG/'),
  rdf_register_prefix(bags, 'http://lod.geodan.nl/BAG-schema/'),
  sparql_register_endpoint(
    bag,
    uri_components(http,uri_authority(_,_,'lod.geodan.nl',_),'/BAG/',_,_),
    virtuoso
  ).

