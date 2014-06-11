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
@version 2013/04, 2013/12-2014/01, 2014/06
*/

:- use_module(library(uri)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(sparql(sparql_api)).
:- use_module(sparql(sparql_db)).
:- use_module(xml(xml_namespace)).

:- initialization(init_bag).
init_bag:-
  xml_register_namespace(bag,  'http://lod.geodan.nl/BAG/'),
  xml_register_namespace(bags, 'http://lod.geodan.nl/BAG-schema/'),
  uri_components(Url, uri_components(http,'lod.geodan.nl','/BAG/sparql',_,_)),
  sparql_register_endpoint(bag, query, Url).



test:-
  sparql_select(bag, _, [bags], true, [],
      [rdf(var(s),vocab:pand_status,var(o))], 10, _, _, Resources, []),
  dcg_with_output_to(current_user, list(pl_term, Resources)).

