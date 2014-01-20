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
@version 2013/04, 2013/12
*/

:- use_module(generics(print)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(bag).
:- sparql_add_prefix(bag).
:- sparql_add_remote(bag, 'lod.geodan.nl', '/BAG/sparql').

:- xml_register_namespace(bags).
:- sparql_add_prefix(bags).



test:-
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [bags],
      select,
      true,
      [],
      [rdf(var(s), vocab:pand_status, var(o))],
      10,
      _
    ),
    Query
  ),
  'SPARQL_enqueue'(bag, Query, _VarNames, Resources),
  with_output_to(current_user, print_list([], Resources)).

