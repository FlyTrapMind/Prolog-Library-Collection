:- module(
  datetime_std,
  [
  ]
).

/** <module> DATETIME_STD

Overview of the RFCs that define DateTime.

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(standards)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(iso, 'http://www.iso.org/').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').

:- initialization(init_lang_ext).



init_lang_ext:-
  standards_graph(G),
  init_iso8601(G),
  init_rfc1123(G).

init_iso8601(G):-
  rdf_global_id(iso:'8601-3', This),
  rdfs_assert_class(iso:'Standard', G),
  rdf_assert_individual(This, iso:'Standard', G),
  rdf_assert_datatype(This, iso:year, gYear, 2004, G),
  rdf_assert_datatype(This, iso:edition, int, 3, G),
  rdf_assert_literal(
    This,
    iso:title,
    en,
    'Data elements and interchange formats – Information interchange – Representation of dates and times',
    G
  ),
  rdf_assert(This, iso:obsoletes, iso:'2014', G),
  rdf_assert(This, iso:obsoletes, iso:'2015', G),
  rdf_assert(This, iso:obsoletes, iso:'2711', G),
  rdf_assert(This, iso:obsoletes, iso:'3307', G),
  rdf_assert(This, iso:obsoletes, iso:'4031', G),
  rdf_assert(This, iso:obsoletes, iso:'8601-1', G),
  rdf_assert(This, iso:obsoletes, iso:'8601-2', G).

init_rfc1123(G):-
  rdf_global_id(rfc:'1123', This),
  rdfs_assert_class(rfc:'Standard', G),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 1989, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Requirements for Internet Hosts -- Application and Support',
    G
  ),
  rdf_assert_literal(This, rfc:editor, en, 'R. Braden', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc1123.txt', G).

