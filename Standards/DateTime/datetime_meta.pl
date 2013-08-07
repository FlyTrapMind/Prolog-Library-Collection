:- module(datetime_meta, []).

/** <module> DATETIME_META

Meta-data on the standards that define date-time formats.

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(standards(std_meta)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(iso, 'http://www.iso.org/').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').
:- xml_register_namespace(w3c, 'http://www.w3.org/').

:- initialization(init_lang_ext).



init_lang_ext:-
  standards_graph(G),
  init_iso2014(G),
  init_iso2015(G),
  init_iso2711(G),
  init_iso3307(G),
  init_iso4031(G),
  init_iso8601_1(G),
  init_iso8601_2(G),
  init_iso8601_3(G),
  init_rfc1123(G),
  init_w3c_datetime(G).

init_iso2014(G):-
  assert_standard(G, iso, '2014', _).

init_iso2015(G):-
  assert_standard(G, iso, '2015', _).

init_iso2711(G):-
  assert_standard(G, iso, '2711', _).

init_iso3307(G):-
  assert_standard(G, iso, '3307', _).

init_iso4031(G):-
  assert_standard(G, iso, '4030', _).

init_iso8601_1(G):-
  assert_standard(G, iso, '8601-1', This),
  rdf_assert_datatype(This, iso:year, gYear, 1988, G),
  rdf_assert_datatype(This, iso:edition, int, 1, G),
  rdf_assert_literal(
    This,
    iso:title,
    en,
    'Data elements and interchange formats – Information interchange\c
     – Representation of dates and times',
    G
  ),
  rdf_assert(This, iso:obsoletes, iso:'2014', G),
  rdf_assert(This, iso:obsoletes, iso:'2015', G),
  rdf_assert(This, iso:obsoletes, iso:'2711', G),
  rdf_assert(This, iso:obsoletes, iso:'3307', G),
  rdf_assert(This, iso:obsoletes, iso:'4031', G),
  rdf_assert(This, iso:references, iso:'30-0', G),
  rdf_assert(This, iso:references, iso:'30-1', G),
  rdf_assert(This, iso:references, iso:'646', G).

init_iso8601_2(G):-
  assert_standard(G, iso, '8601-2', This),
  rdf_assert_datatype(This, iso:edition, int, 2, G),
  rdf_assert_literal(This, iso:editor, 'Louis Visser', G),
  rdf_assert_literal(
    This,
    iso:title,
    en,
    'Data elements and interchange formats – Information interchange\c
     – Representation of dates and times',
    G
  ),
  rdf_assert(This, iso:obsoletes, iso:'8601-1', G).

init_iso8601_3(G):-
  assert_standard(G, iso, '8601-3', This),
  rdf_assert_datatype(This, iso:year, gYear, 2004, G),
  rdf_assert_datatype(This, iso:edition, int, 3, G),
  rdf_assert_literal(
    This,
    iso:title,
    en,
    'Data elements and interchange formats – Information interchange\c
     – Representation of dates and times',
    G
  ),
  rdf_assert(This, iso:obsoletes, iso:'8601-2', G).

init_rfc1123(G):-
  assert_standard(G, rfc, '1123', This),
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

init_w3c_datetime(G):-
  assert_standard(G, w3c, datetime, This),
  rdf_assert_datatype(This, w3c:year, gYear, 1997, G),
  rdf_assert_literal(This, w3c:author, 'Misha Wolf', G),
  rdf_assert_literal(This, w3c:author, 'Charles Wicksteed', G),
  rdf_assert(This, foaf:homepage, w3c:'TR/1998/NOTE-datetime-19980827', G).

