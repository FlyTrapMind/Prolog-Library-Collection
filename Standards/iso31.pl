:- module(iso31, []).

/** <module> ISO31

Support for ISO 31.

@author Wouter Beek
@version 2013/08
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(standards(std_meta)).

%:- initialization(init_iso31).



init_iso31:-
  standards_graph(G),
  init_iso31_0(G),
  init_iso31_1(G),
  init_iso31_2(G),
  init_iso31_3(G),
  init_iso31_4(G),
  init_iso31_5(G),
  init_iso31_6(G),
  init_iso31_7(G),
  init_iso31_8(G),
  init_iso31_9(G),
  init_iso31_10(G),
  init_iso31_11(G),
  init_iso31_12(G),
  init_iso31_13(G),
  init_iso80000_2(G),
  init_iso80000_3(G),
  init_iso80000_4(G),
  init_iso80000_5(G),
  init_iso80000_8(G).

init_iso31_0(G):-
  assert_standard(G, iso, '31-0', This),
  rdf_assert_literal(This, iso:title, en, 'General principles', G),
  rdf_assert_datatype(This, iso:year, gYear, 1981, G).

init_iso31_1(G):-
  assert_standard(G, iso, '31-1', This),
  rdf_assert_literal(This, iso:title, en, 'Space and time', G),
  rdf_assert_datatype(This, iso:year, gYear, 1978, G).

init_iso31_2(G):-
  assert_standard(G, iso, '31-2', This),
  rdf_assert_literal(
    This, iso:title, en, 'Periodic and related phenomena', G
  ).

init_iso31_3(G):-
  assert_standard(G, iso, '31-3', This),
  rdf_assert_literal(This, iso:title, en, 'Mechanics', G).

init_iso31_4(G):-
  assert_standard(G, iso, '31-4', This),
  rdf_assert_literal(This, iso:title, en, 'Heat', G).

init_iso31_5(G):-
  assert_standard(G, iso, '31-5', This),
  rdf_assert_literal(This, iso:title, en, 'Electricity and magnetism', G).

init_iso31_6(G):-
  assert_standard(G, iso, '31-6', This),
  rdf_assert_literal(
    This, iso:title, en, 'Light and related electromagnetic radiations', G
  ).

init_iso31_7(G):-
  assert_standard(G, iso, '31-7', This),
  rdf_assert_literal(This, iso:title, en, 'Acoustics', G).

init_iso31_8(G):-
  assert_standard(G, iso, '31-8', This),
  rdf_assert_literal(
    This, iso:title, en, 'Physical chemistry and molecular physics', G
  ).

init_iso31_9(G):-
  assert_standard(G, iso, '31-9', This),
  rdf_assert_literal(This, iso:title, en, 'Atomic and nuclear physics', G).

init_iso31_10(G):-
  assert_standard(G, iso, '31-10', This),
  rdf_assert_literal(
    This, iso:title, en, 'Nuclear reactions and ionizing radiations', G
  ).

init_iso31_11(G):-
  assert_standard(G, iso, '31-11', This),
  rdf_assert_literal(
    This,
    iso:title,
    en,
    'Mathematical signs and symbols for use in the physical sciences and\c
     technology',
    G
  ).

init_iso31_12(G):-
  assert_standard(G, iso, '31-12', This),
  rdf_assert_literal(This, iso:title, en, 'Characteristic numbers', G).

init_iso31_13(G):-
  assert_standard(G, iso, '31-13', This),
  rdf_assert_literal(This, iso:title, en, 'Solid state physics', G).

init_iso80000_2(G):-
  assert_standard(G, iso, '80000-2', This),
  rdf_assert_datatype(This, iso:year, gYear, 2009, G),
  rdf_assert(This, iso:obsoletes, iso:'31-11', G).

init_iso80000_3(G):-
  assert_standard(G, iso, '80000-3', This),
  rdf_assert_datatype(This, iso:year, gYear, 2007, G),
  rdf_assert(This, iso:obsoletes, iso:'31-1', G),
  rdf_assert(This, iso:obsoletes, iso:'31-2', G).

init_iso80000_4(G):-
  assert_standard(G, iso, '80000-4', This),
  rdf_assert_datatype(This, iso:year, gYear, 2006, G),
  rdf_assert(This, iso:obsoletes, iso:'31-3', G).

init_iso80000_5(G):-
  assert_standard(G, iso, '80000-5', This),
  rdf_assert_datatype(This, iso:year, gYear, 2007, G),
  rdf_assert(This, iso:obsoletes, iso:'31-4', G).

init_iso80000_8(G):-
  assert_standard(G, iso, '80000-8', This),
  rdf_assert_datatype(This, iso:year, gYear, 2007, G),
  rdf_assert(This, iso:obsoletes, iso:'31-7', G).

