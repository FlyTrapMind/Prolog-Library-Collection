:- module(lang_ext, []).

/** <module> LANG_EXT

Support for metadata on natural languages.

## Language codes

2-character language codes are covered by [['iso639-1']].
They are also mapped to 3-character language codes in [['iso639-3']].

3-character language codes are covered by [['iso639-2']] and [['iso639-3']].
The former are also mapped to the latter.

3-character language group/family codes are covered by [['iso639-5']].

## Language tags

Language tags that are used by XML are covered by [[rfc1766]] and [[rfc3066]].

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(std_meta)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').

:- initialization(init_lang_ext).



init_lang_ext:-
  standards_graph(G),
  init_rfc1766(G),
  init_rfc3066(G),
  init_rfc4646(G),
  init_rfc4647(G),
  init_rfc5646(G),
  init_rfc4647(G).

init_rfc1766(G):-
  rdf_global_id(rfc:'1766', This),
  rdfs_assert_class(rfc:'Standard', G),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 1995, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Tags for the Identification of Languages',
    G
  ),
  rdf_assert_literal(This, rfc:author, en, 'H. Alvestrand', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc1766.txt', G).

init_rfc3066(G):-
  rdf_global_id(rfc:'3066', This),
  rdfs_assert_class(rfc:'Standard', G),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 2001, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Tags for the Identification of Languages',
    G
  ),
  rdf_assert_literal(This, rfc:author, en, 'H. Alvestrand', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc3066.txt', G),
  rdf_assert(This, rfc:obsoletes, rfc:'1766', G).

init_rfc4646(G):-
  rdf_global_id(rfc:'4646', This),
  rdfs_assert_class(rfc:'Standard', G),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 2006, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Tags for Identifying Languages',
    G
  ),
  rdf_assert_literal(This, rfc:editor, en, 'A. Phillips', G),
  rdf_assert_literal(This, rfc:editor, en, 'M. Davis', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc4646.txt', G),
  rdf_assert(This, rfc:obsoletes, rfc:'3066', G).

init_rfc4647(G):-
  rdf_global_id(rfc:'4647', This),
  rdfs_assert_class(rfc:'Standard', G),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 2006, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Matching of Language Tags',
    G
  ),
  rdf_assert_literal(This, rfc:editor, en, 'A. Phillips', G),
  rdf_assert_literal(This, rfc:editor, en, 'M. Davis', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc4647.txt', G),
  rdf_assert(This, rfc:obsoletes, rfc:'3066', G).

init_rfc5646(G):-
  rdf_global_id(rfc:'5646', This),
  rdfs_assert_class(rfc:'Standard', G),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 2009, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Tags for Identifying Languages',
    G
  ),
  rdf_assert_literal(This, rfc:editor, en, 'A. Phillips', G),
  rdf_assert_literal(This, rfc:editor, en, 'M. Davis', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc5646.txt', G),
  rdf_assert(This, rfc:obsoletes, rfc:'4646', G).

