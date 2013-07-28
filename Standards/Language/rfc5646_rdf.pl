:- module(rfc5646_rdf, []).

/** <module> RFC5646_RDF

Convert the IANA registry text file for RFC 5646 to RDF.

@author Wouter Beek
@version 2013/07
*/

:- use_module(lang(rfc5646_iana)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(uri(rfc2396_dcg)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rfc5646, 'http://www.rfc5646.com/').

rfc5646_graph(rfc5646).
rfc5646_host([www,rfc5646,com]).
rfc5646_scheme(http).

:- initialization(init_rfc5646_rdf).



iana_rdf_conversion:-
  forall(
    iana_find(
      registration(
        _Tree,
        Type,
        Subtag,
        Descriptions,
        Added,
        SuppressScript,
        Scope,
        Prefixes,
        Macrolanguage,
        Comment,
        Deprecated,
        PreferredValue
      )
    ),
    assert_subtag(
      Type,
      Subtag,
      Descriptions,
      Added,
      SuppressScript,
      Scope,
      Prefixes,
      Macrolanguage,
      Comment,
      Deprecated,
      PreferredValue
    )
  ).

assert_subtag(
  Type,
  Subtag,
  Descriptions,
  Added1,
  _SuppressScript,
  _Scope,
  _Prefixes,
  _Macrolanguage,
  Comment,
  _Deprecated,
  _PreferredValue
):-
  rfc5646_graph(G),
  create_subtag_resource(Type, Subtag, G, LanguageSubtag),
  maplist(rdf_assert_literal(LanguageSubtag, rfc5646:description, en), Descriptions),
  rdf_assert_literal(LanguageSubtag, rfc5646:comment, en, Comment),
  rdf_assert_datatype(LanguageSubtag, rfc5646:added, date, Added1, G),
  true.

create_subtag_resource(Type, Subtag, G, LanguageSubtag):-
  rfc5646_scheme(Scheme),
  rfc5646_host(Host),
  phrase(
    rfc2396_uri_reference(
      _Tree,
      Scheme,
      authority(_User,Host,_Port),
      [[Type],[Subtag]],
      _Query,
      _Fragment
    ),
    LanguageSubtag
  ),
  rdfs_label(Class, Type),
  rdf_assert_individual(LanguageSubtag, Class, G),
  rdfs_assert_label(LanguageSubtag, Subtag, G).

init_rfc5646_rdf:-
  rfc5646_graph(G),
  rdfs_assert_class(rfc5646:'Subtag', G),
  rdfs_assert_subclass(rfc5646:'Extension', rfc5646:'Subtag', G),
  rdfs_assert_label(rfc5646:'Extension', en, extension, G),
  rdfs_assert_subclass(rfc5646:'Grandfathered', rfc5646:'Subtag', G),
  rdfs_assert_label(rfc5646:'Grandfathered', en, grandfathered, G),
  rdfs_assert_subclass(rfc5646:'Language', rfc5646:'Subtag', G),
  rdfs_assert_label(rfc5646:'Language', en, language, G),
  rdfs_assert_subclass(rfc5646:'Redundant', rfc5646:'Subtag', G),
  rdfs_assert_label(rfc5646:'Redundant', en, redundant, G),
  rdfs_assert_subclass(rfc5646:'Region', rfc5646:'Subtag', G),
  rdfs_assert_label(rfc5646:'Region', en, region, G),
  rdfs_assert_subclass(rfc5646:'Script', rfc5646:'Subtag', G),
  rdfs_assert_label(rfc5646:'Script', en, script, G),
  rdfs_assert_subclass(rfc5646:'Variant', rfc5646:'Subtag', G),
  rdfs_assert_label(rfc5646:'Variant', en, variant, G).



