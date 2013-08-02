:- module(rfc5646_rdf, [rfc5646_rdf/0]).

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
:- use_module(standards(record_jar)).
:- use_module(uri(rfc2396_dcg)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rfc5646, 'http://www.rfc5646.com/').

rfc5646_graph(rfc5646).
rfc5646_host([www,rfc5646,com]).
rfc5646_scheme(http).

:- initialization(init_rfc5646_rdf).



rfc5646_rdf:-
  absolute_file_name(
    lang(rfc5646_iana_registry),
    File,
    [access(read), file_type(text)]
  ),
  setup_call_cleanup(
    open(File, read, Stream, [type(binary)]),
    once(phrase_from_stream('record-jar'(_Encoding, [_Date|Rs]), Stream)),
    close(Stream)
  ),
  maplist(rfc5646_rdf_record, Rs).

rfc5646_rdf_record(R1):-
  rfc5646_graph(G),
  % Subtag resource
  selectchk('Type'=Type, R1, R2),
  selectchk('Subtag'=Subtag, R2, R3),
  create_subtag_resource(Type, Subtag, G, LanguageSubtag),
  rfc5646_rdf_record_(G, LanguageSubtag, R3).

rfc5646_rdf_record_(_G, _I, []):- !.
rfc5646_rdf_record_(G, I, [Name=Value|T]):-
gtrace,
  rfc5646_rdf_nvpair(G, I, Name, Value),
  rfc5646_rdf_record_(G, I, T).

% Added
rfc5646_rdf_nvpair(G, I, 'Added', V):-
  rdf_assert_datatype(I, rfc5646:added, date, V, G).
% Comment
rfc5646_rdf_nvpair(G, I, 'Comment', V):-
  rdf_assert_literal(I, rfc5646:comment, en, V, G).
% Deprecated
rfc5646_rdf_nvpair(G, I, 'Deprecated', V):-
  rdf_assert_datatype(I, rfc5646:deprecated, date, V, G).
% Description
rfc5646_rdf_nvpair(G, I, 'Description', V):-
  rdf_assert_literal(I, rfc5646:description, en, V, G).
% Macrolanguage
rfc5646_rdf_nvpair(G, I, 'Macrolanguage', V1):-
  create_subtag_resource(language, V1, G, V2),
  rdf_assert(I, rfc5646:macrolanguage, V2, G).
% Preferred-Value
rfc5646_rdf_nvpair(G, I, 'Preferred-Value', V1):-
  create_subtag_resource(language, V1, G, V2),
  rdf_assert(I, rfc5646:preferred_value, V2, G).
% Prefix
rfc5646_rdf_nvpair(G, I, 'Prefix', V1):-
  create_subtag_resource(language, V1, G, V2),
  rdf_assert(I, rfc5646:prefix, V2, G).
% Scope
rfc5646_rdf_nvpair(G, I, 'Scope', V1):-
  rdf_global_id(rfc5646:V1, V2),
  rdf_assert(I, rfc5646:scope, V2, G).
% Suppresses script
rfc5646_rdf_nvpair(G, I, 'Suppress-Script', V1):-
  create_subtag_resource(script, V1, G, V2),
  rdf_assert(I, rfc5646:suppress_script, V2, G).

create_subtag_resource(Type, Subtag, G, LanguageSubtag2):-
  rfc5646_scheme(Scheme),
  rfc5646_host(Host),
  once(
    phrase(
      rfc2396_uri_reference(
        _Tree,
        Scheme,
        authority(_User,Host,_Port),
        [[Type],[Subtag]],
        _Query,
        _Fragment
      ),
      LanguageSubtag1
    )
  ),
  atom_codes(LanguageSubtag2, LanguageSubtag1),
  rdfs_label(Class, Type),
  rdf_assert_individual(LanguageSubtag2, Class, G),
  rdfs_assert_label(LanguageSubtag2, Subtag, G).

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

