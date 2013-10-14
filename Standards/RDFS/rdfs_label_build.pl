:- module(
  rdfs_label_build,
  [
    rdfs_assert_label/3, % +Subject:oneof([bnode,iri])
                         % +Label:atom
                         % +Graph:graph
    rdfs_assert_label/4, % +Subject:oneof([bnode,iri])
                         % +Language:atom
                         % +Label:atom
                         % +Graph:graph
    rdfs_replace_label/3, % +Subject:or([bnode,iri])
                          % +Label:atom
                          % +Graph:atom
    rdfs_retractall_label/3, % +Subject:oneof([bnode,iri])
                             % +Label:atom
                             % +Graph:graph
    rdfs_retractall_label/4 % +Subject:oneof([bnode,iri])
                            % +Language:atom
                            % +Label:atom
                            % +Graph:graph
  ]
).

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_lit_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta(rdfs_assert_label(r,+,+)).
:- rdf_meta(rdfs_assert_label(r,+,+,+)).
:- rdf_meta(rdfs_retractall_label(r,+,+)).
:- rdf_meta(rdfs_retractall_label(r,+,+,+)).



%! rdfs_assert_label(
%!   +Subject:oneof([bnode,iri]),
%!   +Label:atom,
%!   +Grap:atom
%! ) is det.
% Assert the subject's label description.
%
% @param Subject An RDF subject term.
% @param Label An atomic description of a resource.
% @param Graph The atomic name of an RDF graph.
% @see rdfs_assert_label/4 also specifies the label.

rdfs_assert_label(S, Label, G):-
  % @ tbd Why is this necessary?
  rdf_global_id(rdfs:label, P),
  rdf_assert_literal(S, P, Label, G).

%! rdfs_assert_label(
%!   +Subject:oneof([bnode,iri]),
%!   +Language:atom,
%!   +Label:atom,
%!   +Grap:atom
%! ) is det.
% Assert the subject's label description in the given label.
%
% @param Subject An RDF subject term.
% @param Language The atomic name of a language.
% @param Label An atomic description of a resource.
% @param Graph The atomic name of an RDF graph.
%
% @tbd Why are explicit conversions necessary here?

rdfs_assert_label(S1, Lang, Label, G):-
  rdf_global_id(S1, S2),
  rdf_global_id(rdfs:label, P),
  rdf_assert_literal(S2, P, Lang, Label, G).

rdfs_replace_label(S, Label, G):-
  rdfs_retractall_label(S, _OldLabel, G),
  rdfs_assert_label(S, Label, G).

rdfs_retractall_label(S, Label, G):-
  rdf_global_id(rdfs:label, P),
  rdf_retractall_literal(S, P, Label, G).

rdfs_retractall_label(S, Lang, Label, G):-
  rdf_global_id(rdfs:label, P),
  rdf_retractall_literal(S, P, Lang, Label, G).

