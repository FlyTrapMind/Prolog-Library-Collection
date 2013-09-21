:- module(
  rdfs_label,
  [
    rdfs_label2/3, % +Options:list(nvpair)
                   % ?RDF_Term:or([bnode,iri])
                   % ?Label:atom
    rdfs_preferred_label/4, % ?RDF_Term:oneof([bnode,uri])
                            % +LanguageTags:or([atom,list(atom)])
                            % -PreferredLanguageTag:atom
                            % ?PreferredLiteral:atom
    rdfs_list_by_label/3 % +List:uri
                         % +Label:atom
                         % -Element:uri
  ]
).

/** <module> RDFS label

Predicates for RDFS labels.

@author Wouter Beek
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09
*/

:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)). % rdf-meta.
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta(rdfs_label2(+,r,-)).
:- rdf_meta(rdfs_preferred_label(r,+,-,?)).
:- rdf_meta(rdfs_list_by_label(r,+,-)).



%! rdfs_label2(
%!   +Options:list(nvpair),
%!   ?RDF_Term:or([bnode,iri]),
%!   ?Label:atom
%! ) is nondet.

rdfs_label2(O1, RDF_Term, RDFS_Label):-
  rdf_is_list(RDF_Term), !,
  rdf_list([recursive(false)], RDF_Term, RDF_Terms),
  maplist(rdfs_label2(O1), RDF_Terms, RDFS_Labels),
  with_output_to(atom(RDFS_Label), print_list(O1, RDFS_Labels)).
rdfs_label2(O1, RDF_Term, RDFS_Label):-
  option(lang(Lang), O1, en),
  rdfs_preferred_label(RDF_Term, Lang, _UsedLang, RDFS_Label).

%! rdfs_preferred_label(
%!   ?RDF_Term:or([bnode,iri]),
%!   +LanguageTag:atom,
%!   -PreferredLangTags:or([atom,list(atom)]),
%!   ?Label:atom
%! ) is nondet.
% Multiple labels are returned (nondet) in a descending preference order.

rdfs_preferred_label(RDF_Term, LangTags, PreferredLangTag, PreferredLabel):-
  rdfs_label(RDF_Term, Label1),
  rdf_preferred_literal(
    RDF_Term,
    rdfs:label,
    LangTags,
    PreferredLangTag,
    PreferredLabel
  ),
  Label1 == PreferredLabel, !.

%! rdfs_list_by_label(
%!   +RDF_List:or([bnode,iri]),
%!   +Label:atom,
%!   -Element:iri
%! ) is nondet.
% Returns RDF list elements that have the given label.

rdfs_list_by_label(RDF_List, Label, Element):-
  rdf_list_first(RDF_List, First),
  rdfs_list_by_label_(First, Label, Element).

rdfs_list_by_label_(Element, Label, Element):-
  rdfs_label(Element, Label), !.
rdfs_list_by_label_(Element, Label, Element0):-
  rdf_list_next(Element, NextElement),
  rdfs_list_by_label_(NextElement, Label, Element0).

