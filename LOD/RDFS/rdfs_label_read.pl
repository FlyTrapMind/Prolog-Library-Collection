:- module(
  rdfs_label_read,
  [
    rdfs_label/4, % ?Subject:or([bnode,iri])
                  % ?LanguageTag:atom
                  % ?Label:atom
                  % ?Graph:atom
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
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09,
         2014/01-2014/02
*/

:- use_module(dcg(dcg_collection)). % Meta-called.
:- use_module(dcg(dcg_content)). % Meta-called.
:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').



rdfs_label(Subject, Lang, Label, Graph):-
  rdf_literal(Subject, rdfs:label, Lang, Label, Graph).


%! rdfs_label2(
%!   +Options:list(nvpair),
%!   ?RDF_Term:or([bnode,iri]),
%!   ?Label:atom
%! ) is nondet.
% Reads an RDFS label, and adds support for:
%   * Natural language preferences
%   * RDF lists
%
% The following options are supported:
%   * `lang(?LanguageTag:atom)`
%     The preferred natural language.
%
% @tbd Support prioritized language tab lists (as in HTTP Accept-Languages).

:- rdf_meta(rdfs_label2(+,r,-)).
rdfs_label2(O1, RDF_Term, RDFS_Label):-
  rdf_is_list(RDF_Term), !,
  rdf_list([recursive(false)], RDF_Term, RDF_Terms),
  maplist(rdfs_label2(O1), RDF_Terms, RDFS_Labels),
  dcg_with_output_to(atom(RDFS_Label), list(pl_term, RDFS_Labels)).
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

:- rdf_meta(rdfs_preferred_label(r,+,-,?)).
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

:- rdf_meta(rdfs_list_by_label(r,+,-)).
rdfs_list_by_label(RDF_List, Label, Element):-
  rdf_list_first(RDF_List, First),
  rdfs_list_by_label_(First, Label, Element).

rdfs_list_by_label_(Element, Label, Element):-
  rdfs_label(Element, Label), !.
rdfs_list_by_label_(Element, Label, Element0):-
  rdf_list_next(Element, NextElement),
  rdfs_list_by_label_(NextElement, Label, Element0).

