:- module(
  rdf_name,
  [
    rdf_term_name//1, % ?RDF_Term
    rdf_term_name//2, % +Options:list(nvpair)
                      % +RDF_Term
    rdf_triple_name//3, % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +Object:or([bnode,iri,literal])
    rdf_triple_name//4 % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +Object:or([bnode,iri,literal])
                       % +Graph:atom
  ]
).

/** <module> RDF name

Generates names for RDF terms and triples.

@author Wouter Beek
@version 2013/07-2013/09, 2014/01-2014/02
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_collection)).
:- use_module(generics(codes_ext)).
:- use_module(generics(error_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdfs(rdfs_label_read)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)).



% TERM %

%! rdf_term_name(+RDF_Term:oneof([bnode,iri,literal]))// is det.
%! rdf_term_name(
%!   +Options:list(nvpair),
%!   +RDF_Term:oneof([bnode,iri,literal])
%!)// is det.
% Returns a display name for the given RDF term.
%
% The following options are supported:
%   1. =|language(+Language:atom)|=
%      The atomic language tag of the language that is preferred for
%      use in the RDF term's name.
%      The default value is `en`.
%   2. =|uri_desc(+DescriptionMode:oneof([
%        only_literals,
%        only_preferred_label,
%        uri_only,
%        with_literals,
%        with_preferred_label
%      ]))|=
%      Whether or not literals are included in the name of the RDF term.
%      The default value is `uri_only`.
%
% @arg Options A list of name-value pairs.
% @arg RDF_Term An RDF term.

:- rdf_meta(rdf_term_name(r,?,?)).
rdf_term_name(RDF_Term) -->
  rdf_term_name([], RDF_Term).

% RDF list.
% @tbd Fix this.
%rdf_term_name(O1, RDF_List) -->
%  {rdf_is_list(RDF_List)}, !,
%  rdf_list_name(O1, RDF_List).
% Blank node.
rdf_term_name(_, BNode) -->
  {rdf_is_bnode(BNode)}, !,
  rdf_bnode(BNode).
% Literal.
rdf_term_name(_, Literal) -->
  {rdf_is_literal(Literal)}, !,
  rdf_literal(Literal).
% IRI.
rdf_term_name(O1, IRI1) -->
  {(
    rdf_global_id(IRI2, IRI1), IRI2 = _:_
  ;
    is_of_type(iri, IRI1)
  )}, !,
  rdf_iri(O1, IRI1).
% Prolog term.
rdf_term_name(_, PL_Term) -->
  {with_output_to(codes(Codes), write_canonical_catch(PL_Term))},
  codes(Codes).



% TRIPLE %

:- rdf_meta(rdf_triple_name(r,r,r,?,?)).
rdf_triple_name(S, P, O) -->
  tuple(ascii, rdf_term_name, [S,P,O]).
:- rdf_meta(rdf_triple_name(r,r,r,+,?,?)).
rdf_triple_name(S, P, O, G) -->
  tuple(ascii, rdf_term_name, [S,P,O,G]).



% BLANK NODE %

rdf_bnode(BNode) -->
  atom(BNode).



% LITERAL %

rdf_language_tag(Language) -->
  atom(Language).

% Typed literals must occur before plain literals.
rdf_literal(Literal) -->
  rdf_typed_literal(Literal).
rdf_literal(Literal) -->
  rdf_plain_literal(Literal).

% Non-simple plain literals must occur before simple literals.
rdf_plain_literal(literal(lang(Language,Value))) --> !,
  rdf_simple_literal(Value),
  "@",
  rdf_language_tag(Language).
rdf_plain_literal(literal(Value)) -->
  rdf_simple_literal(Value).

rdf_simple_literal(Value) -->
  quoted(atom(Value)).

rdf_typed_literal(literal(type(Datatype,Literal))) -->
  {(
    % The datatype is recognized, so we can display
    % the lexically mapped value.
    xsd_datatype(Datatype)
  ->
    rdf_datatype(Datatype, Literal, Value)
  ;
    Value = Literal
  )},
  quoted(atom(Value)),
  `^^`,
  rdf_iri([], Datatype).



% IRI %

% The options `only_preferred_label` and `with_preferred_label`.
rdf_iri(O1, RDF_Term) -->
  % Whether to include the RDF term itself or only its preferred RDFS label.
  (
    {option(uri_desc(with_preferred_label), O1)}
  ->
    rdf_iri([uri_desc(uri_only)], RDF_Term),
    nl
  ;
    {option(uri_desc(only_preferred_label), O1)}
  ), !,

  % See whether a preferred label can be found.
  {option(language(Lang), O1, en)},
  (
    {rdfs_preferred_label(RDF_Term, Lang, _PreferredLang, PreferredLabel)}
  ->
    atom(PreferredLabel)
  ;
    ``
  ).
% The RDF term is set to collate all literals that (directly) relate to it.
% These are options `only_literals` and `with_literals`.
rdf_iri(O1, RDF_Term) -->
  % The URI, if included.
  {(
    option(uri_desc(with_literals), O1)
  ->
    Elements = [RDF_Term|Literals2]
  ;
    option(uri_desc(only_literals), O1)
  ->
    Elements = Literals2
  )},

  {
    % Labels are treated specially: only the preferred label is included.
    option(language(Lang), O1, en),
    rdfs_preferred_label(RDF_Term, Lang, _PreferredLang, PreferredLabel),

    % All non-label literals are included.
    findall(
      Literal,
      (
        % Any directly related literal.
        rdf(RDF_Term, P, Literal),
        rdf_is_literal(Literal),
        % Exclude literals that are RDFS labels.
        \+ rdf_equal(rdfs:label, P)
      ),
      Literals1
    ),
    append(Literals1, [PreferredLabel], Literals2)
  },

  collection(``, ``, list_to_ord_set, nl, rdf_term_name, Elements).
% Only the URI is used. XML namespace prefixes are used when present.
% This appears last, since it is the default or fallback option.
% When option `uri_desc` is set to `uri_only` we end up here as well.
% Writes a given RDF term that is an IRI.
% This is the IRI ad verbatim, or a shortened version, if there is a
% registered XML namespace prefix for this IRI.
% We take the XML namespace prefix that results in the shortest output form.
% The IRI has at least one XML namespace prefix.
rdf_iri(_, IRI) -->
  % We take the prefix that stands for the longest IRI substring.
  {rdf_resource_to_namespace(IRI, Prefix, LocalName)}, !,
  atom(Prefix),
  `:`,
  atom(LocalName).
% An IRI without an XML namespace prefix.
rdf_iri(_, IRI) -->
  atom(IRI).

