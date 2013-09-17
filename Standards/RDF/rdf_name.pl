:- module(
  rdf_name,
  [
    dcg_rdf_term_name//1, % +RDF_Term:oneof([bnode,literal,uri])
    dcg_rdf_term_name//2, % +Options:list(nvpair)
                          % +RDF_Term:oneof([bnode,literal,uri])
    rdf_pair_name/3, % +Options:list(nvpair)
                     % +RDF_Term1:pair(or([bnode,iri,literal]))
                     % +RDF_Term1:pair(or([bnode,iri,literal]))
    rdf_term_name/1, % +RDF_Term:oneof([bnode,literal,uri])
    rdf_term_name/2, % +Options:list(nvpair)
                     % +RDF_Term:oneof([bnode,literal,uri])
    rdf_term_name/3, % +Options:list(nvpair)
                     % +RDF_Term:oneof([bnode,literal,uri])
                     % -Name:atom
    rdf_triple_name/2, % +Options:list(nvpair)
                       % +Triple:triple(oneof([bnode,uri]),uri,oneof([bnode,literal,uri]))
    rdf_triple_name/3, % +Options:list(nvpair)
                       % +Triple:triple(oneof([bnode,uri]),uri,oneof([bnode,literal,uri]))
                       % -TripleName:atom
    rdf_triple_name/4 % +Options:list(nvpair)
                      % +Subject:oneof([bnode,uri])
                      % +Predicate:uri
                      % +Object:oneof([bnode,literal,uri])
  ]
).

/** <module> RDF_NAME

Generate names for RDF terms and triples.

@author Wouter Beek
@version 2013/07-2013/09
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdfs(rdfs_label)).
:- use_module(xsd(xsd)).

:- rdf_meta(dcg_rdf_term_name(r,?,?)).
:- rdf_meta(dcg_rdf_term_name(+,r,?,?)).
:- rdf_meta(rdf_pair_name(+,r,r)).
:- rdf_meta(rdf_term_name(r)).
:- rdf_meta(rdf_term_name(+,r)).
:- rdf_meta(rdf_term_name(+,r,-)).
:- rdf_meta(rdf_triple_name(+,t)).
:- rdf_meta(rdf_triple_name(+,t,-)).
:- rdf_meta(rdf_triple_name(+,r,r,r)).



dcg_rdf_term_name(RDF_Term) -->
  dcg_rdf_term_name([], RDF_Term).

dcg_rdf_term_name(O1, RDF_Term) -->
  {rdf_term_name(O1, RDF_Term, RDF_TermName)},
  atom(RDF_TermName).

%! rdf_pair_name(
%!   +Options:list(nvpair),
%!   +RDF_Term1:pair(or([bnode,iri,literal])),
%!   +RDF_Term2:pair(or([bnode,iri,literal]))
%! ) is det.
% Writes a pair of RDF terms, e.g., a predicate-object pair.

rdf_pair_name(O1, X, Y):-
  merge_options(O1, [write_method(rdf_term_name)], O2),
  print_pair(O2, X, Y).

rdf_term_name(RDF_Term):-
  rdf_term_name([], RDF_Term).

%! rdf_term_name(
%!   +Options:list(nvpair),
%!   +RDF_Term:oneof([bnode,literal,iri])
%!) is det.
% Returns a display name for the given RDF term.
%
% The following options are supported:
%   1. `language(+Language:atom)`
%      The atomic language tag of the language that is preferred for
%      use in the RDF term's name.
%      The default value is `en`.
%   2. `uri_desc(+DescriptionMode:oneof([uri_only,with_literals,with_preferred_label]))`
%      Whether or not literals are included in the name of the RDF term.
%      The default value is `uri_only`.
%
% @param Options A list of name-value pairs.
% @param RDF_Term An RDF term.
% @param Name The atomic name of an RDF term.

% An RDF list.
rdf_term_name(O1, RDF_Term):-
  rdf_is_list(RDF_Term), !,
  rdf_list_name(O1, RDF_Term).
% A literal with a datatype.
rdf_term_name(O1, literal(type(Datatype,LEX))):- !,
  % The datatype name.
  rdf_term_name(O1, Datatype, DatatypeName),
  % The datatyped value.
  (
    % The datatype is recognized, so the datatyped value can be displayed
    % as a SWI-Prolog native.
    xsd_datatype(DatatypeName, Datatype)
  ->
    xsd_lexicalMap(Datatype, LEX, ValueName)
  ;
    ValueName = LEX
  ),
  % The combined name.
  format('"~w"^^~w', [ValueName,DatatypeName]).
% A plain literal with a language tag.
rdf_term_name(_O1, literal(lang(Language,Literal))):- !,
  format('"~w"@~w', [Literal,Language]).
% A simple literal / a plain literal without a language tag.
rdf_term_name(_O1, literal(Literal)):- !,
  format('"~w"', [Literal]).
% A blank node.
% @tbd Make this less implementation-dependent, e.g. by mapping
%      internal blank nodes to integers.
rdf_term_name(_O1, BNode):-
  rdf_is_bnode(BNode), !,
  write(BNode).
% If the RDF term has a label, then this is included in its name.
% This is not the case for non-label literals.
rdf_term_name(O1, RDF_Term):-
  option(uri_desc(O), O1, uri_only),
  (O == only_preferred_label ; O == with_preferred_label),

  % See whether a preferred label can be found.
  option(language(Lang), O1, en),
  rdfs_preferred_label(RDF_Term, Lang, _PreferredLang, PreferredLabel), !,

  % Whether to include the RDF term itself or not.
  (
    O == with_preferred_label
  ->
    with_output_to(atom(RDF_TermName), rdf_term_iri(RDF_Term)),
    Rows = [RDF_TermName,PreferredLabel]
  ;
    Rows = [PreferredLabel]
  ),

  print_collection(
    [begin(''),end(''),ordering(list_to_ord_set),separator('\n')],
    Rows
  ).
% The RDF term is set to collate all literals that (directly) relate to it.
rdf_term_name(O1, RDF_Term):-
  option(uri_desc(O), O1, uri_only),
  (O = with_literals ; O = only_literals), !,

  % The URI, if included.
  (
    O = only_literals
  ->
    IRI_Name = []
  ;
    with_output_to(atom(IRI_Name0), rdf_term_iri(RDF_Term)),
    IRI_Name = [IRI_Name0]
  ),

  % Labels are treated specially: only the preferred label is included.
  option(language(Lang), O1, en), !,
  rdfs_preferred_label(RDF_Term, Lang, _PreferredLang, PreferredLabel),

  % Non-label literals are all included.
  findall(
    LiteralName,
    (
      rdf_has(RDF_Term, P, Literal),
      rdf_is_literal(Literal),
      % Exclude labels.
      \+ rdf_global_id(rdfs:type, P),
      rdf_term_name(O1, Literal, LiteralName)
    ),
    LiteralNames
  ),

  append(IRI_Name, [PreferredLabel|LiteralNames], Rows),
  print_collection(
    [begin(''),end(''),ordering(list_to_ord_set),separator('\n')],
    Rows
  ).
% Only the URI is used. XML namespace prefixes are used when present.
% This appears last, since it is the fallback option.
% When option `uri_desc` is set to `uri_only` one ends up here as well.
rdf_term_name(_O1, RDF_Term):-
  rdf_term_iri(RDF_Term).

rdf_term_name(O1, RDF_Term, Name):-
  with_output_to(atom(Name), rdf_term_name(O1, RDF_Term)).

%! rdf_term_iri(+IRI) is det.
% Writes a given RDF term that is an IRI.
% This is the IRI ad verbatim, or a shortened version, if there is a
% registered XML namespace prefix for this IRI.
% As a matter of fact, we take the XML namespace prefix that results in
% the shortest output form.

% The IRI has at least one XML namespace prefix.
% We take the one that stands for the longest IRI substring.
rdf_term_iri(IRI):-
  rdf_resource_to_namespace(IRI, XML_NamespacePrefix, LocalName), !,
  write(XML_NamespacePrefix), write(':'), write(LocalName).
% The IRI has no XML namespace prefix.
rdf_term_iri(IRI):-
  write(IRI).

%! rdf_triple_name(+Options:list(nvpair), +RDF_Triple:compound) is det.
% @see Wrapper around rdf_triple_name/4.

rdf_triple_name(O1, rdf(S,P,O)):-
  rdf_triple_name(O1, S, P, O).

rdf_triple_name(O1, rdf(S,P,O), TripleName):-
  with_output_to(atom(TripleName), rdf_triple_name(O1, rdf(S,P,O))).

%! rdf_triple_name(
%!   +Options:list(nvpair),
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal])
%! ) is det.
% The supported options are:
%   * The options supported by rdf_term_name/3.
%   * The options supported by print_tuple/2.

rdf_triple_name(O1, S, P, O):-
  maplist(rdf_term_name(O1), [S,P,O], [S_Name,P_Name,O_Name]),
  print_tuple(O1, [S_Name,P_Name,O_Name]).

