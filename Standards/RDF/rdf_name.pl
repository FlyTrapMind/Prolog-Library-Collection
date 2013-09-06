:- module(
  rdf_name,
  [
    rdf_pair_name/1, % +Pair:pair(or([iri,literal]))
    rdf_term_name/1, % +RDF_Term:oneof([bnode,literal,uri])
    rdf_term_name/2, % +Options:list(nvpair)
                     % +RDF_Term:oneof([bnode,literal,uri])
    rdf_term_name/3, % +Options:list(nvpair)
                     % +RDF_Term:oneof([bnode,literal,uri])
                     % -Name:atom
    rdf_terms_name/2, % +RDF_Terms:list(or(bnode,literal,uri))
                      % -Name:atom
    rdf_terms_name/3, % +Options:list(nvpair)
                      % +RDF_Terms:list(or(bnode,literal,uri))
                      % -Name:atom
    rdf_triple_name/2, % +Triple:triple(oneof([bnode,uri]),uri,oneof([bnode,literal,uri]))
                       % -TripleName:atom
    rdf_triple_name/4 % +Subject:oneof([bnode,uri])
                      % +Predicate:uri
                      % +Object:oneof([bnode,literal,uri])
                      % -TripleName:atom
  ]
).

/** <module> RDF_NAME

Generate names for RDF terms and triples.

@author Wouter Beek
@version 2013/07-2013/09
*/

:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xsd(xsd)).



%! rdf_pair_name(+Pair:pair(or([iri,literal]))) is det.
% Helper predicate for providing an atomic label for predicate-object pairs.

rdf_pair_name(X1-Y1):-
  rdf_global_id(X1, X2),
  rdf_global_id(Y1, Y2),
  print_pair([write_method(rdf_term_name)], X2-Y2).

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
rdf_term_name(O, RDF_Term):-
  rdf_is_list(RDF_Term), !,
  % Recursively retrieve the contents of the RDF list.
  rdf_list(RDF_Term, RDF_Terms),
  maplist(rdf_term_name(O), RDF_Terms, Names),
  print_list([], Names).
% A literal with a datatype.
rdf_term_name(O, literal(type(Datatype,LEX))):- !,
  % The datatype name.
  rdf_term_name(O, Datatype, DatatypeName),
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
rdf_term_name(_O, literal(lang(Language,Literal))):- !,
  format('"~w"@~w', [Literal,Language]).
% A simple literal / a plain literal without a language tag.
rdf_term_name(_O, literal(Literal)):- !,
  format('"~w"', [Literal]).
% A blank node.
% @tbd Make this less implementation-dependent, e.g. by mapping
%      internal blank nodes to integers.
rdf_term_name(_O, BNode):-
  rdf_is_bnode(BNode), !,
  write(BNode).
% Now come the various URIs...
% Only the URI is used. XML namespace prefixes are used when present.
rdf_term_name(O, RDF_Term):-
  option(uri_desc(uri_only), O, uri_only), !,
  rdf_term_iri(RDF_Term).
% If the RDF term has a label, then this is included in its name.
% This is not the case for non-label literals.
rdf_term_name(O1, RDF_Term):-
  option(uri_desc(with_preferred_label), O1, uri_only), !,
  option(language(Lang), O1, en),
  rdfs_preferred_label(RDF_Term, Lang, _PreferredLang, PreferredLabel),
  print_list(O1, [RDF_Term,PreferredLabel]).
% The RDF term is set to collate all literals that (directly) relate to it.
rdf_term_name(O, RDF_Term):-
  option(uri_desc(with_literals), O, uri_only), !,
  with_output_to(atom(IRI_Name), rdf_term_iri(RDF_Term)),

  % Labels are treated specially: only the preferred label is included.
  option(language(Lang), O, en), !,
  rdfs_preferred_label(RDF_Term, Lang, _PreferredLang, PreferredLabel),

  % Now come the related non-label literals.
  findall(
    LiteralName,
    (
      rdf_has(RDF_Term, P, Literal),
      rdf_is_literal(Literal),
      % Exclude labels.
      \+ rdf_global_id(rdfs:type, P),
      rdf_term_name(O, Literal, LiteralName)
    ),
    LiteralNames
  ),

  print_set(
    [begin(''),end(''),separator('\n')],
    [IRI_Name,PreferredLabel,LiteralNames]
  ).

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

%! rdf_terms_name(+RDF_Terms:list(or(bnode,literal,iri)), -Name:atom) is det.
% @see rdf_terms_name/3

rdf_terms_name(RDF_Terms, Name):-
  rdf_terms_name([], RDF_Terms, Name).

%! rdf_terms_name(
%!   +Options:list(nvpair),
%!   +RDF_Terms:list(or(bnode,literal,iri)),
%!   -Name:atom
%! ) is det.
% Retruns an atomic name for the given list of RDF terms.
% List order and duplicates is retained.

rdf_terms_name(O1, RDF_Terms, Name):-
  maplist(rdf_term_name(O1), RDF_Terms, Names),
  with_output_to(atom(Name), print_list(O1, Names)).

rdf_term_pair_name(O1, RDF_Term1-RDF_Term2, Name):-
  maplist(rdf_term_name(O1), [RDF_Term1,RDF_Term2], [Name1,Name2]),
  format(atom(Name), '~w-~w', [Name1,Name2]).

rdf_term_pairs_name(O1, RDF_TermPairs, Name):-
  maplist(rdf_term_pair_name(O1), RDF_TermPairs, Names),
  with_output_to(atom(Name), print_list(O1, Names)).

rdf_triple_name(rdf(S1,P1,O1), T_Name):- !,
  maplist(rdf_global_id, [S1,P1,O1], [S2,P2,O2]),
  rdf_triple_name(S2, P2, O2, T_Name).
rdf_triple_name(T_Name, T_Name).

rdf_triple_name(S, P, O, T_Name):-
  maplist(rdf_term_name, [S,P,O], [S_Name,P_Name,O_Name]),
  with_output_to(atom(T_Name), print_tuple([], [S_Name,P_Name,O_Name])).

