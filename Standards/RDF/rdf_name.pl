:- module(
  rdf_name,
  [
    rdf_term_name/2, % +RDF_Term:oneof([bnode,literal,uri])
                     % -Name:atom
    rdf_term_name/3, % +Options:list(nvpair)
                     % +RDF_Term:oneof([bnode,literal,uri])
                     % -Name:atom
    rdf_triple_name/4 % +Subject:oneof([bnode,uri])
                      % +Predicate:uri
                      % +Object:oneof([bnode,literal,uri])
                      % -Term_Name:atom
  ]
).

/** <module> RDF_NAME

Generate names for RDF terms and triples.

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xsd(xsd)).



%! rdf_term_name(+RDF_Term:oneof([bnode,literal,uri]), -Name:atom) is det.
% @see Wrapper around rdf_term_name/3 with empty settings.

rdf_term_name(RDF_Term, Name):-
  rdf_term_name([], RDF_Term, Name).

%! rdf_term_name(
%!   +Options:list(nvpair),
%!   +RDF_Term:oneof([bnode,literal,uri]),
%!   -Name:atom
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
rdf_term_name(O, RDF_Term, Name):-
  is_rdf_list(RDF_Term), !,
  % Recursively retrieve the contents of the RDF list.
  rdf_list(RDF_Term, RDF_Terms),
  maplist(rdf_term_name(O), RDF_Terms, Names),
  print_list(atom(Name), Names).
% A literal with a datatype.
rdf_term_name(O, literal(type(Datatype,LEX)), Name):- !,
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
  format(atom(Name), '"~w"^^~w', [DatatypeName,ValueName]).
% A plain literal with a language tag.
rdf_term_name(_O, literal(lang(Language,Literal)), Name):- !,
  format(atom(Name), '"~w"@~w', [Literal,Language]).
% A simple literal / a plain literal without a language tag.
rdf_term_name(_O, literal(Literal), Name):- !,
  format(atom(Name), '"~w"', [Literal]).
% A blank node.
% @tbd Make this less implementation-dependent, e.g. by mapping
%      internal blank nodes to integers.
rdf_term_name(_O, BNode, Name):-
  rdf_is_bnode(BNode), !,
  Name = BNode.
% Now come the various URIs...
% Only the URI is used. XML namespace prefixes are used when present.
rdf_term_name(O, RDF_Term, Name):-
  option(uri_desc(uri_only), O, uri_only), !,
  rdf_term_uri(RDF_Term, Name).
% If the RDF term has a label, then this is included in its name.
% This is not the case for non-label literals.
rdf_term_name(O, RDF_Term, Name):-
  option(uri_desc(with_preferred_label), O, uri_only), !,
  option(language(Lang), O, en),
  rdfs_preferred_label(RDF_Term, Lang, Label),
  print_list(O, atom(Name), [RDF_Term,Label]).
% The RDF term is set to collate all literals that (directly) relate to it.
rdf_term_name(O, RDF_Term, Name):-
  option(uri_desc(with_literals), O, uri_only), !,
  rdf_term_uri(RDF_Term, URI_Name),

  % Labels are treated specially: only the preferred label is included.
  option(language(Lang), O, en), !,
  rdfs_preferred_label(RDF_Term, Lang, Label),

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
    atom(Name),
    [URI_Name,Label,LiteralNames]
  ).
% We're out of options here...
rdf_term_name(_O, RDF_Term, Name):-
  term_to_atom(RDF_Term, Name).

% The URI has XML namespace prefixes. Take the one that stands for
% the longest URI substring.
rdf_term_uri(URI, Name):-
  rdf_resource_to_namespace(URI, XML_NamespacePrefix, URI_LocalName), !,
  atomic_list_concat([XML_NamespacePrefix,URI_LocalName], ':', Name).
% The URI has no XML namespace prefix.
rdf_term_uri(URI, Name):-
  term_to_atom(URI, Name).

rdf_triple_name(S, P, O, T_Name):-
  maplist(rdf_term_name, [S,P,O], [S_Name,P_Name,O_Name]),
  print_tuple(atom(T_Name), [S_Name,P_Name,O_Name]).

