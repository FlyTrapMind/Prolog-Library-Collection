:- module(
  sparql_cache,
  [
    sparql_cache/2, % +Resource:or([bnode,iri,literal])
                      % +Graph:atom
    sparql_cache/3 % +Resource:or([bnode,iri,literal])
                     % -Resources:ordset(or([bnode,iri,literal]))
                     % -Propositions:ordset(list)
  ]
).

/** <module> SPARQL Cache

Locally caches triples that are relevant for specific resources.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(generics(row_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).

:- debug(sparql_cache).



%! sparql_cache(+Resource:iri, +Graph:atom) is det.

sparql_cache(Resource, Graph):-
  sparql_cache(Resource, _, Propositions),
  maplist(assert_proposition(Graph), Propositions).

assert_proposition(Graph, [S,P,O]):-
  rdf_assert(S, P, O, Graph).


%! sparql_cache(
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.
% Returns the depth-one resources and propositions for the given resource.
%
% The given resource is assumed to be located on a URL which has
%  a registered SPARQL endpoint.

% Blank node.
sparql_cache(Resource, [], []):-
  rdf_is_bnode(Resource), !.

% Literal.
sparql_cache(Resource, [], []):-
  rdf_is_literal(Resource), !.

% Skip IRI based on parsing (part of) the IRI itself.
sparql_cache(Resource, [], []):-
  uri_components(
    Resource,
    uri_components(Scheme,Domain,Path,_Fragment,_Search)
  ),
  skip_iri(Scheme, Domain, Path).

% IRI with registered SPARQL endpoint.
sparql_cache(Resource, Resources, Propositions):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  sparql_current_remote_domain(Remote, Domain), !,
  phrase(
    sparql_formulate(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource), var(p), var(o))],
      inf,
      _,
      _
    ),
    Query
  ),
  sparql_query(Remote, Query, _VarNames, Rows),

  % Conversion
  rows_to_propositions([Resource], Rows, Propositions),
  ord_union(Propositions, Resources).

%! rows_to_propositions(
%!   +Prefix:list([bnode,literal,iri]),
%!   +Rows:list(compound),
%!   -Propositions:ordset(list)
%! ) is det.
% Returns the ordered set of propositions that occur in
%  the given SPARQL result set rows.
%
% @arg Prefix This contains the stable prefix list of each proposition.
%      This is usually the singleton list of the subject term.
% @arg Rows
% @arg Propositions An ordered set of lists of length 3 (s-p-o).

rows_to_propositions(Prefix, Rows, Props):-
  rows_to_propositions(Prefix, Rows, [], Props).

rows_to_propositions(_, [], Sol, Sol):- !.
rows_to_propositions(Prefix, [H1|T], L1, Sol):-
  row_to_proposition(Prefix, H1, H2),
  ord_add_element(L1, H2, L2),
  rows_to_propositions(Prefix, T, L2, Sol).

row_to_proposition(Prefix, Row, L):-
  Row =.. [row|Suffix],
  append(Prefix,  Suffix, L).


skip_iri(_, Domain, _):-
  atomic_list_concat(DomainComponents, '.', Domain),
  member(DomainComponent, DomainComponents),
  member(DomainComponent, [wikidata,wikipedia]), !.

% Skip IRI based on file extension.
skip_iri(Scheme, Domain, Path):-
  uri_components(
    ResourceWithoutFragmentOrSearch,
    uri_components(Scheme,Domain,Path,_NoFragment,_NoSearch)
  ),
  file_type(Type, ResourceWithoutFragmentOrSearch),
  nonvar(Type),
  member(Type, [html,image,pdf]), !.

