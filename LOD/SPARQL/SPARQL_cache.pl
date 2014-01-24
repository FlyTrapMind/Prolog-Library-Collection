:- module(
  'SPARQL_cache',
  [
    'SPARQL_cache'/2, % +Resource:or([bnode,iri,literal])
                      % +Graph:atom
    'SPARQL_cache'/3 % +Resource:or([bnode,iri,literal])
                     % -Resources:ordset(or([bnode,iri,literal]))
                     % -Propositions:ordset(list)
  ]
).

/** <module> SPARQL Cache

Locally caches triples that are relevant for specific resources.

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module('SPARQL'(row_ext)).
:- use_module('SPARQL'('SPARQL_build')).
:- use_module('SPARQL'('SPARQL_db')).
:- use_module('SPARQL'('SPARQL_ext')).

:- debug('SPARQL_cache').



%! 'SPARQL_cache'(+Resource:iri, +Graph:atom) is det.

'SPARQL_cache'(Resource, Graph):-
  'SPARQL_cache'(Resource, _, Propositions),
  maplist(assert_proposition(Graph), Propositions).

assert_proposition(Graph, [S,P,O]):-
  rdf_assert(S, P, O, Graph).


%! 'SPARQL_cache'(
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.
% Returns the depth-one resources and propositions for the given resource.
%
% The given resource is assumed to be located on a URL which has
%  a registered SPARQL endpoint.

% Blank node.
'SPARQL_cache'(Resource, [], []):-
  rdf_is_bnode(Resource), !.

% Literal.
'SPARQL_cache'(Resource, [], []):-
  rdf_is_literal(Resource), !.

% Skip IRI by file extension.
'SPARQL_cache'(Resource1, [], []):-
  uri_components(
    Resource1,
    uri_components(Scheme,Domain,Path,_Fragment,_Search)
  ),
  uri_components(
    Resource2,
    uri_components(Scheme,Domain,Path,_NoFragment,_NoSearch)
  ),
  file_name_type(_, Type, Resource2),
  memberchk(Type, [html,image,pdf]), !.

% IRI with registered SPARQL endpoint.
'SPARQL_cache'(Resource, Resources, Propositions):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  'SPARQL_current_remote_domain'(Remote, Domain), !,
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource), var(p), var(o))],
      inf,
      _
    ),
    Query
  ),
  'SPARQL_query'(Remote, Query, _VarNames, Rows),

  % Conversion
  rows_to_propositions([Resource], Rows, Propositions),
  ord_union(Propositions, Resources).

