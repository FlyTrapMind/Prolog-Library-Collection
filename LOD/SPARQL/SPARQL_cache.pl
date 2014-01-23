:- module(
  'SPARQL_cache',
  [
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

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_web(rdf_table)).
:- use_module('SPARQL'(row_ext)).
:- use_module('SPARQL'('SPARQL_build')).
:- use_module('SPARQL'('SPARQL_db')).
:- use_module('SPARQL'('SPARQL_ext')).
:- use_module(xml(xml_namespace)).

:- debug('SPARQL_cache').



%! 'SPARQL_cache'(
%!   +Resource:iri,
%!   -Resources:ordset(iri),
%!   -Propositions:ordset(list)
%! ) is det.
% Returns the depth-one resources and propositions for the given resource.
%
% The given resource is assumed to be located on a URL which has
%  a registered SPARQL endpoint.

'SPARQL_cache'(Resource, Resources, Propositions):-
  uri_components(Resource, uri_components(_, Domain, _, _, _)),
  'SPARQL_current_remote'(Remote, Domain, _, _), !,
  'SPARQL_cache'(Remote, Resource, Resources, Propositions).
'SPARQL_cache'(Resource, _, _):-
  debug(
    'SPARQL_cache',
    'No registered SPARQL endpoint for <~w>.',
    [Resource]
  ).


%! 'SPARQL_cache'(
%!   +Remote:atom,
%!   +Resource:iri,
%!   -Resources:ordset(iri),
%!   -Propositions:ordset(list)
%! ) is det.

'SPARQL_cache'(Remote, Resource, Resources, Propositions):-
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
  'SPARQL_enqueue'(Remote, Query, _VarNames, Rows),

  % Conversion
  rows_to_propositions([Resource], Rows, Propositions),
  ord_union(Propositions, Resources).

