:- module(
  'SPARQL_cache_it',
  [
  ]
).

/** <module> SPARQL cache it

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module('LOD'('LOD_query')).
:- use_module(os(file_ext)).
:- use_module(rdf_web(rdf_table)).
:- use_module('SPARQL'('SPARQL_db')).

:- debug('SPARQL_cache_it').



%! 'SPARQL_cache_iterative'(
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list)
%! ) is det.

'SPARQL_cache_iterative'([], [], []):- !.
'SPARQL_cache_iterative'([H|T], Resources, Propositions):- !,
  'SPARQL_cache_iterative'([H|T], [H], Resources, [], Propositions),
  % DEB
  findall(
    [S,P,O,none],
    member([S,P,O], Propositions),
    Quadruples
  ),
  rdf_store_table(Quadruples).
'SPARQL_cache_iterative'(Resource, Resources, Propositions):-
  'SPARQL_cache_iterative'([Resource], Resources, Propositions).

% Blank node.
'SPARQL_cache_iterative'([H|T], Vs, VSol, Props, PropsSol):-
  rdf_is_bnode(H), !,
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).

% Literal.
'SPARQL_cache_iterative'([H|T], Vs, VSol, Props, PropsSol):-
  rdf_is_literal(H), !,
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).

% Skip by file extension.
'SPARQL_cache_iterative'([H1|T], Vs, VSol, Props, PropsSol):-
  uri_components(H1, uri_components(Scheme,Domain,Path,_Fragment,_Search)),
  uri_components(H2, uri_components(Scheme,Domain,Path,_NoFragment,_NoSearch)),
  file_name_type(_, Type, H2),
  memberchk(Type, [html,image,pdf]), !,
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).

% Process IRI.
'SPARQL_cache_iterative'([H1|T1], Vs1, VSol, Props1, PropsSol):-
  message('Resource ~w', [H1]),
  'LOD_query'(H1, Neighbors, NeighborProps),

  % Filter on propositions that are included in results.
  exclude(old_proposition, NeighborProps, NewProps),
  length(NewProps, NumberOfNewProps),
  message('~d propositions added', [NumberOfNewProps]),

  % Filter on resources that have to be visited.
  exclude(old_neighbor(Vs1, NewProps), Neighbors, NewNeighbors),
  length(NewNeighbors, NumberOfNewNeighbors),
  message('~d resources added', [NumberOfNewNeighbors]),

  % Update resources that have to be visiterd.
  append(T1, NewNeighbors, T2),

  % Update results.
  ord_union(Vs1, Neighbors, Vs2),
  ord_union(Props1, NewProps, Props2),

  % Recurse.
  'SPARQL_cache_iterative'(T2, Vs2, VSol, Props2, PropsSol).

% The show must go on!
'SPARQL_cache_iterative'([H|T], Vs, VSol, Props, PropsSol):-
  message('~w failed', [H]),
  'SPARQL_cache_iterative'(T, Vs, VSol, Props, PropsSol).

old_neighbor(Vs1, _, Element):-
  memberchk(Element, Vs1), !.
old_neighbor(_, NewProps, Element):-
  member(
    [_,'http://dbpedia.org/ontology/wikiPageExternalLink',Element],
    NewProps
  ), !.

old_proposition([S,P,O]):-
  rdf(S, P, O), !.
old_proposition([S,P,O]):-
  rdf_predicate_property(P, symmetric(true)),
  rdf(O, P, S), !.


message(Format, Args):-
  debug('SPARQL_cache_it', Format, Args),
  format(user_output, Format, Args),
  nl(user_output),
  flush_output(user_output).

