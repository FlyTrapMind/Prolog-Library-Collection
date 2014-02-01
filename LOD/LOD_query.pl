:- module(
  'LOD_query',
  [
    'LOD_cache'/2, % +Resource:or([bnode,iri,literal])
                   % +Graph:atom
    'LOD_cache'/3, % +Resource:or([bnode,iri,literal])
                   % -Resources:ordset(or([bnode,iri,literal]))
                   % -Propositions:ordset(list(or([bnode,iri,literal])))
    'LOD_local_query'/6 % +Options:list(nvpair)
                        % +URL:url
                        % ?Graph:atom
                        % +Resource:or([bnode,iri,literal])
                        % -Resources:ordset(or([bnode,iri,literal]))
                        % -Propositions:ordset(list(or([bnode,iri,literal])))
  ]
).

/** <module> LOD query

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_module('LOD'(flickrwrappr)).
:- use_module('LOD'('LOD_location')).
:- use_module(os(file_mime)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_serial)).
:- use_module('SPARQL'('SPARQL_cache')).



'LOD_cache'(IRI, Graph):-
  'LOD_cache'(IRI, _, Propositions),
  forall(
    member([S,P,O], Propositions),
    rdf_assert(S, P, O, Graph)
  ).


% Flickrwrappr LOD description.
% We extract these later, see module [flickrwrappr].
'LOD_cache'(URL, [], []):-
  is_flickrwrappr_url(URL), !.

% Query a registered SPARQL endpoint.
'LOD_cache'(IRI, Resources, Propositions):-
  'SPARQL_cache'(IRI, Resources, Propositions), !.

% Download a LOD description based on the IRI prefix.
'LOD_cache'(IRI, Resources, Propositions):-
  rdf_global_id(Prefix:_, IRI),
  (
    'LOD_location'(Prefix, URL), !
  ;
    URL = IRI
  ),
  'LOD_local_query'([], URL, Prefix, IRI, Resources, Propositions).

% Based on the entire IRI we can download a LOD description.
% Sometimes we need special HTTP request headers in order to receive
% machine-readable content upon dereferencing.
'LOD_cache'(IRI, Resources, Propositions):-
  is_of_type(uri, IRI), !,
  rdf_global_id(Prefix:_, IRI),
  findall(
    request_header(N=V),
    'LOD_header'(Prefix, N, V),
    O1
  ),
  'LOD_local_query'(O1, IRI, _NoGraph, IRI, Resources, Propositions).


%! 'LOD_local_query'(
%!   +Options:list(nvpair),
%!   +URL:url,
%!   +Graph:atom,
%!   +Resource:or([bnode,iri,literal]),
%!   -Resources:ordset(or([bnode,iri,literal])),
%!   -Propositions:ordset(list(or([bnode,iri,literal])))
%! ) is det.
% The options are passed to download_to_file/3 -> http_goal -> http_open/3.

'LOD_local_query'(_, _, Graph, Resource, Resources, Propositions):-
  rdf_graph(Graph), !,
  'LOD_local_query_on_loaded_graph'(Resource, Resources, Propositions, Graph).
'LOD_local_query'(O1, URL, Graph, Resource, Resources, Propositions):-
  catch(download_to_file(O1, URL, File), _, fail),
  'LOD_local_query_on_file'(File, Graph, Resource, Resources, Propositions).

% Potential RDF! Let's try to load it in a graph.
'LOD_local_query_on_file'(File, Graph, Resource, Resources, Propositions):-
  file_mime(File, MIME),
  rdf_mime(MIME), !,
  'LOD_local_query_on_graph'(
    File,
    MIME,
    Graph,
    Resource,
    Resources,
    Propositions
  ).
% There is no joy in this: no RDF.
'LOD_local_query_on_file'(File, _, _, [], []):-
  debug(cache_it, 'No RDF in file ~w.', [File]),
  delete_file(File).


% The graph first needs to be loaded.
'LOD_local_query_on_graph'(
  File,
  MIME,
  Graph,
  Resource,
  Resources,
  Propositions
):-
  % If graph is nonvar, it is kept.
  % If graph is var, it is erased.
  rdf_setup_call_cleanup(
    [graph(Graph),mime(MIME)],
    File,
    'LOD_local_query_on_loaded_graph'(Resource, Resources, Propositions)
  ).

'LOD_local_query_on_loaded_graph'(Resource, Resources, Propositions, Graph):-
  setoff(
    [Resource,P,O],
    rdf(Resource, P, O, Graph),
    Propositions
  ),
  ord_union(Propositions, Resources).

