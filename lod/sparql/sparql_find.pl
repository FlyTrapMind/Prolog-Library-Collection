:- module(
  sparql_find,
  [
    sparql_find/3 % +Remote:atom
                    % +SearchTerm:or([atom,iri])
                    % -Resource:iri
  ]
).

/** <module> SPARQL find

Find a single resource based on a search term.

@author Wouter Beek
@version 2014/01, 2014/06
*/

:- use_module(generics(typecheck)).
:- use_module(library(debug)).
:- use_module(sparql(sparql_cache)).



%! sparql_find(
%!   +Endpoint:atom,
%!   +SearchTerm:or([atom,iri]),
%!   -Resource:iri
%! ) is det.
% Returns the resource that best fits the given search term.
%
% If the search term is itself a concept, then this is returned.
% Otherwise, the remote is searched for a resource that is labeled with
%  the given search term.

sparql_find(Endpoint, Resource, Resource):-
  is_of_type(iri, Resource), !,
  % @tbd This can be done more efficiently by just looking for
  %      the first triple.
  sparql_select(Endpoint, _, [], true, [p,o],
      [rdf(iri(Resource),var(p),var(o))], 1, _, _, Rows),
  
  (
    Rows == []
  ->
    debug(sparql_find, 'No results for resource ~w.', [Resource])
  ;
    Rows = [row(Resource)]
  ).
sparql_find(Endpoint, SearchTerm, Resource):-
  sparql_select(Endpoint, _, [rdfs], true, [resource],
      [rdf(var(resource),rdfs:label,var(label)),
       filter(regex(var(label),at_start(SearchTerm),[case_insensitive]))],
      inf, _, _, Rows),
  
  (
    Rows = []
  ->
    debug(sparql_find, 'Could not find a resource for \'~w\'.', [SearchTerm]),
    fail
  ;
    Rows = [row(Resource)|_]
  ).

