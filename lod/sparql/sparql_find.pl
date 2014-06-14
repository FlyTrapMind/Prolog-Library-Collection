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

:- use_module(plSparql(sparql_api)).
:- use_module(plSparql(sparql_cache)).



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
  sparql_ask(Endpoint, [], [rdf(iri(Resource),var(p),var(o))], []).
sparql_find(Endpoint, SearchTerm, Resource):-
  sparql_select(Endpoint, [rdfs], [resource], [
      rdf(var(resource),rdfs:label,var(label)),
      filter(regex(var(label),at_start(SearchTerm),[case_insensitive]))],
      [Resource], [distinct(true),limit(1)]).

