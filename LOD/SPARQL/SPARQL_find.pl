:- module(
  'SPARQL_find',
  [
    'SPARQL_find'/3 % +Remote:atom
                    % +SearchTerm:or([atom,iri])
                    % -Resource:iri
  ]
).

/** <module> SPARQL find

Find a single resource based on a search term.

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(typecheck)).
:- use_module(library(debug)).
:- use_module('SPARQL'('SPARQL_build')).
:- use_module('SPARQL'('SPARQL_cache')).
:- use_module('SPARQL'('SPARQL_ext')).



%! 'SPARQL_find'(
%!   +Remote:atom,
%!   +SearchTerm:or([atom,iri]),
%!   -Resource:iri
%! ) is det.
% Returns the resource that best fits the given search term.
%
% If the search term is itself a concept, then this is returned.
% Otherwise, the remote is searched for a resource that is labeled with
%  the given search term.
%
% @arg Remote
% @arg SearchTerm
% @arg Resource

'SPARQL_find'(Remote, Resource, Resource):-
  is_of_type(iri, Resource), !,
  % @tbd This can be done more efficiently by just looking for
  %      the first triple.
  phrase(
      'SPARQL_formulate'(
	  _,
	  _,
	  [],
	  select,
	  true,
	  [p,o],
	  [rdf(iri(Resource), var(p), var(o))],
	  1,
    _,
	  _
      ),
      Query
  ),
  'SPARQL_query'(Remote, Query, _VarNames, Results),
  (
    Results == []
  ->
		  debug('SPARQL_find', 'No results for resource ~w.', [Resource])
		    ;
		    true
		).
'SPARQL_find'(Remote, SearchTerm, Resource):-
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [rdfs],
      select,
      true,
      [resource],
      [
        rdf(var(resource), rdfs:label, var(label)),
        filter(regex(var(label), at_start(SearchTerm), [case_insensitive]))
      ],
      inf,
      _,
      _
    ),
    Query
  ),
  'SPARQL_query'(Remote, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug('SPARQL_find', 'Could not find a resource for \'~w\'.', [SearchTerm]),
    fail
  ;
    Resources = [row(Resource)|_]
  ).

