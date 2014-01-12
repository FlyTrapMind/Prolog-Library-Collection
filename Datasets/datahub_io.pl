:- module(
  datahub_io,
  [
    ckan/2, % +Predicate:atom
            % +Arguments:list
    ckan_to_rdf/0
  ]
).

/** <module> Access Datahub

@author Wouter Beek
@version 2014/01
*/

:- use_module(datasets(ckan)). % Meta-calls.
:- use_module(datasets(ckan_to_rdf)).
:- use_module(server(api_keys)).



ckan(Predicate, Arguments):-
  options(O1),
  Call =.. [Predicate,O1|Arguments],
  call(Call).

ckan_to_rdf:-
  options(O1),
  ckan_to_rdf(O1).

options([
  api_key(Key),
  authority(Auth),
  deprecated(true),
  paginated(true),
  scheme(Scheme)
]):-
  Auth = 'datahub.io',
  current_api_key('datahub.io', ckan, Key),
  Scheme = http.

