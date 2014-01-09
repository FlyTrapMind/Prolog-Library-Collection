:- module(
  data_overheid_nl,
  [
    ckan/2 % +Predicate:atom
           % +Arguments:list
  ]
).

/** <module> Access to data.overheid.nl

@author Wouter Beek
@see https://data.overheid.nl/
@version 2014/01
*/


:- use_module(datasets(ckan)).
:- use_module(server(api_keys)).



ckan(Predicate, Arguments):-
  Auth = 'data.overheid.nl',
  current_api_key('data.overheid.nl', ckan, Key),
  Scheme = https,
  O1 = [api_key(Key),api_version(_),authority(Auth),scheme(Scheme)],
  Call =.. [Predicate,O1|Arguments],
  call(Call).

