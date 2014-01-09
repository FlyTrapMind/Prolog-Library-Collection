:- module(
  datahub_io,
  [
    ckan/2 % +Predicate:atom
           % +Arguments:list
  ]
).

/** <module> Access Datahub

@author Wouter Beek
@version 2014/01
*/

:- use_module(datasets(ckan)).
:- use_module(server(api_keys)).



ckan(Predicate, Arguments):-
  Auth = 'datahub.io',
  current_api_key('datahub.io', ckan, Key),
  Scheme = http,
  O1 = [api_key(Key),api_version(_),authority(Auth),scheme(Scheme)],
  Call =.. [Predicate,O1|Arguments],
  call(Call).

