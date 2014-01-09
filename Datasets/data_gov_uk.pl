:- module(
  data_gov_uk,
  [
    ckan/2 % +Predicate:atom
           % +Arguments:list
  ]
).

/** <module> Access to data.gov.uk

@author Wouter Beek
@see http://data.gov.uk
@version 2014/01
*/

:- use_module(datasets(ckan)). % Meta-calls.
:- use_module(library(option)).



%! ckan(+Predicate:atom, +Arguments:list) is det.

ckan(Predicate, Arguments):-
  Auth = 'data.gov.uk',
  Scheme = http,
  O1 = [authority(Auth),deprecated(true),paginated(true),scheme(Scheme)],
  Call =.. [Predicate,O1|Arguments],
  call(Call).

