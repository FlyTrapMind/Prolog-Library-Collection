:- module(
  data_gov_uk,
  [
    ckan/3 % +Options:list(nvpair)
           % +Predicate:atom
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



%! ckan(+Options:list(nvpair), +Predicate:atom, +Arguments:list) is det.

ckan(O1, Predicate, Arguments):-
  Auth = 'data.gov.uk',
  Scheme = http,
  O2 = [authority(Auth),scheme(Scheme)],
  merge_options(O1, O2, O3),
  Call =.. [Predicate,O3|Arguments],
  call(Call).

