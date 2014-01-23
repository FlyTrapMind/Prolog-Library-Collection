:- module(
  data_gov_uk,
  [
    data_gov_uk_ckan/2, % +Predicate:atom
                        % +Arguments:list
    data_gov_uk_ckan_to_rdf/1 % +Options:list(nvpair)
  ]
).

/** <module> Access to data.gov.uk

@author Wouter Beek
@see http://data.gov.uk
@version 2014/01
*/

:- use_module(ckan(ckan)). % Meta-calls.
:- use_module(ckan(ckan_to_rdf)).



data_gov_uk_ckan(Predicate, Arguments):-
  options(O1),
  Call =.. [Predicate,O1|Arguments],
  call(Call).

data_gov_uk_ckan_to_rdf(O1):-
  options(O2),
  merge_options(O1, O2, O3),
  ckan_to_rdf(O3).

options([
  authority(Auth),
  deprecated(true),
  paginated(true),
  scheme(Scheme)
]):-
  Auth = 'data.gov.uk',
  Scheme = http.

