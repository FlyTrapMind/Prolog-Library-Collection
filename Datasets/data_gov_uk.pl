:- module(
  data_gov_uk,
  [
    ckan/2, % +Predicate:atom
            % +Arguments:list
    ckan_to_rdf/0
  ]
).

/** <module> Access to data.gov.uk

@author Wouter Beek
@see http://data.gov.uk
@version 2014/01
*/

:- use_module(datasets(ckan)). % Meta-calls.
:- use_module(datasets(ckan_to_rdf)).



%! ckan(+Predicate:atom, +Arguments:list) is det.

ckan(Predicate, Arguments):-
  options(O1),
  Call =.. [Predicate,O1|Arguments],
  call(Call).

ckan_to_rdf:-
  options(O1),
  ckan_to_rdf(O1).

options([authority(Auth),deprecated(true),paginated(true),scheme(Scheme)]):-
  Auth = 'data.gov.uk',
  Scheme = http.

