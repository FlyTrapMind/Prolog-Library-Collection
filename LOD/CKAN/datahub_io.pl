:- module(
  datahub_io,
  [
    datahub_io_ckan/3, % +Options:list(nvpair)
                       % +Predicate:atom
                       % +Arguments:list
    datahub_io_ckan_to_rdf/1 % +Options:list(nvpair)
  ]
).

/** <module> Access Datahub

# Question on syntax and semantics of object properties

Datahub/CKAN uses several types objects (e.g. organizations, packages, resources). I have problems understanding the properties these resources have.

Syntax:
1) Is there documentation available that describes the various properties an object can have?
2) Is these documentation available that identifies which properties are required and which are optional? (E.g. `id` may be required, `webstore_url` may be optional.
3) Are there restrictions for the values a property may have? Several properties have values that are strings which seem to encode dates in some ISO/RFC format. Knowing the format would be helpful. Several properties have strings that represent integers. Does this mean that the value can be a non-integer as well (mayne the string "NaN")?

Semantics:
4) Is there a (semantic) distinction between a property that has the value `@null` and a property that is absent?
5) There seem to be spurious properties in the Datahub, e.g. resource X has the `SpirosAlexiou` property. Doing a Web search on that string brings up the Datahub user page of a person named Spiros Alexiou (URL: http://datahub.io/nl/user/salexiou).

I have more examples of spurious proprties. If an overview of these is considered helpful I can make it available.

--

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ckan(ckan)). % Meta-calls.
:- use_module(ckan(ckan_api)).
:- use_module(ckan(ckan_to_rdf)).
:- use_module(dcg(dcg_content)).
:- use_module(generics(user_input)).
:- use_module(server(api_keys)).



datahub_io_ckan(O1, Predicate, Arguments):-
  options(O2),
  merge_options(O2, O1, O3),
  Call =.. [Predicate,O3|Arguments],
  call(Call).

datahub_io_ckan_to_rdf(O1):-
  options(O2),
  merge_options(O1, O2, O3),
  ckan_to_rdf(O3).

options([
  api_key(Key),
  api_version(3),
  authority(Auth),
  deprecated(true),
  paginated(true),
  scheme(Scheme)
]):-
  Auth = 'datahub.io',
  user_input('Enter the API key for datahub.io.', codes, Key),
  Scheme = http.

