:- module(
  uri_scheme,
  [
    uri_scheme/1 % ?Scheme:atom
  ]
).

/** <module> URI Scheme

IANA-registered URI schemes.

@author Wouter Beek
@version 2014/02
*/

:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_label_read)).
:- use_module(standards(iana_to_rdf)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').

:- initialization(init_uri_scheme).



uri_scheme(Scheme):-
  rdfs_label(Registration, _, Scheme, uri_scheme),
  rdfs_individual_of(Registration, iana:'URISchemaRegistration').


init_uri_scheme:-
  absolute_file_name(
    data(uri_scheme),
    File,
    [access(read),file_errors(fail),file_type(turtle)]
  ), !,
  rdf_load([format(turtle)], uri_scheme, File).
init_uri_scheme:-
  assert_iana(
    uri_scheme,
    'http://www.iana.org/assignments/uri-schemes/',
    iana:'URISchemaRegistration',
    ['uri-schemes-1','uri-schemes-2']
  ),
  absolute_file_name(
    data(uri_scheme),
    File,
    [access(write),file_type(turtle)]
  ),
  rdf_save([format(turtle)], uri_scheme, File),
  init_uri_scheme.

