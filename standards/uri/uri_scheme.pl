:- module(
  iana,
  [
    iana_uri_scheme/1 % ?Scheme:atom
  ]
).

/** <module> URI Scheme

IANA-registered URI schemes.

@author Wouter Beek
@version 2014/02-2014/03, 2014/05
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_ser(rdf_serial)).

:- initialization(init_uri_scheme).

init_uri_scheme:-
  absolute_file_name(
    data(uri_scheme),
    File,
    [access(read),file_errors(fail),file_type(ntriples)]
  ), !,
  rdf_load(File, [format(ntriples),graph(uri_scheme)]).
init_uri_scheme:-
  iana_scrape_uri_scheme(uri_scheme),
  absolute_file_name(
    data(uri_scheme),
    File,
    [access(write),file_type(ntriples)]
  ),
  rdf_save([format(ntriples)], uri_scheme, File),
  init_uri_scheme.

