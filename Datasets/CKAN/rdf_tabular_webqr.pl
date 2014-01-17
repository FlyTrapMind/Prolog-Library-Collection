:- module(rdf_tabular_ckan, []).

/** <module> RDF Tabular CKAN

CKAN-specific configuration of RDF Tabular.

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(http/http_dispatch)).
:- use_module(rdf_web(rdf_tabular)).
:- use_module(server(web_modules)).

:- initialization(web_module_add('CKAN Tabular', rdf_tabular, rdf_tabular_ckan)).

:- http_handler(root(rdf_tabular), rdf_tabular, []).


rdf_tabular_ckan(Request):-
  rdf_tabular(Request, ckan_overview).

ckan_overview -->
  overview_class(ckan:'Organization'),
  overview_class(ckan:'Package'),
  overview_class(ckan:'Resource'),
  overview_class(ckan:'Tag'),
  overview_class(ckan:'User').
