:- module(rdf_tabular_ckan, []).

/** <module> RDF Tabular CKAN

CKAN-specific configuration of RDF Tabular.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(rdf_web(rdf_tabular_class)).
:- use_module(server(web_modules)).

:- initialization(web_module_add('CKAN Tabular', rdf_tabular_ckan)).

:- http_handler(rdf_tabular(ckan), rdf_tabular_ckan, []).



rdf_tabular_ckan(_Request):-
  reply_html_page(
    app_style,
    title('CKAN class-based overview'),
    \ckan_overview
  ).

ckan_overview -->
  html([
    \rdf_tabular_class(_, ckan:'Group'),
    \rdf_tabular_class(_, ckan:'Package'),
    \rdf_tabular_class(_, ckan:'Resource'),
    \rdf_tabular_class(_, ckan:'Tag'),
    \rdf_tabular_class(_, ckan:'User')
  ]).

