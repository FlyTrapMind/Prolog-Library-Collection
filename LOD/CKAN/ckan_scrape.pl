:- module(
  ckan_scrape,
  [
    ckan_scrape/1 % ?Site:atom
  ]
).

/** <module> CKAN scrape

Automated script that scraped CKAN.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ckan(data_gov_uk)). % Meta-call.
:- use_module(ckan(datahub_io)). % Meta-call.
:- use_module(ckan(rdf_tabular_ckan)). % Load Web interface.
:- use_module(generics(db_ext)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_serial)).

:- initialization(init_ckan_scrape).
init_ckan_scrape:-
  create_nested_directory(data('CKAN')),
  db_add_novel(user:file_search_path(ckan_data, data('CKAN'))).



%ckan_site(datahub_io).
ckan_site(data_gov_uk).

ckan_scrape(Site):-
  var(Site), !,
  ckan_site(Site),
  ckan_scrape(Site).
ckan_scrape(Site):-
  rdf_graph(Site), !.
ckan_scrape(Site):-
  absolute_file_name(
    data(Site),
    File,
    [access(read),file_errors(fail),file_type(turtle)]
  ), !,
  rdf_load([format(turtle)], Site, File).
ckan_scrape(Site):-
  atomic_list_concat([Site,ckan_to_rdf], '_', Pred),
  call(Pred, [graph(Site)]),

  % Store the results of scraping.
  absolute_file_name(
    data(Site),
    File,
    [access(write),file_type(turtle)]
  ),
  rdf_save([format(turtle)], Site, File).

