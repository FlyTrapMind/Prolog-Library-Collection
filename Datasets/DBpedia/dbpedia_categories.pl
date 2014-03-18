:- module(
  dbpedia_categories,
  [
    dbpedia_load_categories/0
  ]
).

/** <module> DBpedia categories

DBpedia publishes the following three kind of files about categories:
  * *|Articles Categories|*
    Links from concepts to categories using the SKOS vocabulary.
  * *|Categories (Labels)|*
    Labels for Categories.
  * *|Categories (Skos)|*
    Information which concept is a category and how categories are
    related using the SKOS Vocabulary.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(generics(uri_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(archive_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- initialization(dbpedia_load_categories).



%! dbpedia_load_categories is det.
% Loads the SKOS hierarchy of categories.

dbpedia_load_categories:-
  dbpedia_categories_url(Url),
  dbpedia_load_categories(Url).

% Load the hierarchy.
dbpedia_load_categories(Url):-
  url_to_file_name(Url, File1),
  file_name_extension(File2, bz2, File1),
  exists_file(File2), !,
  access_file(File2, read),
  rdf_load(File2, [format(turtle),graph(dbpedia_categories)]).
dbpedia_load_categories(Url):-
  download_to_file([], Url, File),
  extract_archive(File),
  dbpedia_load_categories(Url).


dbpedia_categories_url('http://downloads.dbpedia.org/3.9/en/skos_categories_en.ttl.bz2').

