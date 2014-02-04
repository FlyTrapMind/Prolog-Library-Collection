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
@version 2014/02
*/

:- use_module(generics(archive_ext)).
:- use_module(generics(uri_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_serial)).

:- initialization(dbpedia_load_categories).



%! dbpedia_load_categories is det.
% Loads the SKOS hierarchy of categories.

% Load the hierarchy.
dbpedia_load_categories:-
  absolute_file_name(
    data(skos_categories_en),
    File,
    [access(read),file_errors(fail),file_type(turtle)]
  ), !,
  rdf_load([format(turtle)], dbpedia_categories, File).
% First download the file from DBpedia, then load the hierarchy.
dbpedia_load_categories:-
  download_to_file(
    [],
    'http://downloads.dbpedia.org/3.9/en/skos_categories_en.ttl.bz2',
    File1
  ),
  absolute_file_name(data('.'), Dir, [access(write),file_type(directory)]),
  extract_archive(File1, Dir, _),
  directory_files(
    [
      file_types([turtle]),
      include_directories(false),
      include_self(false),
      recursive(false)
    ],
    Dir,
    [File2|_]
  ),
  create_file(data('.'), skos_categories_en, turtle, File3),
  rdf_convert_file(_, File2, 'application/x-turtle', File3),
  dbpedia_load_categories.

