% On Windows 8 I have had the pleasure of swipl defaulting to the
% =text= encoding. This did _not_ process special characters correctly.

:- set_prolog_flag(encoding, utf8).

% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined
% by the parent project (PGC is a library).

load_pgc:-
  source_file(load_pgc, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(pgc, ThisDirectory)),

  % If there is no outer project, then PGC is the project.
  once((
    user:file_search_path(project, _)
  ;
    assert(user:file_search_path(project, ThisDirectory))
  )),
  % If there is not outer project, then PGC is the project.
  once((
    current_predicate(project_name/1)
  ;
    assert(user:project_name('PGC'))
  )),
  
  % Do not write module loads to the standard output stream.
  set_prolog_flag(verbose_load, silent),

  % Assert the various search paths.
  assert(user:file_search_path(datasets,     pgc('Datasets'))),
    assert(user:file_search_path(dbnl,       datasets('DBNL'))),
  assert(user:file_search_path(dcg,          pgc('DCG'))),
  assert(user:file_search_path(generics,     pgc('Generics'))),
  assert(user:file_search_path(graph_theory, pgc('Graph Theory'))),
  assert(user:file_search_path(ilp,          pgc('ILP'))),
  assert(user:file_search_path(logic,        pgc('Logic'))),
  assert(user:file_search_path(math,         pgc('Math'))),
  assert(user:file_search_path(os,           pgc('OS'))),
  assert(user:file_search_path(server,       pgc('Server'))),
  assert(user:file_search_path(standards,    pgc('Standards'))),
    assert(user:file_search_path(html,         standards('HTML'))),
    assert(user:file_search_path(iso,          standards('ISO'))),
    assert(user:file_search_path(owl,          standards('OWL'))),
    assert(user:file_search_path(rdf,          standards('RDF'))),
    assert(user:file_search_path(rdfs,         standards('RDFS'))),
    assert(user:file_search_path(rfc,          standards('RFC'))),
    assert(user:file_search_path(sparql,       standards('SPARQL'))),
    assert(user:file_search_path(svg,          standards('SVG'))),
    assert(user:file_search_path(tests,        standards('Tests'))),
    assert(user:file_search_path(xml,          standards('XML'))),
  assert(user:file_search_path(tms,          pgc('TMS'))),
  assert(user:file_search_path(vocabularies, pgc('Vocabularies'))),
    assert(user:file_search_path(skos,         vocabularies('SKOS'))),
  
  % Start logging.
  use_module(generics(logging)),
  start_log,

  % Start Wallace.
  use_module(server(wallace)),
  start_wallace.
:- load_pgc.

