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
    user:file_search_path(pgc, _)
  ;
    assert(user:file_search_path(pgc, ThisDirectory))
  )),
  % If there is not outer project, then PGC is the project.
  once((
    current_predicate(project_name/1)
  ;
    assert(user:project_name('PGC'))
  )),

  % Assert the various search paths.
  assert(user:file_search_path(datasets,     pgc('Datasets'))),
  assert(user:file_search_path(dcg,          pgc('DCG'))),
  assert(user:file_search_path(generics,     pgc('Generics'))),
  assert(user:file_search_path(graph_theory, pgc('Graph Theory'))),
    assert(user:file_search_path(dgraph,     graph_theory('DGRAPH'))),
    assert(user:file_search_path(rdf_graph,  graph_theory('RDF Graph'))),
    assert(user:file_search_path(ugraph,     graph_theory('UGRAPH'))),
  assert(user:file_search_path(ilp,          pgc('ILP'))),
  assert(user:file_search_path(logic,        pgc('Logic'))),
    assert(user:file_search_path(rdf_mt,       logic('RDF MT'))),
  assert(user:file_search_path(math,         pgc('Math'))),
  assert(user:file_search_path(os,           pgc('OS'))),
  assert(user:file_search_path(ps,           pgc('PS'))),
    assert(user:file_search_path(tms,          ps('TMS'))),
      assert(user:file_search_path(atms,         ps('ATMS'))),
      assert(user:file_search_path(atms,         ps('Doyle'))),
  assert(user:file_search_path(server,       pgc('Server'))),
  assert(user:file_search_path(standards,    pgc('Standards'))),
    assert(user:file_search_path(datetime,     standards('DateTime'))),
    assert(user:file_search_path(geo,          standards('Geography'))),
    assert(user:file_search_path(gv,           standards('GraphViz'))),
    assert(user:file_search_path(html,         standards('HTML'))),
    assert(user:file_search_path(http,         standards('HTTP'))),
    assert(user:file_search_path(lang,         standards('Language'))),
    assert(user:file_search_path(owl,          standards('OWL'))),
    assert(user:file_search_path(rdf,          standards('RDF'))),
    assert(user:file_search_path(rdfs,         standards('RDFS'))),
    assert(user:file_search_path(sparql,       standards('SPARQL'))),
    assert(user:file_search_path(svg,          standards('SVG'))),
    assert(user:file_search_path(tests,        standards('Tests'))),
    assert(user:file_search_path(uri,          standards('URI'))),
    assert(user:file_search_path(xml,          standards('XML'))),
      assert(user:file_search_path(xsd,          xml('XSD'))),
  assert(user:file_search_path(stat,         pgc('Stats'))),
  assert(user:file_search_path(vocab,        pgc('Vocab'))),
    assert(user:file_search_path(skos,         vocabularies('SKOS'))),
    assert(user:file_search_path(void,         vocabularies('VoID'))),
  assert(user:file_search_path(web,          pgc('Web'))),
    assert(user:file_search_path(crawler,      vocabularies('Crawler'))),
  
  % Check whether the PGC runs on the current SWI-Prolog version.
  use_module(os(swipl_ext)),
  check_prolog_version,
  
  use_module(generics(db_ext)),
  db_add_novel(user:prolog_file_type(db, database)),
  use_module(os(file_ext)),
  
  absolute_file_name(
    project(web_modules),
    File,
    [access(write),file_type(database)]
  ),
  safe_delete_file(File),
  
  % Start logging.
  use_module(generics(logging)),
  start_log,
  
  use_module(rdf(rdf_web)),
  use_module(tms(tms_web)).
:- load_pgc.

