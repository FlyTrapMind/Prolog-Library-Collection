% On Windows 8 I have had the pleasure of swipl defaulting to the
% =text= encoding. This did _not_ process special characters correctly.

:- set_prolog_flag(encoding, utf8).

:- use_module(library(apply)).
:- use_module(library(prolog_pack)).

:- initialization(load_pgc).

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
  
  % If there is no outer project, then PGC is the project.
  % (For debug purposes only.)
  once((
    current_predicate(project_name/1)
  ;
    assert(user:project_name('PGC'))
  )),

  % Assert the various search paths.
  assert(user:file_search_path(ap,              pgc('AP'))),
  assert(user:file_search_path(datasets,        pgc('Datasets'))),
    assert(user:file_search_path(ckan,            datasets('CKAN'))),
  assert(user:file_search_path(dcg,             pgc('DCG'))),
    assert(user:file_search_path(flp,             dcg('Formal Languages'))),
    assert(user:file_search_path(plp,             dcg('Programming Languages'))),
    assert(user:file_search_path(nlp,             dcg('NLP'))),
  assert(user:file_search_path(generics,        pgc('Generics'))),
  assert(user:file_search_path(graph_theory,    pgc('Graph Theory'))),
    assert(user:file_search_path(dgraph,          graph_theory('DGRAPH'))),
    assert(user:file_search_path(rdf_graph,       graph_theory('RDF Graph'))),
    assert(user:file_search_path(ugraph,          graph_theory('UGRAPH'))),
  assert(user:file_search_path(ilp,             pgc('ILP'))),
  assert(user:file_search_path(logic,           pgc('Logic'))),
    assert(user:file_search_path(rdf_mt,          logic('RDF MT'))),
  assert(user:file_search_path(math,            pgc('Math'))),
  assert(user:file_search_path(os,              pgc('OS'))),
  assert(user:file_search_path(programming,     pgc('Programming'))),
  assert(user:file_search_path(ps,              pgc('PS'))),
    assert(user:file_search_path(tms,             ps('TMS'))),
      assert(user:file_search_path(atms,            tms('ATMS'))),
      assert(user:file_search_path(doyle,           tms('Doyle'))),
  assert(user:file_search_path(server,          pgc('Server'))),
  assert(user:file_search_path(standards,       pgc('Standards'))),
    assert(user:file_search_path(datetime,        standards('DateTime'))),
    assert(user:file_search_path(geo,             standards('Geography'))),
    assert(user:file_search_path(gv,              standards('GraphViz'))),
    assert(user:file_search_path(html,            standards('HTML'))),
    assert(user:file_search_path(http,            standards('HTTP'))),
      assert(user:file_search_path(http_headers,    http('Headers'))),
      assert(user:file_search_path(http_parameters, http('Parameters'))),
    assert(user:file_search_path(lang,            standards('Language'))),
    assert(user:file_search_path(owl,             standards('OWL'))),
    assert(user:file_search_path(rdf,             standards('RDF'))),
      assert(user:file_search_path(rdf_conv,        rdf('Conversion'))),
      assert(user:file_search_path(rdf_reasoning,   rdf('Reasoning'))),
      assert(user:file_search_path(rdf_web,         rdf('Web'))),
    assert(user:file_search_path(rdfs,            standards('RDFS'))),
    assert(user:file_search_path(sparql,          standards('SPARQL'))),
    assert(user:file_search_path(svg,             standards('SVG'))),
    assert(user:file_search_path(tests,           standards('Tests'))),
    assert(user:file_search_path(uri,             standards('URI'))),
    assert(user:file_search_path(xml,             standards('XML'))),
      assert(user:file_search_path(xsd,             xml('XSD'))),
  assert(user:file_search_path(stat,            pgc('Stats'))),
  assert(user:file_search_path(vocab,           pgc('Vocab'))),
    assert(user:file_search_path(skos,            vocab('SKOS'))),
    assert(user:file_search_path(void,            vocab('VoID'))),
  assert(user:file_search_path(web,             pgc('Web'))),
    assert(user:file_search_path(crawler,         web('Crawler'))),
  
  % Check SWI-Prolog version.
  use_module(os(swipl_ext)),
  check_prolog_version,
  
  % Initialize Web module registration.
  use_module(generics(db_ext)),
  db_add_novel(user:prolog_file_type(db, database)),
  absolute_file_name(
    project(web_modules),
    File,
    [access(write),file_type(database)]
  ),
  use_module(os(file_ext)),
  safe_delete_file(File),
  
  % Install packages.
  % This requires user interaction on the first load.
  maplist(load_pack, [regex,smtp]),
  
  % Start logging.
  use_module(generics(logging)),
  % @tbd Strange module problem again...
  logging:start_log.

load_pack(Pack):-
  catch(
    use_module(library(Pack)),
    _,
    ignore(pack_install(Pack))
  ).

