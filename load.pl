% The load file for the Prolog Generics Collection.
% This assumes that the search path =project= is already defined.

load:-
  assert(user:file_search_path(datasets,     pgc('Datasets'))),
  assert(user:file_search_path(generics,     pgc('Generics'))),
  assert(user:file_search_path(graph_theory, pgc('Graph Theory'))),
  assert(user:file_search_path(math,         pgc('Math'))),
  assert(user:file_search_path(server,       pgc('Server'))),
  assert(user:file_search_path(standards,    pgc('Standards'))),
    assert(user:file_search_path(html,         standards('HTML'))),
    assert(user:file_search_path(iso,          standards('ISO'))),
    assert(user:file_search_path(owl,          standards('OWL'))),
    assert(user:file_search_path(rdf,          standards('RDF'))),
    assert(user:file_search_path(rdfs,         standards('RDFS'))),
    assert(user:file_search_path(sparql,       standards('SPARQL'))),
    assert(user:file_search_path(svg,          standards('SVG'))),
    assert(user:file_search_path(tests,        standards('Tests'))),
  assert(user:file_search_path(vocabularies, pgc('Vocabularies'))),
    assert(user:file_search_path(skos,         vocabularies('SKOS'))).

:- load.

