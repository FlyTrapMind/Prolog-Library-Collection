:- module(rdf_mt_test, []).

/** <module> RDF_MT_TEST

Tests for RDFS model theory.

@author Wouter Beek
@version 2013/08
*/

:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_mt(rdf_mt_build)).
:- use_module(rdf_mt(rdf_mt_i)).
:- use_module(rdf_mt(rdf_mt_print)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(ex, 'http://www.example.com/').

:- begin_tests(rdf_mt).

test(rdf_mt, [cleanup(clean_test), setup(build_test)]):-
  test_graph(G),
  test_model(M),
  i(G, M, A),
  rdf_mt_print_model(user_output, G, M, A).

% @tbd This is in option setup/1 of test/2. Still red...
build_test:-
  test_graph(G),
  test_model(M),
  build_test_syntax(G),
  build_test_model(G, M),
  build_test_map(G, M).

build_test_map(G, M):-
  rdf_add_i_s(G, ex:a, M, 1),
  rdf_add_i_s(G, ex:b, M, 1),
  rdf_add_i_s(G, ex:c, M, 2),
  rdf_add_i_l(G, literal(type(ex:b,whatever)), M, 2).

build_test_model(G, M):-
  rdf_add_model(M),
  % Resources.
  rdf_add_resource(M, 1),
  rdf_add_resource(M, 2),
  rdf_add_plain_literals(G, M),

  % Properties.
  rdf_add_property(M, 1),
  rdf_add_i_ext(M, 1, 1, 2),
  rdf_add_i_ext(M, 1, 2, 1).

build_test_syntax(G):-
  rdf_assert(ex:a, ex:b, ex:c, G),
  rdf_assert(ex:c, ex:a, ex:a, G),
  rdf_assert(ex:c, ex:b, ex:a, G),
  rdf_assert(ex:a, ex:b, literal(type(ex:b,whatever)), G).

clean_test:-
  test_model(M),
  rdf_unload_model(M),
  test_graph(G),
  rdf_unload_graph(G).

test_graph(rdf_mt_test_graph).

test_model(rdf_mt_test_model).

:- end_tests(rdf_mt).

