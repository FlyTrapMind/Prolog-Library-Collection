:- module(
  iana_to_rdf,
  [
    assert_iana/4 % +Graph:atom
                  % +URL_Prefix:atom
                  % +ParentClass:iri
                  % +Categories:list(atom)
  ]
).

/** <module> IANA CSV to RDF

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(generics(uri_ext)).
:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').

:- rdf_meta(assert_iana(+,+,r,+)).
:- rdf_meta(assert_iana_category(+,+,r,+)).
:- rdf_meta(assert_iana_schema(+,r)).



%! assert_iana(
%!   +Graph:atom,
%!   +URL_Prefix:atom,
%!   +ParentClass:iri,
%!   +Categories:list(atom)
%! ) is det.

assert_iana(Graph, URL_Prefix, ParentClass, Categories):-
  assert_iana_schema(Graph, ParentClass),
  maplist(assert_iana_category(Graph, URL_Prefix, ParentClass), Categories).


assert_iana_category(Graph, URL_Prefix, ParentClass, Category):-
  rdf_global_id(iana:Category, Class),
  rdfs_assert_subclass(Class, ParentClass, Graph),
  rdfs_assert_label(Class, Category, Graph),

  atomic_list_concat([URL_Prefix,Category,'.csv'], URL),
  setup_call_cleanup(
    (
      download_to_file([], URL, File),
      csv_read_file(File, [_|Rows])
    ),
    maplist(assert_iana_row(Graph, Class), Rows),
    delete_file(File)
  ).


assert_iana_row(Graph, Class, Row):-
  rdf_bnode(Registration),
  rdf_assert_individual(Registration, Class, Graph),
  assert_iana_row1(Graph, Registration, Row).

assert_iana_row1(
  Graph,
  Registration,
  row(Name,Template,Description,Reference)
):-
  assert_iana_row1(Graph, Registration, row(Name,Template,Reference)),
  (
    Description == '', !
  ;
    rdf_assert_datatype(
      Registration,
      iana:description,
      xsd:string,
      Description,
      Graph
    )
  ), !.
assert_iana_row1(Graph, Registration, row(Name,Template,Reference)):-
  (
    Template == '', !
  ;
    rdf_assert_datatype(
      Registration,
      iana:template,
      xsd:string,
      Template,
      Graph
    )
  ),
  (
    Reference == '', !
  ;
    rdf_assert_datatype(
      Registration,
      iana:reference,
      xsd:string,
      Reference,
      Graph
    )
  ),
  rdfs_assert_label(Registration, Name, Graph).


assert_iana_schema(Graph, Class):-
  % Class registration.
  rdfs_assert_class(iana:'Registration', Graph),
  rdfs_assert_label(iana:'Registration', 'IANA registration', Graph),
  rdfs_assert_subclass(Class, iana:'Registration', Graph),

  % Property description.
  rdf_assert_property(iana:description, Graph),
  rdfs_assert_domain(iana:description, iana:'Registration', Graph),
  rdfs_assert_range(iana:description, xsd:string, Graph),
  rdfs_assert_label(iana:description, description, Graph),

  % Property template.
  rdf_assert_property(iana:template, Graph),
  rdfs_assert_domain(iana:template, iana:'Registration', Graph),
  rdfs_assert_range(iana:template, xsd:string, Graph),
  rdfs_assert_label(iana:template, template, Graph),

  % Property reference.
  rdf_assert_property(iana:reference, Graph),
  rdfs_assert_domain(iana:reference, iana:'Registration', Graph),
  rdfs_assert_range(iana:reference, xsd:string, Graph),
  rdfs_assert_label(iana:reference, reference, Graph).

