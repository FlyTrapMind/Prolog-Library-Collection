:- module(
  iana_to_rdf,
  [
    assert_iana/4 % +Graph:atom
                  % +UrlPrefix:url
                  % +ResourceClass:iri
                  % +Categories:list(atom)
  ]
).

/** <module> IANA CSV to RDF

@author Wouter Beek
@version 2014/01-2014/03, 2014/05
*/

:- use_module(library(apply)).
:- use_module(library(http/http_open)).

:- use_module(xml(xml_namespace)).

:- use_module(plRdf_conv(csv_to_rdf)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').

:- rdf_meta(assert_iana(+,+,r,+)).
:- rdf_meta(assert_iana_category(+,+,r,+)).
:- rdf_meta(assert_iana_schema(+,r)).



%! assert_iana(
%!   +Graph:atom,
%!   +UrlPrefix:url,
%!   +ResourceClass:iri,
%!   +Categories:list(atom)
%! ) is det.

assert_iana(Graph, UrlPrefix, ResourceClass, Categories):-
  maplist(assert_iana_category(Graph, UrlPrefix, ResourceClass), Categories).


assert_iana_category(Graph, UrlPrefix, ResourceClass, Category):-
  atomic_list_concat([UrlPrefix,Category,'.csv'], Url),
  setup_call_cleanup(
    http_open(Url, Stream, []),
    (
      set_stream(Stream, reposition(true)),
      csv_to_rdf(Stream, Graph, iana, ResourceClass)
    ),
    close(Stream)
  ).

