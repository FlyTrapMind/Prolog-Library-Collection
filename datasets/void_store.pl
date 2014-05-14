:- module(
  void_store,
  [
    void_store/1 % -Iris:ordset(iri)
  ]
).

/** <module> VoID store

@author Wouter Beek
@see http://void.rkbexplorer.com/
@version 2014/05
*/

:- use_module(library(aggregate)).
:- use_module(library(xpath)).

:- use_module(xml(xml_dom)).



void_store(Iris):-
  void_store_url(Url),
  xml_url_to_dom(Url, Dom),
  aggregate_all(
    set(Iri),
    (
      xpath_chk(Dom, //div(@class=content), ContentDiv),
      xpath(ContentDiv, //li, Li),
      xpath_chk(Li, a(@href), Iri)
    ),
    Iris
  ).


void_store_url('http://void.rkbexplorer.com/browse/').

