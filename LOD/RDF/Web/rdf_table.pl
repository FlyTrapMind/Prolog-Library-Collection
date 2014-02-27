:- module(
  rdf_table,
  [
    rdf_store_table/1, % +Quadruples:list(list)
    rdf_store_table/4 % +Subject:or([bnode,iri])
                      % +Predicate:iri
                      % +Object:or([bnode,iri,literal])
                      % +Graph:atom
  ]
).

/** <module> RDF table

Allows RDF tables to be created in the terminal
 and displayed in the Web browser.

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(www_browser)).
:- use_module(rdf_web(rdf_html_table)).

:- rdf_meta(rdf_table(r,r,r,+)).

:- http_handler(root(rdf_table), rdf_table, []).

%! rdf_table(?Timestamp:positive_integer, ?Quadruples:list(list)) is nondet.

:- dynamic(rdf_table/2).



rdf_store_table(Quadruples):-
  get_time(Time),
  assert(rdf_table(Time, Quadruples)),
  http_absolute_uri(root(rdf_table), Link),
  www_open_url(Link).

rdf_store_table(S, P, O, G):-
  setoff(
    [S,P,O,G],
    rdf(S, P, O, G),
    Quadruples
  ),
  rdf_store_table(Quadruples).

rdf_table(_Request):-
  findall(
    Timestamp-Quadruples,
    rdf_table(Timestamp, Quadruples),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, [Timestamp-Quadruples|_]),
  reply_html_page(
    app_style,
    title(\rdf_table_caption(Timestamp)),
    \rdf_html_table([], rdf_table_caption(Timestamp), Quadruples)
  ).
  
rdf_table_caption(Timestamp) -->
  `RDF Table at `,
  {format_time(atom(FormattedTime), '%FT%T%:z', Timestamp)},
  atom(FormattedTime).

