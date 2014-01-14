:- module(
  rdf_table,
  [
    rdf_table/4 % +Subject:or([bnode,iri])
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

:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(www_browser)).
:- use_module(rdf_web(rdf_html)).

:- rdf_meta(rdf_table(r,r,r,+)).

:- http_handler(root(rdf_table), rdf_table, []).

%! rdf_table(?Time:positive_integer, ?Quadruples:list(list)) is nondet.

:- dynamic(rdf_table/2).



rdf_table(_):-
  findall(Time-L, rdf_table(Time, L), Pairs1),
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, [Time1-L|_]),
  format_time(atom(Time2), '%FT%T%:z', Time1),
  format(atom(Caption), 'RDF Table at ~w', [Time2]),
  reply_html_page(app_style, title(Caption), \rdf_html_table(Caption, L)).

rdf_table(S, P, O, G):-
  setoff([S,P,O,G], rdf(S, P, O, G), L),
  get_time(Time),
  assert(rdf_table(Time, L)),
  http_absolute_uri(root(rdf_table), Link),
  www_open_url(Link).

