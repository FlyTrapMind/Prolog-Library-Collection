:- module(
  crawler,
  [
    crawl/1, % +Local:uri
    reset_crawler/0,
    unvisited/1, % -Link:uri
    visited/1 % -Link:uri
  ]
).

/** <module> Crawler

Crawler for HTML sites.

@author Wouter Beek
@version 2012/09
*/

:- use_module(server(link_collection)).
:- use_module(server(parser)).
:- use_module(standards(html)).

:- dynamic(unvisited/1).
:- dynamic(visited/1).



crawl:-
  unvisited(Url),
  store_new_uri(Url),
  download_html(Url, Dom, []),
  parse_dom(Dom),
  retract(unvisited(Url)),
  assert(visited(Url)),
  crawl.

crawl(Local):-
  reset_crawler,
  assert(unvisited(Local)),
  crawl.

reset_crawler:-
  retractall(unvisited(_)),
  retractall(visited(_)).

