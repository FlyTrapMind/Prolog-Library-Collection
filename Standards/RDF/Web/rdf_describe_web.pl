:- module(
  rdf_describe_web,
  [
    rdf_describe_web/2 % +Resource:atom
                       % -DOM:list
  ]
).

/** <module> RDF describe Web

Generates Web pages that describe a resource.

@author Wouter Beek
@tbd Add blank node map.
@tbd Add namespace legend.
@tbd Add local/remote distinction.
@tbd Include images.
@version 2013/12
*/

:- use_module(generics(meta_ext)).
:- use_module(html(html_table)).
:- use_module(http_headers(rfc2616_accept_language)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_web)).
:- use_module(server(web_modules)).

:- initialization(web_module_add('RDF DESCRIBE', rdf_describe_web, rdf_desc)).

:- http_handler(root(rdf_desc), rdf_describe, []).



%! rdf_describe(+Request:list) is det.
% Example:
% ~~~{.url}
% http://localhost:5000/rdf_desc?resource=dbpedia:Monkey
% ~~~

rdf_describe(Request):-
  memberchk(search(Search), Request),
  memberchk(resource=R1, Search),
  rdf_web_argument(R1, R2),
  reply_html_page(app_style, \rdf_describe_head(R1), \rdf_describe_body(R2)).

rdf_describe_body(R) -->
  {
    setoff(
      [P2,O2],
      (
        rdf(R, P1, O1),
        maplist(rdf_term_name([]), [P1,O1], [P2,O2])
      ),
      PO_Pairs
    )
  },
  html(
    \html_table(
      [header(true),indexed(true)],
      [['Predicate','Object']|PO_Pairs]
    )
  ).

rdf_describe_head(R) -->
  html(title(['Description of resource denoted by ', R])).

%! rdf_describe_web(+Resource:atom, -DOM:list) is det.

rdf_describe_web(S1, DOM):-
  rdf_web_argument(S1, S2),
  findall(
    [P,O],
    rdf(S2, P, O),
    PO_Pairs
  ),
  format(
    atom(Caption),
    'Description of the resource denoted by ~w.',
    [S2]
  ),
  html_table(
    [caption(Caption),header(true),indexed(true)],
    [['Predicate','Object']|PO_Pairs],
    DOM
  ).

