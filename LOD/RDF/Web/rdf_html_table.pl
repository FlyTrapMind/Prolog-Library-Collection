:- module(
  rdf_html_table,
  [
    rdf_html_table//2 % :Caption
                      % +Rows:list(list(ground))
  ]
).

/** <module> RDF HTML table

Generates HTML tables with RDF content.

@author Wouter Beek
@version 2014/01
*/

:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(rdf_web(rdf_html_term)).



%! rdf_html_table(:Caption, +Data:list(list(ground)))// is det.
% @arg Data `[P,O,G]` or `[S,P,O,G]`.

:- meta_predicate(rdf_html_table(//,+,?,?)).
rdf_html_table(Caption, [H|T]) --> !,
  {
    same_length(H, H0),
    append(_, H0, ['Subject','Predicate','Object','Graph'])
  },
  html(
    \html_table(
      [header(true),indexed(true)],
      Caption,
      rdf_html_term,
      [H0,H|T]
    )
  ).
% Do not fail for empty data lists.
rdf_html_table(_, []) --> !, [].
