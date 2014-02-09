:- module(
  html_ap_term,
  [
    html_ap_term//1 % +AP_Status:compound
  ]
).

/** <module> HTML AP term

Generates HTML for AP status compound terms.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(dcg(dcg_content)). % Used as meta-argument in nvpair//2.
:- use_module(generics(uri_ext)).
:- use_module(html(html_pl_term)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).



html_ap_term(ap(status(Status),Message)) --> !,
  {atomic_list_concat([ap,Status], '_', Class)},
  html(
    div(class=Class,
    \html_ap_message(Message))
  ).
html_ap_term(ap_alias(Alias)) --> !,
  {
    http_absolute_location(root(ap_table), Location1, []),
    uri_query_add(Location1, alias, Alias, Location2)
  },
  html(span(class='ap-alias', a(href=Location2, Alias))).
html_ap_term(PL_Term) -->
  html_pl_term(PL_Term).

% Randomize IRIs.
html_ap_message(randomize_iris) --> !,
  html(div(class=randomize_iris, 'Aap')).

% RDF conversion.
html_ap_message(rdf_conversion(Files)) --> !,
  html(
    div(class=rdf_conversion, [
      div(class=action, 'to turtle'),
      \html_files(Files)
    ])
  ).

% Other
html_ap_message(Message) -->
  html(
    span(class=ap_message,
      \html_pl_term(Message)
    )
  ).

mime(MIME) -->
  html_mime(MIME).

