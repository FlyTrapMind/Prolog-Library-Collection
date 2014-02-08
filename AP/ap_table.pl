:- module(
  ap_table,
  [
    ap_register_header/2, % +Alias:atom
                          % +Header:list(atom)
    ap_register_row/2 % +Alias:atom
                      % +Row:list(compound)
  ]
).

/** <module> AP table

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ap(html_ap_term)). % Used in meta-argument of html_table//4.
:- use_module(dcg(dcg_content)).
:- use_module(generics(uri_ext)).
:- use_module(html(html_table)).
:- use_module(html(html_pl_term)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

:- http_handler(root(ap_table), ap_table, []).

:- web_module_add('AP table', ap_table).

:- dynamic(header/2).
:- dynamic(row/2).



ap_register_header(Alias, Header):-
  assert(header(Alias, Header)).

ap_register_row(Alias, Row):-
  assert(row(Alias, Row)).

% Show the results for the given AP alias.
ap_table(Request):-
  memberchk(search(Search), Request),
  memberchk(alias=Alias, Search), !,
  
  once(header(Alias, Header)),
  findall(
    Row,
    row(Alias, Row),
    Rows
  ),
  reply_html_page(
    app_style,
    title('AP results'),
    \html_table(
      [header_row(true),indexed(true)],
      (`Results of automated process `, atom(Alias)),
      html_ap_term,
      [Header|Rows]
    )
  ).
% Show all AP aliases.
ap_table(_Request):-
  findall(
    [ap_alias(Alias)],
    row(Alias, _),
    Aliases
  ),
  reply_html_page(
    app_style,
    title('AP results'),
    \html_table(
      [header_row(true),indexed(true)],
      `Overview of automated processes.`,
      html_ap_term,
      [['Alias']|Aliases]
    )
  ).

