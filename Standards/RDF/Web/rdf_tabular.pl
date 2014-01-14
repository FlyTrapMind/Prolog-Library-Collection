:- module(
  rdf_tabular,
  [
  ]
).

/** <module> RDF tabular

Generated RDF HTML tables.

@author Wouter Beek
@tbd Add blank node map.
@tbd Add namespace legend.
@tbd Add local/remote distinction.
@tbd Include images.
@version 2013/12-2014/01
*/

:- use_module(generics(meta_ext)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_web)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

:- initialization(web_module_add('RDF Tabular', rdf_tabular, rdf_tabular)).

:- http_handler(root(rdf_tabular), rdf_tabular, []).



rdf_tabular(Request):-
gtrace,
  memberchk(search(Search), Request),
  memberchk(term=Term1, Search), !,
  rdf_web_argument(Term1, Term2),
  format(atom(Caption), 'Description of resource denoted by ~w.', [Term1]),
  reply_html_page(app_style, title(Caption), \rdf_tabular(Caption, Term2)).
rdf_tabular(_):-
  reply_html_page(app_style, title('Overview of resources.'), \overview).


overview -->
  overview([
    ckan:'Organization',
    ckan:'Package',
    ckan:'Resource',
    ckan:'Tag',
    ckan:'User'
  ]).

overview([]) --> [].
overview([H1|T]) -->
  {
    rdf_global_id(H1, H2),
    with_output_to(atom(Name), rdf_term_name(H2)),
    format(atom(Caption), 'Instances of ~w.', [Name]),
    setoff([Instance], rdfs_individual_of(Instance, H2), Instances)
  },
  html(
    \html_table(
      [
        caption(Caption),
        cell_dcg(rdf_linked_term),
        header(true),
        indexed(true)
      ],
      [['Instance']|Instances]
    )
  ),
  overview(T).


rdf_tabular(Caption, S) -->
  {setoff([P,O,G], rdf(S, P, O, G), L)},
  rdf_table_(Caption, L).

