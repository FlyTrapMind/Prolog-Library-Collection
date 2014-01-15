:- module(rdf_tabular, []).

/** <module> RDF tabular

Generated RDF HTML tables.

@author Wouter Beek
@tbd Add blank node map.
@tbd Add namespace legend.
@tbd Add local/remote distinction.
@tbd Include images.
@version 2013/12-2014/01
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(meta_ext)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_dcg)).
:- use_module(rdf(rdf_image)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_html)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).

:- initialization(web_module_add('RDF Tabular', rdf_tabular, rdf_tabular)).

:- http_handler(root(rdf_tabular), rdf_tabular, []).



rdf_tabular(Request):-
  memberchk(search(Search), Request),
  memberchk(term=Term, Search), !,
  reply_html_page(
    app_style,
    title(['Overview of resource ',Term]),
    \rdf_tabular(Term)
  ).
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
    setoff([Instance], rdfs_individual_of(Instance, H2), Instances),
    length(Top1, 50),
    (append(Top1, _, Instances) -> Top2 = Top1 ; Top2 = Instances)
  },
  html(
    \html_table(
      [
        caption(Caption),
        cell_dcg(rdf_html_term),
        header(true),
        indexed(true)
      ],
      [['Instance']|Top2]
    )
  ),
  overview(T).


rdf_tabular(Term) -->
  {
    dcg_phrase(rdf_term(S1), Term),
    rdf_global_id(S1, S2)
  },
  rdf_tabular_pl(S2).

% Datatype (in typed literal).
rdf_tabular_pl(D) -->
  {
    rdf_datatype(_, D), !,
    setoff([Value], rdf_datatype(_, _, D, Value, _), Values),
    format(atom(Caption), 'Ordered value list for datatype ~w.', [D])
  },
  html_table(
    [caption(Caption),cell_dcg(rdf_html_term),header(true),indexed(true)],
    [['Value']|Values]
  ).
% Predicate term.
rdf_tabular_pl(P) -->
  {
    O0 = [cell_dcg(rdf_html_term),header(true),indexed(true)],
    rdf(_, P, _), !,
    format(atom(Caption1), 'Domain of property ~w.', [P]),
    merge_options([caption(Caption1)], O0, O1),
    setoff([C], (rdf(S, P, _), rdfs_individual_of(S, C)), Cs1),
    format(atom(Caption2), 'Range of property ~w.', [P]),
    merge_options([caption(Caption2)], O0, O2),
    setoff([C], (rdf(_, P, O), rdfs_individual_of(O, C)), Cs2)
  },
  html_table(O1, [['Class']|Cs1]),
  html_table(O2, [['Class']|Cs2]),
  
  % For literal ranges we also display the values that occur.
  {
    setoff([Value], value_for_p(P, Value), Values),
    format(atom(Caption3), 'Values that occur for property ~w.', [P]),
    merge_options([caption(Caption3)], O0, O3),
    length(Values, Length)
  },
  (
    % Do not display tables that are crazy big.
    {Length > 1000}, !
  ;
    html_table(O3, [['Value']|Values])
  ).
% Subject term.
rdf_tabular_pl(S) -->
  {
    setoff([P,O,G], rdf(S, P, O, G), L),
    format(atom(Caption), 'Description of resource denoted by ~w.', [S])
  },
  rdf_html_table(Caption, L).

value_for_p(P, Value):-
  rdf(_, P, literal(type(_,Value))).
value_for_p(P, Value):-
  rdf(_, P, literal(lang(_,Value))).
value_for_p(P, Value):-
  rdf(_, P, literal(Value)),
  \+ compound(Value).

