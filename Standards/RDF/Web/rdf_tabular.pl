:- module(
  rdf_tabular,
  [
    overview_instances//1, % +Resources:list(iri)
    rdf_tabular/2 % +Request:list
                  % :Content
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

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_dcg)).
:- use_module(rdf(rdf_image)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_html)).
:- use_module(server(app_ui)).

:- meta_predicate(rdf_tabular(+,//)).
:- meta_predicate(rdf_tabular_body(//,?,?)).



rdf_tabular(Request, _):-
  memberchk(search(Search), Request),
  memberchk(term=Term, Search), !,
  reply_html_page(
    app_style,
    title(['Overview of resource ',Term]),
    \rdf_tabular(Term)
  ).
rdf_tabular(_, Content):-
  reply_html_page(
    app_style,
    title('Overview of resources.'),
    \rdf_tabular_body(Content)
  ).

rdf_tabular_body(Content) -->
  dcg_call(Content).



% CLASSES %

overview_class(Class1) -->
  {
    rdf_global_id(Class1, Class2),
    with_output_to(atom(Name), rdf_term_name(Class2)),
    format(atom(Caption), 'Instances of ~w.', [Name]),
    setoff([Instance], rdfs_individual_of(Instance, Class2), Instances1),
    list_truncate(Instances1, 50, Instances2)
  },
  html_table(
    [caption(Caption),cell_dcg(rdf_html_term),header(true),indexed(true)],
    [['Instance']|Instances2]
  ).



% INSTANCES %

overview_instance(Instance1) -->
  {
    rdf_global_id(Instance1, Instance2),
    with_output_to(atom(Name), rdf_term_name(Instance2)),
    format(atom(Caption), 'Instance ~w.', [Name]),
    setoff([P,O,G], rdf(Instance2, P, O, G), L)
  },
  rdf_html_table(Caption, L).


overview_instances(L1) -->
  {
    % Order all resources based on the number of triples describing them.
    findall(
      N-R,
      (
        member(R, L1),
        rdf_estimate_complexity(R, _, _, N)
      ),
      P1
    ),
    keysort(P1, P2),
    reverse(P2, P3),
    pairs_values(P3, L2)
  },
  overview_instances_(L2).


overview_instances_([]) --> [].
overview_instances_([H|T]) -->
  overview_instance(H),
  overview_instances_(T).


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

