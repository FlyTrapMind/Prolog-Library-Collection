:- module(
  rdf_tabular,
  [
    overview_class//1, % +Class:iri
    overview_instances//2, % +Resources:list(iri)
                           % +Grap:atom
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

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_hierarchy)).
:- use_module(rdf(rdf_image)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).
:- use_module(tms(tms)).

:- meta_predicate(rdf_tabular(+,//)).
:- meta_predicate(rdf_tabular_body(//,?,?)).

http:location(rdf_tabular, root(rdf_tabular), []).
:- http_handler(root(rdf_tabular), rdf_tabular_default, [priority(-1)]).

:- initialization(web_module_add('RDF Tabular', rdf_tabular_default)).



rdf_tabular_default(Request):-
  findall(G, (rdf_graph(G), \+tms(G)), Gs),
  rdf_global_id(rdfs:subClassOf, P),
  rdf_tabular(Request, rdf_export_hierarchy(Gs, P)).

rdf_tabular(Request, _):-
  memberchk(search(Search), Request),
  memberchk(term=Term, Search), !,
  reply_html_page(
    app_style,
    title(['Overview of resource ',Term]),
    \rdf_tabular_term(Term)
  ).
rdf_tabular(_, Content):-
  reply_html_page(
    app_style,
    title('Overview of resources.'),
    \dcg_call(Content)
  ).



% DESCRIBE AN RDF CLASS %

overview_class(Class1) -->
  {
    rdf_global_id(Class1, Class2),
    setoff(
      [Instance],
      rdfs_individual_of(Instance, Class2),
      Instances1
    ),
    list_truncate(Instances1, 50, Instances2)
  },
  rdf_html_table(
    overview_class_caption(Class2),
    [['Instance']|Instances2]
  ).

overview_class_caption(Class) -->
  `Instances of `,
  rdf_term_name(Class).


% DESCRIBE AN RDF INSTANCE %

overview_instance(Instance1, G) -->
  {
    rdf_global_id(Instance1, Instance2),
    setoff(
      [P,O,G],
      rdf(Instance2, P, O, G),
      L
    )
  },
  rdf_html_table(overview_instance_caption(Instance2), L).

overview_instance_caption(Instance) -->
  `Overview of instance `,
  rdf_term_name(Instance),
  `.`.

overview_instances(L1, G) -->
  {
    % Order all resources based on the number of triples describing them.
    setoff(
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
  overview_instances1(L2, G).

overview_instances1([], _) --> [].
overview_instances1([H|T], G) -->
  overview_instance(H, G),
  overview_instances1(T, G).



% DESCRIBE AN RDF TERM %

rdf_tabular_term(Term) -->
  {
    dcg_phrase(rdf_term_name(S1), Term),
    rdf_global_id(S1, S2)
  },
  rdf_tabular_term1(S2).

% Datatype (in typed literal).
% Show all its values.
rdf_tabular_term1(D) -->
  {
    rdf_datatype(_, D), !,
    setoff([Value], rdf_datatype(_, _, D, Value, _), Values)
  },
  rdf_html_table(
    (`Ordered value list for datatype `, atom(D), `.`),
    [['Value']|Values]
  ).
% Predicate term.
% Show:
%   1. all domain classes,
%   2. all range classes,
%   3. all values for literal ranges.
rdf_tabular_term1(P) -->
  {
    rdf(_, P, _), !,
    setoff([C], (rdf(S, P, _), rdfs_individual_of(S, C)), Cs1),
    setoff([C], (rdf(_, P, O), rdfs_individual_of(O, C)), Cs2)
  },
  rdf_html_table(
    (`'Domain of property `, rdf_term_name(P), `.`),
    [['Class']|Cs1]
  ),
  rdf_html_table(
    (`Range of property `, rdf_term_name(P), `.`),
    [['Class']|Cs2]
  ),

  % For literal ranges we also display the values that occur.
  {
    setoff([Value], value_for_p(P, Value), Values1),
    list_truncate(Values1, 100, Values2)
  },
  rdf_html_table(
    (`Values that occur for property `, rdf_term_name(P), `.`),
    [['Value']|Values2]
  ).
% Subject term.
% Display all predicate-object pairs (per graph).
rdf_tabular_term1(S) -->
  overview_instance(S, _).

value_for_p(P, Value):-
  rdf(_, P, literal(type(_,Value))).
value_for_p(P, Value):-
  rdf(_, P, literal(lang(_,Value))).
value_for_p(P, Value):-
  rdf(_, P, literal(Value)),
  \+ compound(Value).

