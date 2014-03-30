:- module(
  pl_predicates,
  [
    pl_predicates//1 % +Predicates:list
  ]
).

/** <module> Prolog predicates

Web interface to Prolog predicates.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(uri_query)).
:- use_module(html(html_list)).
:- use_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(prolog_xref)).
:- use_module(pl_web(html_pl_term)).
:- use_module(server(web_modules)).

http:location(pl, root(pl), []).
:- http_handler(pl(predicates), pl_predicates, []).

:- web_module_add('plPred', pl_predicates).



% A single predicate term.
pl_predicates(Request):-
  request_query_read(Request, predicate, Predicate), !,
  reply_html_page(
    app_style,
    title(['Prolog predicates - Predicate ',Predicate]),
    \pl_predicate(Predicate)
  ).
pl_predicates(_Request):-
  reply_html_page(app_style, title('Prolog predicates'), html('TODO')).


pl_predicate(Predicate) -->
  {
    % Enumerate all predicate properties.
    findall(
      [Name,Value],
      (
        predicate_property(Predicate, Property),
        Property =.. [Name|T],
        (
          T == []
        ->
          Value = true
        ;
          T = [Value]
        )
      ),
      Rows
    )
  },
  html_table(
    [header_row(true)],
    html(['Overview of predicate ',Predicate,'.']),
    html_pl_term,
    Rows
  ).


pl_predicates([H|T]) -->
  pl_predicate(H),
  pl_predicates(T).
pl_predicates([]) --> [].

