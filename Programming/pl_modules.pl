:- module(pl_modules, []).

/** <module> Prolog modules

Web interface to Prolog modules.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(uri_query)).
:- use_module(html(html_list)).
:- use_module(html(html_table)).
:- use_module(html(pl_term_html)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(prolog_xref)).
:- use_module(programming(pl_predicates)).
:- use_module(server(web_modules)).

http:location(pl, root(pl), []).
:- http_handler(pl(modules), pl_modules, []).

:- web_module_add('plMod', pl_modules).



pl_modules(Request):-
  request_query_read(Request, module, Module),
  reply_html_page(
    app_style,
    title(['Prolog modules - Module ',Module]),
    \pl_module(Module)
  ).
pl_modules(_Request):-
  reply_html_page(app_style, title('Prolog modules'), \pl_modules).


pl_modules -->
  {
    % Set of currently loaded modules.
    aggregate_all(
      set(Module),
      current_module(Module),
      Modules
    ),
    % Properties of modules.
    findall(
      [Module|Properties],
      (
        member(Module, Modules),
        findall(Property, module_property(Module, Property), Properties)
      ),
      Rows
    )
  },
  html_table(
    [header_row(true)],
    html('Overview of modules.'),
    pl_module_term_html,
    [['Module','Class','File','Line count','Exported predicates',
      'Exported operators']|Rows]
  ).


pl_module(Module) -->
  {module_property(Module, exports(Predicates))},
  pl_predicates(Predicates).


pl_module_term_html(Module) -->
  {
    current_module(Module), !,
    http_absolute_location(pl(modules), Location1, []),
    uri_query_add(Location1, module, Module, Location2)
  },
  html(span(class=module, a(href=Location2, Module))).
pl_module_term_html(class(Class)) --> !,
  html(span(class=class, Class)).
pl_module_term_html(file(File)) --> !,
  html(span(class=file, File)).
pl_module_term_html(line_count(LineCount)) --> !,
  html(span(class=line_count, LineCount)).
pl_module_term_html(exports(Predicates)) --> !,
  html_list([ordered(true)], html_predicate, Predicates).
pl_module_term_html(exported_operators(Operators)) --> !,
  html_list([ordered(true)], html_operator, Operators).
pl_module_term_html(X) -->
  pl_term_html(X).

