:- module(
  dev_server,
  [
    push/1, % +DOM:list
    push/2, % +Type:oneof([console_output,status_pane])
            % +DOM:list
    push/4, % +Type:oneof([console_output,status_pane])
            % +DTD_Name:atom
            % +StyleName:atom
            % +DOM:list
    start_dev_server/0,
    start_dev_server/1 % +Port:nonneg
  ]
).

/** <module> Wallace webserver

Using this module automatically starts the server.

Logging is required once Wallace is started, because module
=|web_message|= causes debug messages to be appended to the
current logging stream.

@author Wouter Beek
@see http://semanticweb.cs.vu.nl/prasem/
@version 2012/05, 2012/09-2012/12, 2013/02-2013/10
*/

:- use_module(generics(db_ext)).
:- use_module(http(http)).
:- use_module(library(debug)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(os(datetime_ext)).
:- use_module(server(server_ext)).
:- use_module(server(web_console)).
:- use_module(server(web_message)). % Module registration.
:- use_module(sparql(sparql_web)).

%! content_queue(
%!   ?Type:oneof([console_output,status_pane]),
%!   ?DTD_Name:atom,
%!   ?StyleName:atom,
%!   ?DOM:list
%! ) is nondet.
% This is used to push content for the Web front-end.

:- dynamic(content_queue/4).

%! history(
%!   ?Type:oneof([console_output,status_pane]),
%!   ?DateTime,
%!   ?DTD_Name:atom,
%!   ?StyleName:atom,
%!   ?DOM:list
%! ) is nondet.

:- dynamic(history/5).

:- db_add_novel(http:location(dev_server, root(dev_server), [])).

% Serve CSS files.
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).

% Serve JavaScript files.
:- db_add_novel(http:location(js, root(js), [])).
:- db_add_novel(user:file_search_path(js, server(js))).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

% HTTP handlers for the Wallace server.
:- http_handler(dev_server(.), dev_server, [prefix, priority(-10)]).
:- http_handler(dev_server(console_output), console_output, []).
:- http_handler(dev_server(history), history, []).
:- http_handler(dev_server(status_pane), status_pane, []).

% HTML resources and their dependencies.
:- html_resource(css('console_output.css'), [requires(css('dev_server.css'))]).
:- html_resource(css('status_pane.css'), [requires(css('dev_server.css'))]).
:- html_resource(css('dev_server.css'), []).
:- html_resource(js('console_output.js'), [requires(js('dev_server.js'))]).
:- html_resource(js('status_pane.js'), [requires(js('dev_server.js'))]).

% plDoc documentation.
:- http_handler(dev_server(doc), documentation, [prefix,priority(0)]).

% By registering these modules, their Web predicates become accessible
% from the Web console.
:- register_module(web_message).

:- multifile(user:body//2).
:- multifile(user:head//2).

:- setting(default_port, nonneg, 5000, 'The default port for the development server.').

:- debug(dev_server).



% START DEV SERVER %

start_dev_server:-
  start_dev_server(_Port).

%! start_dev_server(?Port:nonneg) is det.
% Starts the development server on the given port.

% Notice that its port need not be the default port.
start_dev_server(Port1):-
  % Either use the given port or the default port.
  setting(default_port, DefaultPort),
  default(Port1, DefaultPort, Port2),
  start_server(Port2, dev_server_dispatch).

% This makes it easy to debug the catching of HTTP handlers.
dev_server_dispatch(Request):-
  http_dispatch(Request).


% WEB FRONTEND %

console_output -->
  html([
    div(id(console_output), []),
    \html_requires(css('console_output.css')),
    \html_requires(js('console_output.js'))
  ]).

console_output(_Request):-
  retract(content_queue(console_output, DTD_Name, Style_Name, DOM)), !,
  serve_xml(DTD_Name, Style_Name, DOM).
console_output(Request):-
  serve_nothing(Request).

dev_server(Request):-
  http_parameters(
    Request,
    [
      web_command(Command, [default(no_command)]),
      web_input(Query, [default(no_input)])
    ]
  ),
  (
    Command \== no_command
  ->
    web_console(Command, Markup),
    push(console_output, Markup)
  ;
    Query \== no_input
  ->
    sparql_output_web(Query, DOM),
    push(console_output, html, dev_server, DOM)
  ;
    true
  ),
  reply_html_page(dev_server, [], []).

documentation(Request):-
  doc_browser,
  % Stay on the root page.
  dev_server(Request).

history(_Request):-
  % Fixate the DTD and Style used.
  history(status_pane, _DateTime, DTD_Name, StyleName, _DOM), !,
  findall(
    [element(h1,[],[DateTime])|DOM],
    history(status_pane, DateTime, DTD_Name, StyleName, DOM),
    DOMs
  ),
  reverse(DOMs, RDOMs),
  append(RDOMs, DOM),
  serve_xml(
    DTD_Name,
    StyleName,
    [element(html,[],[element(body,[],DOM)])]
  ).
history(_Request):-
  reply_html_page([],[p('The history is empty.')]).

markup_mold(DTD_Name/StyleName/DOM, DTD_Name, StyleName, DOM):- !.
markup_mold(StyleName/DOM, html, StyleName, DOM):- !.
markup_mold(DOM, html, dev_server, DOM):- !.

%! push(+Markup:list) is det.
% @see Wrapper around push/2 that pushes markup to the console output.

push(Markup):-
  push(console_output, Markup).

%! push(+Type:oneof([console_output,status_pane]), +Markup:list) is det.

push(Type, Markup):-
  markup_mold(Markup, DTD_Name, StyleName, DOM),
  push(Type, DTD_Name, StyleName, DOM).

push(Type, DTD_Name, StyleName, DOM):-
  % Assert the content for AJAX retrieval.
  assertz(content_queue(Type, DTD_Name, StyleName, DOM)),

  % Also store the content in the history.
  current_date_time(DateTime),
  assertz(history(Type, DateTime, DTD_Name, StyleName, DOM)).

status_pane(_Request):-
  retract(content_queue(status_pane, DTD_Name, Style_Name, DOM)), !,
  serve_xml(DTD_Name, Style_Name, DOM).
status_pane(Request):-
  serve_nothing(Request).

status_pane -->
  html([
    \html_requires(css('status_pane.css')),
    \html_requires(js('status_pane.js')),
    div(id(status_pane), [])
  ]).

user:body(dev_server, _Body) -->
  html(
    body(
      onload('loadConsoleOutputFunctions(); loadStatusPaneFunctions();'),
      [
        \html_requires(css('dev_server.css')),
        \console_input,
        \console_output,
        \status_pane
      ]
    )
  ).

user:head(dev_server, _Head) -->
  {project_name(Project)},
  html(head(title(Project))).

