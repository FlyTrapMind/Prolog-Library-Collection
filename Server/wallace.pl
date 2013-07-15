:- module(
  wallace,
  [
    push/2, % +Type:oneof([console_output,status_pane])
            % +DOM:list
    push/4, % +Type:oneof([console_output,status_pane])
            % +DTD_Name:atom
            % +StyleName:atom
            % +DOM:list
    start_wallace/0,
    wallace_uri/1 % -URI:uri
  ]
).

/** <module> Wallace webserver

Using this module automatically starts the server.

Logging is required once Wallace is started, because module
=|web_message|= causes debug messages to be appended to the
current logging stream.

@author Wouter Beek
@see http://semanticweb.cs.vu.nl/prasem/
@version 2012/05, 2012/09-2012/12, 2013/02-2013/06
*/

:- use_module(generics(db_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/thread_httpd)).
:- use_module(os(datetime_ext)).
:- use_module(server(web_console)).
:- use_module(server(web_message)). % Module registration.
:- use_module(sparql(sparql_web)).
:- use_module(http(http)).

% This is used to push content for the console output and status pane.
:- dynamic(content_queue(_Type, _DTD_Name, _StyleName, _DOM)).
:- dynamic(history(_Type, _DataTime, _DTD_Name, _StyleName, _DOM)).

% By registering these modules, their Web predicates become accessible
% from the Web console.
:- register_module(web_message).

% Serve CSS files.
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).

% Serve JavaScript files.
:- db_add_novel(http:location(js, root(js), [])).
:- db_add_novel(user:file_search_path(js, server(js))).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

% HTTP handlers for the Wallace server.
:- http_handler(root(.), wallace, [prefix, priority(-10)]).
:- http_handler(root(console_output), console_output, []).
:- http_handler(root(history), history, []).
:- http_handler(root(status_pane), status_pane, []).

% HTML resources and their dependencies.
:- html_resource(css('console_output.css'), [requires(css('wallace.css'))]).
:- html_resource(css('status_pane.css'), [requires(css('wallace.css'))]).
:- html_resource(css('wallace.css'), []).
:- html_resource(js('console_output.js'), [requires(js('wallace.js'))]).
:- html_resource(js('status_pane.js'), [requires(js('wallace.js'))]).

:- multifile(user:body//2).
:- multifile(user:head//2).



% START SERVER %

default_port(5000).

start_wallace:-
  default_port(Port),
  http_server_property(Port, start_time(_Time)), !.
start_wallace:-
  default_port(Port),
  % Make sure Wallace is shut down whenever Prolog shuts down.
  assert(user:at_halt(http_stop_server(Port, []))),
  http_server(http_dispatch, [port(Port)]).



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

documentation(Request):-
  doc_browser,
  % Stay on the root page.
  wallace(Request).

history(_Request):-
  % Fixate the DTD and Style used.
  history(status_pane, _DateTime, DTD_Name, StyleName, _DOM), !,
  findall(
    [element(h1, [], [DateTime]) | DOM],
    history(status_pane, DateTime, DTD_Name, StyleName, DOM),
    DOMs
  ),
  reverse(DOMs, RDOMs),
  append(RDOMs, DOM),
  serve_xml(
    DTD_Name,
    StyleName,
    [element(html, [], [element(body, [], DOM)])]
  ).
history(_Request):-
  reply_html_page([], [p('The history is empty.')]).

push(Type, DOM):-
  push(Type, html, wallace, DOM).

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

user:body(wallace, _Body) -->
  html(
    body(
      onload('loadConsoleOutputFunctions(); loadStatusPaneFunctions();'),
      [
        \html_requires(css('wallace.css')),
        \console_input,
        \console_output,
        \status_pane
      ]
    )
  ).

user:head(wallace, Head) -->
  {project_name(Project)},
  html(head(title(Project), Head)).

wallace(Request):-
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
    web_console(Command, DTD_Name, StyleName, DOM),
    push(console_output, DTD_Name, StyleName, DOM)
  ;
    Query \== no_input
  ->
    sparql_output_web(Query, DOM),
    push(console_output, html, wallace, DOM)
  ;
    true
  ),
  reply_html_page(wallace, [], []).

wallace_uri(URI):-
  default_port(Port),
  http_open:parts_uri(
    [host(localhost), path('/'), port(Port), scheme(http)],
    %[host('semanticweb.cs.vu.nl'), path('/prasem/'), scheme(http)],
    URI
  ).

