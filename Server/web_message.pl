:- module(
  web_message,
  [
    log_web/1, % -Markup:list
    web_message/1 % +Term
  ]
).

/** <module> Web message

Acts on messages printed by print_message/2.

@author Wouter Beek
@version 2013/02, 2013/04-2013/05
*/

:- use_module(html(html)).
:- use_module(library(http/http_open)).
:- use_module(server(dev_server)).
:- use_module(server(error_web)).

:- dynamic(current_log_row/1).



log_web(Markup):-
  \+ current_log_file(_File),
  !,
  Markup = [element(h1, [], ['Logging is currently switched off.'])].
log_web([HTML_Table]):-
  current_log_file(File),
  csv_read_file(File, Rows, [arity(4), functor(row)]),
  findall(
    [Situation, DateTime, Category, Message],
    member(row(Situation, DateTime, Category, Message), Rows),
    TRs
  ),
  list_to_table(
    [header(true)],
    [['Situation', 'DateTime', 'Category', 'Message'] | TRs],
    HTML_Table
  ).

prolog:debug_print_hook(_Type, 'EXCEPTION', [Exception]):-
  error_web(Exception, Markup),
  push(status_pane, html, dev_server, Markup),
  !.
prolog:debug_print_hook(_Type, 'EXCEPTION', [Exception]):-
  !,
  gtrace, %DEB
  format(user, '~w', [Exception]). %DEB
prolog:debug_print_hook(Type, Format, Arguments):-
  format(atom(Message), Format, Arguments),
  push(
    status_pane,
    html,
    dev_server,
    [element(p, [], ['[', Type, ']', ' ', Message])]
  ),
  append_to_log(Type, Format, Arguments).

web_message(open_uri(_URI)):-
  format(user, 'YES!', []),
  dev_server_uri(URI),
  http_open(
    URI,
    _Stream,
    [
      request_header(msg=test),
      request_header('Content-Type'='application/x-www-form-urlencoded')
    ]
  ).

