:- module(
  web_message,
  [
    log_web/1, % -Markup:list
    log_web/2, % +Category:atom
               % -Markup:list
    web_message/1 % +Term
  ]
).

/** <module> Web message

Acts on messages printed by print_message/2.

@author Wouter Beek
@version 2013/02, 2013/04-2013/05, 2013/08-2013/09, 2013/11
*/

:- use_module(generics(logging)).
:- use_module(html(html_table)).
:- use_module(library(csv)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(settings)).
:- use_module(math(math_ext)).
:- use_module(os(ansi_ext)).
:- use_module(server(web_console)).
:- use_module(server(web_error)).
:- use_module(server(web_modules)).

:- dynamic(current_log_row/1).

:- http_handler(root(msg), web_message, [priority(1)]).

:- initialization(web_module_add('Messages', web_message, msg)).

:- setting(
  max_log_length,
  nonneg,
  500,
  'The maximum number of log items to show.'
).



log_web(Markup):-
  log_web(_Category, Markup).

log_web(_Category, Markup):-
  \+ current_log_file(_File), !,
  Markup = [element(h1,[],['Logging is currently switched off.'])].
log_web(Category, [HTML_Table]):-
  current_log_file(File),
  (
    var(Category)
  ->
    MaxNumberOfRows = inf
  ;
    setting(max_log_length, MaxNumberOfRows)
  ),
  findall(
    [DateTime,Category,Message],
    (
      csv_read_file_row(
        File,
        row(DateTime,Category,Message),
        [arity(3),functor(row),line(RowNumber)]
      ),
      between(1, MaxNumberOfRows, RowNumber)
    ),
    TRs
  ),
  html_table(
    [caption('Log messages'),header(true)],
    [['DateTime','Category','Message']|TRs],
    HTML_Table
  ).

prolog:debug_print_hook(_Type, 'EXCEPTION', [Exception]):-
  web_error(Exception, Markup),
  push(status_pane, html, app_style, Markup), !.
%prolog:debug_print_hook(_Type, 'EXCEPTION', [Exception]):- !,
%  format(user, '~w', [Exception]). %DEB
prolog:debug_print_hook(Type, Format, Args):-
  format(atom(Msg), Format, Args),

  % Write to the status pane in the Web front-end.
  push(
    status_pane,
    html,
    app_style,
    [element(p,[],['[',Type,']',' ',Msg])]
  ),

  % Write to the terminal.
  %ansi_format(user_output, [bold,fg(green)], '[~w] ', [Type]),
  %ansi_formatnl(user_output, [fg(green)], '~w', [Msg]),

  % Write to the log stream/file.
  append_to_log(Type, Format, Args).

web_message(open_uri(_URI)):- !,
  format(user, 'YES!', []),
  http_absolute_location(root(.), URI, []),
  http_open(
    URI,
    _Stream,
    [
      request_header(msg=test),
      request_header('Content-Type'='application/x-www-form-urlencoded')
    ]
  ).
web_message(_Request):-
  reply_html_page(app_style, title('Messages'), \web_message_body).

web_message_body -->
  {log_web(Markup)},
  html(Markup).

