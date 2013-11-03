:- module(app_server, [start_app_server/0]).

/** <module> App server

@author Wouter Beek
@version 2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).
:- use_module(library(thread_pool)).
:- use_module(server(app_home)).
:- use_module(server(login_api)).
:- use_module(server(server_ext)).
:- use_module(server(statistics_api)).

%! app_server_port(?Port:positive_integer) is semidet.

:- dynamic(app_server_port/1).

:- db_add_novel(user:prolog_file_type(db, database)).

% Define the default application server port.
:- setting(
  default_app_server_port,
  positive_integer,
  5000,
  'The default port at which the application server is started.'
).



% Start the application server using the default port (in settings).
start_app_server:-
  (
    db_read(dev_server:dev_server_port(Port)), !
  ;
    setting(default_app_server_port, Port)
  ),
  start_app_server(Port).

%! start_app_server(?Port) is det.
% Start the application server on the given port.

start_app_server(Port):-
  (
    absolute_file_name(
      project(settings),
      File,
      [access(read),file_errors(fail),file_type(database)]
    )
  ->
    load_settings(File)
  ;
    true
  ),
  thread_pool_create(cheapthreads, 90, []),
  start_server(Port, app_server_dispatch),
  db_replace_novel(app_server_port(Port), [e]).

app_server_dispatch(Request):-
  http_dispatch(Request).

