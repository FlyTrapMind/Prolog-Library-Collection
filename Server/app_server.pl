:- module(app_server, [start_app_server/0]).

/** <module> App server

Using this module automatically starts the server.

Web home page of the development server.
Displays a form for entering Web predicates and displays the results
of their execution.
Also includes a status bar with updates/messages.

@author Wouter Beek
@see http://semanticweb.cs.vu.nl/prasem/
@version 2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).
:- use_module(library(thread_pool)).
:- use_module(server(server_ext)).

:- db_add_novel(user:prolog_file_type(db, database)).

% Define the default application server port.
:- setting(
  default_app_server_port,
  positive_integer,
  5000,
  'The default port at which the application server is started.'
).

:- initialization(start_app_server).


% Start the application server when running on dotcloud.
start_app_server:-
  getenv('PORT_WWW', PortAtom), !,
  atom_number(PortAtom, Port),
  start_server(Port, http_dispatch).
% Start the application server using the default port (in settings).
start_app_server:-
  setting(default_app_server_port, Port),
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
  %thread_pool_create(cheapthreads, 90, []),
  start_server(Port, app_server_dispatch).

app_server_dispatch(Request):-
  http_dispatch(Request).

