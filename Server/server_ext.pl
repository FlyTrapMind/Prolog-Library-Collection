:- module(
  server_ext,
  [
    server_rebase/1, % +Prefix:atom
    start_server/1, % +Port:between(1000,9999)
    start_server/2 % +Port:between(1000,9999)
                   % :ServerGoal
  ]
).

/** <module> Server extensions

Extensions for SWI-Prolog servers.

## Default HTTP handlers

SWI-Prolog defines the following HTTP locations:

| *Abbreviation*   | *Path*               |
| =css=            | =|root(css)|=        |
| =icons=          | =|root(icons)|=      |
| =js=             | =|root(js)|=         |
| =pldoc=          | =|root(.)|=          |
| =pldoc_man=      | =|pldoc(refman)|=    |
| =pldoc_pkg=      | =|pldoc(package)|=   |
| =pldoc_resource= | =|/debug/help/res/|= |
| =root=           | =|/|=                |

SWI-Prolog defines the following HTTP handlers:

| *Spec*                  | *Handler*                                             |
| =|css(.)|=              | =|http_server_files:serve_files_in_directory(css)|=   |
| =|icons(.)|=            | =|http_server_files:serve_files_in_directory(icons)|= |
| =|js(.)|=               | =|http_server_files:serve_files_in_directory(js)|=    |
| =|pldoc(.)|=            | =|pldoc_root|=                                        |
| =|pldoc(doc)|=          | |
| =|pldoc(doc_for)|=      | |
| =|pldoc(edit)|=         | |
| =|pldoc(file)|=         | |
| =|pldoc('index.html')|= | |
| =|pldoc(man)|=          | |
| =|pldoc(pack)|=         | |
| =|pldoc('pack/')|=      | |
| =|pldoc(place)|=        | |
| =|pldoc('res/')|=       | |
| =|pldoc(search)|=       | |
| =|pldoc_man(.)|=        | |
| =|pldoc_pkg(.)|=        | |

@author Wouter Beek
@version 2013/10-2013/11
*/

:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).

:- setting(
  http:prefix,
  atom,
  '/',
  'The prefix for all HTTP paths in this projet.'
).

:- meta_predicate(start_server(+,:)).
:- meta_predicate(start_server_on_next_port(+,+,:)).

:- multifile(prolog:message//1).

:- debug(server_ext).



% Rebase the entire Web application.
server_rebase(Prefix):-
  set_setting(http:prefix, Prefix).

%! start_server(+Port:between(1000,9999)) is det.
% @see Wrapper around start_server/2.

start_server(Port):-
  start_server(Port, _ServerGoal).

%! start_server(+Port:between(1000,9999), :ServerGoal) is det.

% A server is already running.
start_server(Port, _ServerGoal):-
  http_server_property(Port, start_time(StartTime)), !,
  debug(
    server_ext,
    'The server at port ~w is used as the application server \c
     (start time ~w).',
    [Port,StartTime]
  ).
% No server is running yet, so start a server.
start_server(Port, ServerGoal0):-
  % Make sure the server is shut down whenever SWI-Prolog shuts down.
  assert(user:at_halt(http_stop_server(Port, []))),

  % Estimate the number of workes based on the number of CPU cores.
  current_prolog_flag(cpu_count, NumberOfCores),
  NumberOfWorkers is NumberOfCores * 2,

  % Allow a custom goal for server dispatching.
  default(ServerGoal0, http_dispatch, ServerGoal),

  start_server_on_next_port(Port, NumberOfWorkers, ServerGoal),

  % INFO
  print_message(informational, server_ext(started(Port))).

start_server_on_next_port(Port, NumberOfWorkers, ServerGoal):-
  catch(
    http_server(ServerGoal, [port(Port),workers(NumberOfWorkers)]),
    error(socket_error(_Msg), _),
    (
      NextPort is Port + 1,
      start_server_on_next_port(NextPort, NumberOfWorkers, ServerGoal)
    )
  ).

prolog:message(server_ext(started(Port))) -->
  {setting(http:prefix, Prefix)},
  ['You can access the server at http://localhost:~w/~w'-[Port,Prefix]].

