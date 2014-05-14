:- module(
  server_ext,
  [
    start_server/1, % +Port:between(1000,9999)
    start_server/2 % +Port:between(1000,9999)
                   % :ServerGoal
  ]
).

/** <module> Server extensions

Extensions for SWI-Prolog servers.

Dispatching non-login methods goes through this module,
which checks whether the user has authorization.

# HTTP locations

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

# HTTP handlers

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

# Server rebase

In order to rebase an entire Web application:
~~~{.pl}
set_setting(http:prefix, Prefix).
~~~
@see http://www.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/http.html%27%29

--

@author Wouter Beek
@version 2013/10-2014/01
*/

:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_session)). % Session support.
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).

:- use_module(generics(meta_ext)).

:- meta_predicate(start_server(+,:)).
:- meta_predicate(start_server_on_next_port(+,+,:,-)).



%! server_port(+Port:between(1000,9999)) is semidet.
%! server_port(-Port:between(1000,9999)) is multi.
% Valid values for server port.

server_port(Port):-
  between(1000, 9999, Port).


%! start_server(+Port:between(1000,9999)) is det.

start_server(Port):-
  start_server(Port, _).

%! start_server(+Port:between(1000,9999), :ServerGoal) is det.

% A server is already running at the given port.
start_server(Port, _):-
  http_server_property(Port, start_time(StartTime)), !,
  debug(
    server_ext,
    'The server at port ~w is used as the application server \c
     (start time ~w).',
    [Port,StartTime]
  ).
% No server is running yet, so start a server.
start_server(Port, ServerGoal):-
  % Estimate the number of workes based on the number of CPU cores.
  current_prolog_flag(cpu_count, NumberOfCores),
  NumberOfWorkers is NumberOfCores * 2,

  % Allow a custom goal for server dispatching.
  default(http_dispatch, ServerGoal),

  start_server_on_next_port(Port, NumberOfWorkers, ServerGoal, PortUsed),

  % Make sure the server is shut down whenever SWI-Prolog shuts down.
  at_halt(http_stop_server(PortUsed, [])),

  % INFO
  print_message(informational, server_ext(started(PortUsed))).


%! start_server_on_next_port(
%!   +Port:between(1000,9999),
%!   +NumberOfWorkers:positive_integer,
%!   :ServerGoal,
%!   -PortUsed:between(1000,9999)
%! ) is det.
% Keeps incresing the given port number until a free port number is found;
% then starts a new server at that port.

% Increment port numbers until a free one is found.
start_server_on_next_port(Port, NumberOfWorkers, ServerGoal, PortUsed):-
  server_port(Port), !,
  catch(
    (
      http_server(ServerGoal, [port(Port),workers(NumberOfWorkers)]),
      PortUsed = Port
    ),
    error(socket_error(_), _),
    (
      NextPort is Port + 1,
      start_server_on_next_port(
        NextPort,
        NumberOfWorkers,
        ServerGoal,
        PortUsed
      )
    )
  ).
% At the end of the port numeber list we start over
% by trying out the lowest port number.
start_server_on_next_port(_, NumberOfWorkers, ServerGoal, PortUsed):-
  start_server_on_next_port(1000, NumberOfWorkers, ServerGoal, PortUsed).



% Messages

:- multifile(prolog:message//1).

prolog:message(server_ext(started(Port))) -->
  {setting(http:prefix, Prefix)},
  ['You can access the server at http://localhost:~w/~w'-[Port,Prefix]].

