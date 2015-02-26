:- module(
  server_ext,
  [
    start_server/2, % +Port:between(1000,9999)
                    % +Options:list(nvpair)
    start_server/3 % +Port:between(1000,9999)
                   % :ServerGoal
                   % +Options:list(nvpair)
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
```prolog
set_setting(http:prefix, Prefix).
```
@see http://www.swi-prolog.org/pldoc/doc_for?object=section%28%27packages/http.html%27%29

--

@author Wouter Beek
@version 2013/10-2014/01, 2014/05
*/

:- use_module(library(error)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_session)). % Session support.
:- use_module(library(http/thread_httpd)).
:- use_module(library(settings)).

:- use_module(plc(generics/meta_ext)).

:- meta_predicate(start_server(+,:,+)).
:- meta_predicate(start_new_server(+,:,+)).





is_server_port(Port):-
  catch(
    server_port(Port),
    _,
    fail
  ).


%! server_port(+Port:between(1000,9999)) is semidet.
%! server_port(-Port:between(1000,9999)) is multi.
% Valid values for server port.
%
% @throws instantiation_error If port is not instantiated.
% @throws type_error If port is instantiated to a non-port number.

server_port(Port):-
  must_be(between(1000, 9999), Port).


%! start_server(+Port:between(1000,9999), +Options:list(nvpair)) is det.
%
% @throws instantiation_error If port is not instantiated.
% @throws type_error If port is instantiated to a non-port number.

start_server(Port, Options):-
  start_server(Port, _, Options).

%! start_server(
%!   +Port:between(1000,9999),
%!   :ServerGoal,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * use_existing(+UseExisting:boolean)
%     If there is already a server running at the given port,
%     we use that server.
%   * Other options are passed on to http_server/2.
%
% @throws instantiation_error If port is not instantiated.
% @throws type_error If port is instantiated to a non-port number.

% Loop the sequence of server port numbers.
start_server(Port, ServerGoal, Options):-
  \+ is_server_port(Port), !,
  start_server(1000, ServerGoal, Options).
% A server is already running at the given port.
start_server(Port, _, Options):-
  http_server_property(Port, start_time(_)),
  option(use_existing(true), Options), !,
  print_message(informational, server_using_existing(Port)).
% The current port is already in use, so try another one.
start_server(Port1, ServerGoal, Options):-
  http_server_property(Port1, start_time(_)), !,
  print_message(informational, server_running(Port1)),
  Port2 is Port1 + 1,
  start_server(Port2, ServerGoal, Options).
% No server is running yet, so start a new server.
start_server(Port, ServerGoal, Options):-
  start_new_server(Port, ServerGoal, Options).

start_new_server(Port, ServerGoal, Options1):-
  % Estimate the number of workes based on the number of CPU cores.
  current_prolog_flag(cpu_count, NumberOfCores),
  NumberOfWorkers is NumberOfCores * 2,

  % Allow a custom goal for server dispatching.
  default(http_dispatch, ServerGoal),

  merge_options(Options1, [port(Port),workers(NumberOfWorkers)], Options2),
  http_server(ServerGoal, Options2),

  % Make sure the server is shut down whenever SWI-Prolog shuts down.
  at_halt(stop_server(Port)),

  % Info.
  http_log('Starting weblog demo on port ~w~n', [Port]),
  print_message(informational, server_started(Port)).

stop_server:-
  setting(http:port, Port),
  stop_server(Port).

stop_server(Port):-
  must_be(between(1000,9999), Port),
  http_server_property(Port, _), !,
  http_stop_server(Port, []),
  print_message(informational, server_stopped(Port)).
stop_server(_).



% Messages

:- multifile(prolog:message//1).

prolog:message(server_running(Port,StartTime)) -->
  ['The server at port ~d is already in use (start time: '-[Port]],
  time_started(StartTime),
  [').'].
prolog:message(server_started(Port)) -->
  ['The server started on port ~w.'-[Port]].
prolog:message(server_stopped(Port)) -->
  ['The server at port ~d has stopped.'-[Port]].
prolog:message(server_using_existing(Port)) -->
  ['The server at port ~d is reused (start time: '-[Port]],
  time_started(Port),
  [').'].

time_started(Port) -->
  {
    http_server_property(Port, start_time(StartTime)), !,
    http_timestamp(StartTime, Text)
  },
  [Text].

