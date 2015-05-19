:- module(
  app_server,
  [
    start_app_server/0,
    start_app_server/1, % +Options:list(nvpair)
    start_app_server_clas/0,
    start_app_server_clas/1 % +Options:list(nvpair)
  ]
).

/** <module> Application server

Using this module automatically starts the server.

If you login, the system will redirect  you to its public address.
I.e., if you connected to `http://localhost:5000/` it will redirect you to
`http://my.domain.org:500/`.
This can be undesirable on e.g., a notebook that is not always connected to
the internet and/or may change address and/or may be behind a firewall.
You can disable redirection using the settings below.
These settings may also be necessary  if the server is behind a proxy.

```prolog
:- set_setting_default(http:public_host, localhost).
:- set_setting_default(http:public_port, setting(http:port)).
```

---

@author Wouter Beek
@author Jan Wielemaker (copied from ClioPatria)
@version 2013/11-2013/12, 2014/05, 2015/01
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(option)).
:- use_module(library(optparse)).
:- use_module(library(settings)).

:- use_module(plc(server/server_ext)).

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(db, database).

% Define the default application server port in the same way ClioPatria does.
:- if(\+ current_setting(http:port)).
  :- setting(
    http:port,
    nonneg,
    env('PORT',3020),
    'The default port the HTTP server listens to.'
  ).
:- endif.





%! start_app_server is det.

start_app_server:-
  start_app_server([]).

%! start_app_server(+Options:list(nvpair)) is det.
% Starts an application server.
%
% dotCloud
% ========
%
% dotCloud defines the `PORT_WWW` environment variable.
%
% @see http://docs.dotcloud.com/services/custom/

start_app_server(Options):-
  % Set port option.
  (   option(port(Port), Options)
  ->  true
  ;   getenv('PORT_WWW', PortAtom)
  ->  atom_number(PortAtom, Port)
  ;   setting(http:port, Port)
  ),
  
  % Load settings from file.
  (   absolute_file_name(
        project(settings),
        File,
        [access(read),file_errors(fail),file_type(database)]
      )
  ->  load_settings(File)
  ;   true
  ),
  
  start_server(Port, app_server_dispatch, Options).

%! start_app_server_clas is det.
% @see start_app_server_clas/1

start_app_server_clas:-
  start_app_server_clas([]).

%! start_app_server_clas(+Options:list(nvpair)) is det.
% Variant of start_app_server/1 that processes command-line arguments.

start_app_server_clas(Options1):-
  % Command-line options.
  opt_arguments(
    [
      [default(false),longflags([debug]),opt(debug),type(boolean)],
      [default(3020),longflags([port]),opt(port),type(integer)]
    ],
    Options2,
    _
  ),
  merge_options(Options1, Options2, Options3),
  start_app_server(Options3).

%! app_server_dispatch(+Request:list) is det.
% A wrapper predicate whose sole purpose is to be a handle for trace statements.

app_server_dispatch(Request):-
  http_dispatch(Request).

