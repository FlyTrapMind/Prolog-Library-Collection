:- module(
  service_db,
  [
    register_service/4, % +Service:atom
                        % +User:atom
                        % +Password:atom
                        % +Api:atom
    service/4 % ?Service:atom
              % ?User:atom
              % ?Password:atom
              % ?Api:atom
  ]
).

/** <module> Service DB

Persistent store for user+password registrations for services.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(error)).
:- use_module(library(persistency)). % Declarations.

:- use_module(generics(persistent_db_ext)).

%! service(?Service:atom, ?User:atom, ?Password:atom) is nondet.

:- persistent(service(service:atom,user:atom,password:atom,api:atom)).

:- initialization(service_db_init).





%! register_service(+Service:atom, +User:atom, +Password:atom) is det.
% Registers a user to the persistent user database.
%
% Succeeds without change in case the exact same registration already exists.

register_service(Service, User, Password, Api):-
  service(Service, User, Password, Api), !.
register_service(Service, User, Password, Api):-
  assert_service(Service, User, Password, Api).





% INITIALIZATION %

%! service_db_file(-File:atom) is det.

service_db_file(File):-
  absolute_file_name(
    data(service),
    File,
    [access(write),file_type(database)]
  ).


%! service_db_init is det.

service_db_init:-
  service_db_file(File),
  persistent_db_init(File, service_db_update).


%! service_db_update(+Age:nonneg) is det.

service_db_update(_).

