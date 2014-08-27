:- module(
  service_db,
  [
    register_service/3, % +Service:atom
                     % +User:atom
                     % +Password:atom
    service/3 % ?Service:atom
              % ?User:atom
              % ?Password:atom
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

:- persistent(service(service:atom,user:atom,password:atom)).

:- initialization(service_db_init).



%! register_service(+Service:atom, +User:atom, +Password:atom) is det.
% Registers a user to the persistent user database.
%
% Succeeds without change in case the exact same registration already exists.

register_service(Service, User, Password):-
  user(Service, User, Password), !.
register_service(Service, User, Password):-
  assert_user(Service, User, Password).



% Initialization.

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

