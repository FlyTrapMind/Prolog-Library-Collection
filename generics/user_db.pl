:- module(
  user_db,
  [
    register_user/3, % +Service:atom
                     % +User:atom
                     % +Password:atom
    user/3 % ?Service:atom
           % ?User:atom
           % ?Password:atom
  ]
).

/** <module> User DB

Persistent store for user+password registrations.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(error)).
:- use_module(library(persistency)). % Declarations.

:- use_module(generics(persistent_db_ext)).

%! user(?Service:atom, ?User:atom, ?Password:atom) is nondet.

:- persistent(user(service:atom,user:atom,password:atom)).

:- initialization(user_db_init).



%! register_user(+Service:atom, +User:atom, +Password:atom) is det.
% Registers a user to the persistent user database.
%
% Succeeds without change in case the exact same registration already exists.

register_user(Service, User, Password):-
  user(Service, User, Password), !.
register_user(Service, User, Password):-
  assert_user(Service, User, Password).



% Initialization.

%! user_db_file(-File:atom) is det.

user_db_file(File):-
  absolute_file_name(data(user), File, [access(write),file_type(database)]).


%! user_db_init is det.

user_db_init:-gtrace,
  user_db_file(File),
  persistent_db_init(File, user_db_update).


%! user_db_update(+Age:nonneg) is det.

user_db_update(_).

