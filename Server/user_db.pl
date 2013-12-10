:- module(
  user_db,
  [
    user/1, % ?User:atom
    user/2, % ?Name:atom
            % ?Properties:list
    user_add/2, % +Name:atom
                % +Properties:list
    user_property/2, % ?Name:atom
                     % ?Property:compound
    user_remove/1 % +Name:atom
  ]
).

/** <module> User administration

Core user administration.

The user administration is based on the following:
  * A persistent fact user/2
  * Session management

@author Jan Wielemaker
@author Torbjörn Lager
@author Wouter Beek
@version 2013/10-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(persistency)).
:- use_module(os(os_ext)).

:- db_add_novel(user:prolog_file_type(db, database)).

%! user(?User:atom, ?Properties:list:compound) is nondet.

:- persistent(user(user:atom,properties:list(compound))).

:- initialization(init_users).



%! current_user(?User:atom, ?Properties:list:compound) is nondet.

current_user(User, Properties):-
  with_mutex(user_db, user(User, Properties)).

init_users:-
  (
    users_file(File), !
  ;
    absolute_file_name(
      project(users),
      File,
      [access(write),file_type(database)]
    ),
    touch(File)
  ),
  db_attach(File, [sync(close)]).

%! user(?User:atom) is nondet.
% Registered users.

user(User):-
  current_user(User, _Properties).

%! user_add(+Name:atom, +Properties:list) is det.
% Adds a new user with the given properties.

user_add(Name, Options):-
  with_mutex(user_db, assert_user(Name, Options)).

%! user_property(?User:atom, ?Property:compound) is nondet.
%! user_property(+User:atom, +Property:compound) is semidet.
% Users and their properties.
%
% In addition to properties explicitly stored with users, we define:
%   * =|connection(LoginTime,Idle)|=
%   * =|session(SessionID)|=

user_property(User, Property):-
  nonvar_det(user_property_(User, Property)).

user_property_(User, Property):-
  current_user(User, Properties),
  member(Property, Properties).
user_property_(User, Property):-
  login_db:user_property(User, Property).

%! user_remove(+Name:atom) is det.
% Delete named user from user-database.

user_remove(Name):-
  with_mutex(
    user_db,
    (
      user(Name, _Properties1), !,
      retractall_user(Name, _Properties2)
    )
  ).
user_remove(Name):-
  existence_error(user, Name).

users_file(File):-
  absolute_file_name(
    project(users),
    File,
    [access(read),file_errors(fail),file_type(database)]
  ).
