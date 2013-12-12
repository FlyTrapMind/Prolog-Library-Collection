:- module(
  user_db,
  [
    add_user/2, % +UserName:atom
                % +Properties:list
    user/1, % ?UserName:atom
    user/2, % ?UserName:atom
            % ?Properties:list
    user_property/2, % ?UserName:atom
                     % ?Property:compound
    remove_user/1 % +UserName:atom
  ]
).

/** <module> User administration

Core user administration.
Also keeps track of who is logged in.

The user administration is based on the following:
  * Persistent facts logged_in/3 and user/2.
  * Session management.

@author Wouter Beek
@version 2013/10-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(user_input)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_session)).
:- use_module(library(lists)).
:- use_module(library(persistency)). % Declarations
:- use_module(os(os_ext)).
:- use_module(server(login_db)).
:- use_module(server(password_db)).

:- db_add_novel(user:prolog_file_type(db, database)).

%! user(?UserName:atom, ?Properties:list:compound) is nondet.
:- persistent(user(user:atom,properties:list(compound))).

:- initialization(init_user_db).



%! add_user(+UserName:atom, +Properties:list) is det.
% Adds a new user with the given properties.

add_user(UserName, Properties):-
  with_mutex(user_db, assert_user(UserName, Properties)).

%! current_user(?UserName:atom, ?Properties:list:compound) is nondet.

current_user(UserName, Properties):-
  with_mutex(user_db, user(UserName, Properties)).

init_user_db:-
  user_db_file(File), !,
  db_attach(File, []).
init_user_db:-
  absolute_file_name(
    project(users),
    File,
    [access(write),file_type(database)]
  ),
  touch(File),
  db_attach(File, []),
  
  % First time deployment.
  add_user(admin, [roles([admin])]),
  user_input_password('Enter the password for admin.', UnencryptedPassword),
  add_password(admin, UnencryptedPassword).

%! remove_user(+UserName:atom) is det.
% Delete named user from user-database.

remove_user(UserName):-
  with_mutex(
    user_db,
    (
      once(user(UserName, _Properties1)),
      retractall_user(UserName, _Properties2)
    )
  ).
remove_user(UserName):-
  existence_error(user, UserName).

%! user(?UserName:atom) is nondet.
% Registered users.

user(UserName):-
  current_user(UserName, _Properties).

%! user_property(?UserName:atom, ?Property:compound) is nondet.
%! user_property(+UserName:atom, +Property:compound) is semidet.
% Users and their properties.
%
% In addition to properties explicitly stored with users, we define:
%   * =|connection(LoginTime,Idle)|=
%   * =|session(SessionID)|=

% Connection information for a user.
user_property(UserName, connection(LoginTime,Idle)):- !,
  logged_in(Session, UserName, LoginTime),
  http_current_session(Session, idle(Idle)).
% Session identification for a user.
user_property(UserName, session(Session)):- !,
  logged_in(Session, UserName, _LoginTime),
  % A session can have at most one user.
  (nonvar(Session) -> ! ; true).
% Explicitly stored properties.
user_property(UserName, Property):-
  nonvar_det(user_property_(UserName, Property)).

user_property_(UserName, Property):-
  current_user(UserName, Properties),
  member(Property, Properties).
user_property_(UserName, Property):-
  user_property(UserName, Property).

%! user_db_file(-File:atom) is semidet.
% Returns the file that stores the database of users.
% Fails in case the file does not exist.

user_db_file(File):-
  absolute_file_name(
    project(users),
    File,
    [access(read),file_errors(fail),file_type(database)]
  ).

