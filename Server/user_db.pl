:- module(
  user_db,
  [
    login/1, % +User:atom
    logout/1, % +User:atom
    logged_in/1, % -User:atom
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
Also keeps track of who is logged in.

The user administration is based on the following:
  * Persistent facts logged_in/3 and user/2.
  * Session management.

@author Jan Wielemaker
@author Torbjörn Lager
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(user_input)).
:- use_module(library(debug)).
:- use_module(library(http/http_session)).
:- use_module(library(persistency)). % Declarations
:- use_module(os(os_ext)).
:- use_module(server(passwords)).

:- db_add_novel(user:prolog_file_type(db, database)).

%! logged_in(?Session:atom, ?User:atom, ?Time:float) is nondet.
:- persistent(logged_in(session:atom,user:atom,time:float)).

%! user(?User:atom, ?Properties:list:compound) is nondet.
:- persistent(user(user:atom,properties:list(compound))).

:- initialization(init_user_db).

:- debug(user_db).



%! current_user(?User:atom, ?Properties:list:compound) is nondet.

current_user(User, Properties):-
  with_mutex(user_db, user(User, Properties)).

%! current_logged_in(?Session:atom, ?User:atom, ?Time:float) is nondet.

current_logged_in(Session, User, Time):-
  with_mutex(user_db, logged_in(Session, User, Time)).

init_user_db:-
  user_db_file(File), !,
  db_attach(File, [sync(close)]).
init_user_db:-
  absolute_file_name(
    project(users),
    File,
    [access(write),file_type(database)]
  ),
  touch(File),
  db_attach(File, [sync(close)]),
  % First time deployment.
  user_add(admin, [roles([admin])]),
  user_input_password('Enter the password for admin.', Password),
  add_password(admin, Password).

%! logged_in(-User:atom) is det.
% Succeeds if the given user name denotes the currently logged in user.

logged_in(User):-
  % Identify the current session.
  http_session_id(Session),
  user_property(User, session(Session)).

%! login(+User:atom) is det.
% Accept the given user as logged into the current session.

login(User) :-
  get_time(LoginTime),
  http_session_id(Session),
  with_mutex(
    user_db,
    (
      retractall_logged_in(Session, _User, _LoginTime),
      assert_logged_in(Session, User, LoginTime)
    )
  ),
  debug(user_db, 'Login user ~w on session ~w.', [User,Session]).

%! logout(+User:atom) is det.
% Logout the given user.

logout(User):-
  with_mutex(user_db, retractall_logged_in(_Session, User, _LoginTime)),
  debug(user_db, 'Logout user ~w.', [User]).

%! user(?User:atom) is nondet.
% Registered users.

user(User):-
  current_user(User, _Properties).

%! user_add(+Name:atom, +Properties:list) is det.
% Adds a new user with the given properties.

user_add(Name, Options):-
  with_mutex(user_db, assert_user(Name, Options)).

user_db_file(File):-
  absolute_file_name(
    project(users),
    File,
    [access(read),file_errors(fail),file_type(database)]
  ).

%! user_property(?User:atom, ?Property:compound) is nondet.
%! user_property(+User:atom, +Property:compound) is semidet.
% Users and their properties.
%
% In addition to properties explicitly stored with users, we define:
%   * =|connection(LoginTime,Idle)|=
%   * =|session(SessionID)|=

% Connection information for a user.
user_property(User, connection(LoginTime,Idle)):- !,
  current_logged_in(Session, User, LoginTime),
  http_current_session(Session, idle(Idle)).
% Session identification for a user.
user_property(User, session(Session)):- !,
  current_logged_in(Session, User, _Time),
  % A session can have at most one user.
  (nonvar(Session) -> ! ; true).
% Explicitly stored properties.
user_property(User, Property):-
  nonvar_det(user_property_(User, Property)).

user_property_(User, Property):-
  current_user(User, Properties),
  member(Property, Properties).
user_property_(User, Property):-
  user_property(User, Property).

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

