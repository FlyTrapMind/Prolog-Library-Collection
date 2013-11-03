:- module(
  login_db,
  [
    login/1, % +User:atom
    logout/1, % +User:atom
    logged_in/1 % -User:atom
  ]
).

/** <module> Login DB

Keeps track of who is logged in.

@author Jan Wielemaker
@author Torbjörn Lager
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2013/10-2013/11
*/

:- use_module(library(debug)).
:- use_module(library(http/http_session)).
:- use_module(library(persistency)). % Declaration persistent/1.

%! logged_in(?Session:atom, ?User:atom, ?Time:float) is nondet.

:- persistent(logged_in(session:atom,user:atom,time:float)).

:- debug(login_db).



%! current_logged_in(?Session:atom, ?User:atom, ?Time:float) is nondet.

current_logged_in(Session, User, Time):-
  with_mutex(login_db, logged_in(Session, User, Time)).

%! logged_in(-User:atom) is det.
% Succeeds if the given user name denotes the currently logged in user.

logged_in(User):-
  % Identify the current session.
  http_session_id(Session),
  user_property(User, session(Session)).

% Connection information for a user.
user_property(User, connection(LoginTime,Idle)):-
  current_logged_in(Session, User, LoginTime),
  http_current_session(Session, idle(Idle)).
% Session identification for a user.
user_property(User, session(Session)):-
  current_logged_in(Session, User, _Time),
  % A session can have at most one user.
  (nonvar(Session) -> ! ; true).

%! login(+User:atom) is det.
% Accept the given user as logged into the current session.

login(User) :-
  get_time(LoginTime),
  http_session_id(Session),
  with_mutex(
    login_db,
    (
      retractall_logged_in(Session, _User, _LoginTime),
      assert_logged_in(Session, User, LoginTime)
    )
  ),
  debug(login_db, 'Login user ~w on session ~w.', [User,Session]).

%! logout(+User:atom) is det.
% Logout the given user.

logout(User):-
  with_mutex(login_db, retractall_logged_in(_Session, User, _LoginTime)),
  debug(login_db, 'Logout user ~w.', [User]).

