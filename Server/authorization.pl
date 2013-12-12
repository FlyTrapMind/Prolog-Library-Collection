:- module(
  authorization,
  [
    authorized/2 % +Method:oneof([delete,get,post])
                 % +Request:list
  ]
).

/** <module> Authorization

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/12
*/

:- use_module(library(http/http_authenticate)).
:- use_module(library(lists)).
:- use_module(server(login_db)).
:- use_module(server(password_db)).
:- use_module(server(user_db)).

%! allow(
%!   ?Role:oneof([admin]),
%!   ?User:atom,
%!   ?Method:oneof([delete,get,post]),
%!   ?Path:atom,
%!   ?Request:list
%! ) is nondet.

:- dynamic(allow/5).
:- multifile(allow/5).

%! deny(
%!   ?Role:oneof([admin]),
%!   ?User:atom,
%!   ?Method:oneof([delete,get,post]),
%!   ?Path:atom,
%!   ?Request:list
%! ) is nondet.

:- dynamic(deny/5).
:- multifile(deny/5).



%! authorized(+Method:oneof([delete,get,post]), +Request:list) is semidet.

authorized(Method, Request):-
  memberchk(path(Path), Request),
  password_db_file_unix(File),
  (
    http_authenticate(basic(File), Request, [UserName|_Fields]), !
  ;
    logged_in(UserName), !
  ;
    UserName = anonymous
  ),
  (
    user(UserName, UserProperties),
    memberchk(roles(Roles), UserProperties),
    member(Role, Roles),
    (
      allow(Role, UserName, Method, Path, Request)
    ->
      (
        deny(Role, UserName, Method, Path, Request)
      ->
        throw(http_reply(authorise(basic, 'secure')))
      ;
        Done = true
      )
    ;
      throw(http_reply(authorise(basic, 'secure')))
    ),
    Done == true, !
  ;
    % Deny is the default.
    throw(http_reply(authorise(basic, 'secure')))
  ).

allow(admin, _, post,   '/settings',   _).
allow(_,     _, get,    '/settings',   _).
allow(admin, _, get,    '/statistics', _).
allow(admin, _, post,   '/users',      _).
allow(admin, _, delete, '/users',      _).
allow(_,     _, get,    '/users',      _).
allow(_,     _, _,      '/rdf/db',           _).
allow(_,     _, _,      '/session/db',       _).
allow(_,     _, _,      '/session/eq',       _).

% Deny if user is using a Safari browser
% deny(_, _, _, _, Request) :-
%   memberchk(user_agent(UserAgent), Request),
%   sub_atom(UserAgent, _, _, _, 'Safari').

% Deny if too late in the night (server-side)
% deny(_, _, _, _, Request) :-
%   get_time(TimeStamp),
%   stamp_date_time(TimeStamp, DateTime, local),
%   date_time_value(hour, DateTime, Hour),
%   Hour >= 22.

% Deny if too much content
% deny(_, _, Method, _, Request) :-
%   memberchk(Method, [put, post]),
%   memberchk(content_length(Bytes), Request),
%   Bytes > 1000.

% Deny if no quota
% :- dynamic quota/2.
% deny(_, User, Method, Path, _) :-
%   (   quota(User, N)
%   ->  (   N > 0
%     ->  N1 is N - 1,
%       retractall(quota(User, _)),
%       assert(quota(User, N1)),
%       fail
%     ;   true
%     )
%   ;   assert(quota(User, 4)),
%     fail
%   ).

