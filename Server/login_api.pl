:- module(login_api, []).

/** <module> Login API

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/11
*/

:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(server(dispatch)).
:- use_module(server(login_db)).

:- http_handler(root(login), dispatch_login, []).



%! dispatch_login(+Request:list)
% The dispatch method for logging in does not itself use authentication.

dispatch_login(Request):-
  http_method(Request, Method),
  dispatch_method(login_api, Method, Request).

%! dispatch_method(+Method, +Request)
%	Handling of `POST` and `DELETE` on `/login`.

% A `DELETE` request on `/login` logs the user out.
dispatch_method(delete, _Request):-
  logged_in(User),
  logout(User),
  reply_json(json([ok= @true,msg=User]), [width(0)]).
% A `POST` request on `/login` logs the user in.
dispatch_method(post, Request):-
  http_authenticate(basic(passwords), Request, [User|_Fields]),
  login(User),
  reply_json(json([ok= @true,msg=User]), [width(0)]).
% `GET'
dispatch_method(get, Request):-
gtrace,
  http_authenticate(basic(passwords), Request, [User|_Fields]),
  login(User),
  reply_json(json([ok= @true,msg=User]), [width(0)]).
