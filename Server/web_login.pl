:- module(web_login, []).

/** <module> Web login

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/12
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/js_write)).
:- use_module(server(app_ui)).
:- use_module(server(passwords)).
:- use_module(server(server_ext)).
:- use_module(server(user_db)).
:- use_module(server(web_ui)). % For JavaScript generics.

:- http_handler(root(login), dispatch_login, []).
:- http_handler(root(login_ui), login_ui, []).

:- html_resource(js('login.js'), [requires([js('generics.js')])]).



%! dispatch_login(+Request:list)
% The dispatch method for logging in does not itself use authentication.

dispatch_login(Request):-
  http_method(Request, Method),
  dispatch_method(Method, Request).

%! dispatch_method(+Method, +Request)
%	Handling of `POST` and `DELETE` on `/login`.

% A `DELETE` request on `/login` logs the user out.
dispatch_method(delete, _Request):-
  logged_in(User),
  logout(User),
  reply_json(json([ok= @true,msg=User]), [width(0)]).
% A `POST` request on `/login` logs the user in.
dispatch_method(post, Request):-
  password_file(File),
  http_authenticate(basic(File), Request, [User|_Fields]),
  login(User),
  reply_json(json([ok= @true,msg=User]), [width(0)]).
dispatch_method(get, Request):-
  http_redirect(see_other, root(login_ui), Request).

login_ui(_Request):-
  reply_html_page(app_style, \login_ui_head, \login_ui_body).

login_ui_head -->
  html(\html_requires(js('login.js'))).

login_ui_body -->
  html([
    h1('Login'),
    form([onsubmit='return false;'], [
      fieldset(class='pure-group', [
        legend('Login'),
        div(class='pure-control-group', [
          button([class='pure-button',onclick='login();'], 'POST'),
          code('/login'),
          \clear_button(['login-post-auth-username','login-post-password'])
        ]),
        div(class='pure-control-group', [
          'Authorization:',
          input([
            id='login-post-auth-username',
            name='login-post-auth-username',
            size='10',
            spellcheck=false,
            type=text,
            value=''
          ]),
          ':',
          input([
            id='login-post-password',
            name='login-post-password',
            size='10',
            spellcheck=false,
            type=password,
            value=''
          ])
        ]),
        div(class='pure-control-group', [
          button([class='pure-button',onclick='logout()'], 'DELETE'),
          code('/login')
        ])
      ]),
      fieldset(class='pure-group', [
        legend('Presentation'),
        div(class='pure-control-group', [
          'Show as ',
          select([class=menu,name=presentation],
            option([selected,value='HTML'],'HTML')
          ),
          &(nbsp),
          &(nbsp),
          &(nbsp),
          label([class='pure-checkbox',for=jsonTrunc], [
            input([id=jsonTrunc,name=jsonTrunc,type=checkbox]),
            'Truncate long strings '
          ])
        ])
      ])
    ]),
    div(id=success, [])
  ]).

