:- module(web_login_ui, []).

/** <module> Web login

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/11
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(server(app_ui)).
:- use_module(server(web_ui)).

:- http_handler(root(login_ui), login_ui, []).

:- html_resource(js('yui-min-3.14.0.js'), []).



login_ui(_Request):-
  reply_html_page(app_style, \login_ui_head, \login_ui_body).

login_ui_head -->
  html([
    \html_requires(js('yui-min-3.14.0.js')),
    \js_script({|javascript(_)||
      YUI().use('node', 'event', function (Y) {
        function login() {
          switchPageIfNeeded();
          var username = document.getElementById("login-post-auth-username").value;
          var password = document.getElementById("login-post-password").value;
          paste('Click on a JSON property to display a path here...', 'gray');
          YAHOO.util.Connect.initHeader('Authorization','Basic ' + encode64(username + ":" + password));
          YAHOO.util.Connect.asyncRequest("POST", "/login", {
            success: function(o) {
              var response = YAHOO.lang.JSON.parse(o.responseText);
              var html = display(response, "response");
              parent.display.document.getElementById("data").innerHTML = html;
            }
          },"");
        }
        function logout() {
          switchPageIfNeeded();
          paste('Click on a JSON property to display a path here...', 'gray');
          YAHOO.util.Connect.asyncRequest("DELETE", "/login", {
            success: function(o) {
              var response = YAHOO.lang.JSON.parse(o.responseText);
              var html = display(response, "response");
              parent.display.document.getElementById("data").innerHTML = html;
            }
          },"");
        }
      });
    |})
  ]).

login_ui_body -->
  html([
    h1('Login'),
    form([], [
      fieldset(class='pure-group', [
        legend('Login'),
        div(class='pure-control-group', [
          \http_button(get, 'POST', 'login();'),
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
          \http_button(get, 'DELETE', 'logout();'),
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
          'Truncate long strings ',
          input([id=jsonTrunc,name=jsonTrunc,type=checkbox])
        ])
      ])
    ])
  ]).

