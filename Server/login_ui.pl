:- module(login_ui, []).

/** <module> Login UI

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/11
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(server(server_ui)).

:- http_handler(root(login), login_ui, []).



login_ui(_Request):-
  reply_html_page(app_style, \login_js, \login_ui).

login_js -->
  html(
    \js_script({|javascript(_)||
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
    |})
  ).

login_ui -->
  location('/login'),
  login,
  presentation.

login -->
  category('PRESENTATION'),
  login_http.

login_http -->
  login_post,
  login_delete.

login_post -->
  html(tr(td(class=frame, [\login_post_ui,\login_post_authorization]))).

login_post_ui -->
  html(
    span(class=url, [
      \http_button(get, 'POST', 'login();'),
      \login_post_label,
      \clear_button(['login-post-auth-username','login-post-password'])
    ])
  ).

login_post_label -->
  html(code('/login')).

login_post_authorization -->
  html(
    div(style='padding:2px;margin-top:3px;', [
      'Authorization:',
      \login_post_authorization_field1,
      b(':'),
      \login_post_authorization_field2
    ])
  ).

login_post_authorization_field1 -->
  html(
    input([
      id='login-post-auth-username',
      name='login-post-auth-username',
      size='10',
      spellcheck=false,
      style='padding:0;width:7em;',
      type=text,
      value=''
    ])
  ).

login_post_authorization_field2 -->
  html(
    input([
      id='login-post-password',
      name='login-post-password',
      size='10',
      spellcheck=false,
      style='padding:0;width:7em;',
      type=password,
      value=''
    ])
  ).

login_delete -->
  html(
    tr(
      td(class=frame,
        span(class=url, [
          \http_button(get, 'DELETE', 'logout();'),
          \login_delete_label
        ])
      )
    )
  ).

login_delete_label -->
  html(code('/login')).

presentation -->
  html([
    \category('PRESENTATION'),
    tr(
      td(class=frame, [
        'Show as ',
        \presentation_selection,
        &(nbsp),
        &(nbsp),
        &(nbsp),
        'Truncate long strings ',
        \presentation_checkbox
      ])
    )
  ]).

presentation_selection -->
  html(
    select([class=menu,name=presentation],
      option([selected,value='HTML'],'HTML')
    )
  ).

presentation_checkbox -->
  html(input([id=jsonTrunc,name=jsonTrunc,type=checkbox])).

