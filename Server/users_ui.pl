:- module(users_ui, []).

/** <module> Users UI

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@version 2009, 2013/10-2013/11
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(server(app_ui)).
:- use_module(server(web_ui)).

:- http_handler(root(admin), admin_ui, []).
:- http_handler(root(users), http_redirect(moved, root(admin)), [prefix]).



admin_ui(_Request):-
  reply_html_page(framelike_style, \admin_js, \admin_ui).

% UI %

admin_ui -->
  location('/admin/*'),
  users,
  settings,
  statistics.

% UI: USERS %

users -->
  category('USERS'),
  users_http.

users_http -->
  users_post,
  users_delete,
  users_get.

% UI: USERS: POST %

users_post -->
  html(tr(td(class=frame,[\users_post_ui,\users_post_content]))).

users_post_ui -->
  html(
    span(class=url, [
      \http_button(delete, 'POST', 'postUser();'),
      \users_post_user,
      \users_post_password,
      \clear_button(
        ['user-post-user','user-post-password','user-post-content']
      )
    ])
  ).

users_post_user -->
  users_post_user_label,
  users_post_user_field.

users_post_user_label -->
  html(code('/admin/users?user=')).

users_post_user_field -->
  html(
    input([
      style='padding:0;width:7em;',
      id='user-post-user',
      name='user-post-user',
      type=text,
      size='10',
      spellcheck=false,
      value=aladdin
    ])
  ).

users_post_password -->
  users_post_password_label,
  users_post_password_field.

users_post_password_label -->
  html([&(amp),code('password=')]).

users_post_password_field -->
  html(
    input([
      style='padding:0;width:7em;',
      id='user-post-password',
      name='user-post-password',
      size='10',
      type=password,
      value=''
    ])
  ).

users_post_content -->
  html(
    input([
      class=string,
      id='user-post-content',
      name='user-post-content',
      type=text,
      spellcheck=false,
      value='[roles([user])]'
    ])
  ).

% UI: USERS: DELETE %

users_delete -->
  html(
    tr(
      td(class=frame,
        span(class=url, [
          \http_button(get, 'DELETE', 'deleteUser();'),
          \users_delete_user,
          \clear_button(['user-delete-user'])
        ])
      )
    )
  ).

users_delete_user -->
  users_delete_label,
  users_delete_field.

users_delete_label -->
  html(code('/admin/users?user=')).

users_delete_field -->
  html(
    input([
      style='padding:0;width:7em;',
      id='user-delete-user',
      name=user2,
      type=text,
      size='10',
      spellcheck=false,
      value=aladdin
    ])
  ).

% UI: USERS: GET %

users_get -->
  html(
    tr(
      td(class=frame,
        span(class=url, [
          \http_button(get, 'GET', 'getUsers();'),
          \users_get_user,
          \clear_button(['user-get-user'])
        ])
      )
    )
  ).

users_get_user -->
  users_get_label,
  users_get_field.

users_get_label -->
  html(code('/admin/users?user=')).

users_get_field -->
  html(
    input([
      id='user-get-user',
      name='user-get-user',
      size='10',
      spellcheck=false,
      style='padding:0;width:7em;',
      type=text,
      value=aladdin
    ])
  ).

% UI: SETTINGS %

settings -->
  settings_header,
  settings_http.

settings_header -->
  html(tr(td(valign=bottom, p(class=c1,'SETTINGS')))).

settings_http -->
  settings_post,
  settings_get.

settings_post -->
  html(tr(td(class=frame, [\settings_post_ui,\settings_post_content]))).

settings_post_ui -->
  html(
    span(class=url, [
      \http_button(delete, 'POST', 'postSetting();'),
      \settings_post_module,
      \settings_post_setting,
      \clear_button([
        'setting-post-module',
        'setting-post-setting',
        'setting-post-content'
      ])
    ])
  ).

settings_post_module -->
  settings_post_module_label,
  settings_post_module_field.

settings_post_module_label -->
  html(code('/admin/settings?module=')).

settings_post_module_field -->
  html(
    input([
      style='padding:0;width:7em;',
      id='setting-post-module',
      name='setting-post-module',
      type=text,
      size='10',
      spellcheck=false,
      value=''
    ])
  ).

settings_post_setting -->
  settings_post_setting_label,
  settings_post_setting_field.

settings_post_setting_label -->
  html([&(amp),code('setting=')]).

settings_post_setting_field -->
  html(
    input([
      id='setting-post-setting',
      name='setting-post-setting',
      size='10',
      spellcheck=false,
      style='padding:0;width:7em;',
      type=text,
      value=''
    ])
  ).

settings_post_content -->
  html(
    input([
      class=string,
      id='setting-post-content',
      name='setting-post-content',
      spellcheck=false,
      type=text,
      value=''
    ])
  ).

% UI: SETTINGS: GET

settings_get -->
  html(
    tr(
      td(class=frame,
        span(class=url, [
          \http_button(get, 'GET', 'getSettings();'),
          \settings_get_module,
          \settings_get_setting,
          \clear_button(['setting-get-module','setting-get-setting'])
        ])
      )
    )
  ).

settings_get_module -->
  settings_get_module_label,
  settings_get_module_field.

settings_get_module_label -->
  html(code('/admin/settings?module=')).

settings_get_module_field -->
  html(
    input([
      id='setting-get-module',
      name='setting-get-module',
      type=text,
      size='10',
      spellcheck=false,
      style='padding:0;width:7em;',
      value=''
    ])
  ).

settings_get_setting -->
  settings_get_setting_label,
  settings_get_setting_field.

settings_get_setting_label -->
  html(code('&setting=')).

settings_get_setting_field -->
  html(
    input([
      id='setting-get-setting',
      name='setting-get-setting',
      size='10',
      spellcheck=false,
      style='padding:0;width:7em;',
      type=text,
      value=''
    ])
  ).

% UI: STATISTICS %

statistics -->
  statistics_header,
  statistics_http.

statistics_header -->
  html(tr(td(valign=bottom,p(class=c1,'STATISTICS')))).

statistics_http -->
  statistics_get.

% UI: STATISTICS: GET %

statistics_get -->
  html(
    tr(
      td(class=frame,
        span(class=url, [
          \http_button(get, 'GET', 'getStatistics();'),
          \statistics_get_label
        ])
      )
    )
  ).

statistics_get_label -->
  html(code('/admin/statistics')).

admin_js -->
  html(
    \js_script({|javascript(_)||
      function postUser() {
        switchPageIfNeeded();
        var user = document.getElementById("user-post-user").value;
        var password = document.getElementById("user-post-password").value;
        var content = document.getElementById("user-post-content").value;
        paste('Click on a JSON property to display a path here...', 'gray');
        YAHOO.util.Connect.asyncRequest("POST", "/admin/users?user=" + user + "&password=" + password, {
          success: function(o) {
            var response = YAHOO.lang.JSON.parse(o.responseText);
            var html = display(response, "response");
            parent.display.document.getElementById("data").innerHTML = html;
          }
        }, content);
      }
      function deleteUser() {
        switchPageIfNeeded();
        var user = document.getElementById("user-delete-user").value;
        paste('Click on a JSON property to display a path here...', 'gray');
        YAHOO.util.Connect.asyncRequest("DELETE", "/admin/users?user=" + user, {
          success: function(o) {
            var response = YAHOO.lang.JSON.parse(o.responseText);
            var html = display(response, "response");
            parent.display.document.getElementById("data").innerHTML = html;
          }
        }, "");
      }
      function getUsers() {
        switchPageIfNeeded();
        var user = document.getElementById("user-get-user").value;
        paste('Click on a JSON property to display a path here...', 'gray');
        YAHOO.util.Connect.asyncRequest("GET", "/admin/users?user=" + user, {
          success: function(o) {
            var response = YAHOO.lang.JSON.parse(o.responseText);
            var html = display(response, "response");
            parent.display.document.getElementById("data").innerHTML = html;
          }
        });
      }
      function postSetting() {
        switchPageIfNeeded();
        var module = document.getElementById("setting-post-module").value;
        var setting = document.getElementById("setting-post-setting").value;
        var content = document.getElementById("setting-post-content").value;
        paste('Click on a JSON property to display a path here...', 'gray');
        YAHOO.util.Connect.asyncRequest("POST", "/admin/settings?module=" + module + "&setting=" + setting, {
          success: function(o) {
            var response = YAHOO.lang.JSON.parse(o.responseText);
            var html = display(response, "response");
            parent.display.document.getElementById("data").innerHTML = html;
          }
        }, content);
      }
      function getSettings() {
        switchPageIfNeeded();
        var module = document.getElementById("setting-get-module").value;
        var setting = document.getElementById("setting-get-setting").value;
        paste('Click on a JSON property to display a path here...', 'gray');
        YAHOO.util.Connect.asyncRequest("GET", "/admin/settings?module=" + module + "&setting=" + setting, {
          success: function(o) {
            var response = YAHOO.lang.JSON.parse(o.responseText);
            var html = display(response, "response");
            parent.display.document.getElementById("data").innerHTML = html;
          }
        });
      }
      function getStatistics() {
        switchPageIfNeeded();
        paste('Click on a JSON property to display a path here...', 'gray');
        YAHOO.util.Connect.asyncRequest("GET", "/admin/statistics", {
          success: function(o) {
            var response = YAHOO.lang.JSON.parse(o.responseText);
            var html = display(response, "response");
            parent.display.document.getElementById("data").innerHTML = html;
          }
        });
      }
    |})
  ).

