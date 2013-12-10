:- module(app_ui, []).

/** <module> Application UI

The Web UI for the PGC application server.

This provides a simple menu with the loaded Web modules.

# Centering content

Sometimes I want to center content (no just text).

~~~{.css}
margin-left: auto;
margin-right: auto;
width: 50em;
~~~

@author Wouter Beek
@version 2013/11-2013/12
*/

:- use_module(html(html_form)).
:- use_module(html(html_list)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(server(app_server)). % Make sure there is an application server.
:- use_module(server(web_login)).
:- use_module(server(web_ui)). % Make sure the Web paths are defined.

:- http_handler(root(.), home, []).
:- http_handler(root(test), test, []).

:- html_resource(css('pure-min-0.3.0.css'), []).
:- html_resource(
  'http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css',
  []
).

:- multifile(user:head//2).
:- multifile(user:body//2).



user:body(app_style, Content) -->
  html(
    body(
      div([class='pure-g-r',id=layout],[
        \menulink,
        \menu,
        \main(Content)
      ])
    )
  ).

content(Content) -->
  html(div([class=content,id=content], Content)).

footer -->
  html(
    footer(class=footer,
      div(class=['pure-menu','pure-menu-horizontal','pure-menu-open'],
        ul(
          li('Developed between 2012/05 and 2013/11 by Wouter Beek.')
        )
      )
    )
  ).

user:head(app_style, Head) -->
  html(
    head([
      \html_requires(css('pure-min-0.3.0.css')),
      \html_requires('http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css')
    |Head])
  ).

home(_Request):-
  reply_html_page(app_style, [], []).

login -->
  html(
    \submission_form('/',
      fieldset(class='pure-group', [
        input([
          class=text,
          id=username,
          required=required,
          size=10,
          type=text
        ], []),
        input([
          class=text,
          id=password,
          required=required,
          size=10,
          type=password
        ], []),
        \submit_button
      ])
    )
  ).

main(Content) -->
  html(div([class='pure-u-1',id=main], \content(Content))).

test(Request):-
  write(Request).

menu -->
  html(
    div([class='pure-u',id=menu],
      div(class=['pure-menu','pure-menu-open'], [
        a([class='pure-menu-heading',href='/'], 'PraSem'),
        \html_module_list([ordered(false)], [])
      ])
    )
  ).

menulink -->
  html(
    a(
      [
        class='pure-menu-link',
        href='/',
        id=menuLink,
        style='position:fixed;left:0;display:block;'
      ],
      span([])
    )
  ).

