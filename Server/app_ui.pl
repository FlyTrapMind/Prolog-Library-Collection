:- module(app_ui, []).

/** <module> SWAPP WWW home

The home page for the SWAPP Website.

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@version 2012/05, 2012/09-2012/12, 2013/02-2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(html(html_list)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(server(app_server)).

:- http_handler(root(.), home, []).

% /css
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- html_resource('http://yui.yahooapis.com/pure/0.3.0/pure-min.css', []).
:- html_resource('http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css', []).

% /img
:- db_add_novel(http:location(img, root(img), [])).
:- db_add_novel(user:file_search_path(img, server(img))).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).
:- html_resource('http://yui.yahooapis.com/3.13.0/build/yui/yui-min.js', []).

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
  % Centering content:
  % `style='margin-left:auto;margin-right:auto;width:50em;`
  html(div(class=content, Content)).

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
      \html_requires('http://yui.yahooapis.com/pure/0.3.0/pure-min.css'),
      \html_requires('http://purecss.io/combo/1.6.5?/css/main.css&/css/menus.css&/css/rainbow/baby-blue.css')
    |
      Head
    ])
  ).

home(_Request):-
  reply_html_page(app_style, [], []).

login -->
  html(
    form([id=login,action='/'],
      div([
        input([id=username,size='10',type=text]),
        input([id=password,size='10',type=password]),
        input([alt='Login',class=btn,src='img/login.gif',type=image])
      ])
    )
  ).

main(Content) -->
  html(div([class='pure-u-1',id=main], \content(Content))).

menu -->
  html(
    div([class='pure-u',id=menu],
      div(class=['pure-menu','pure-menu-open'], [
        a([class='pure-menu-heading',href='/'], 'PraSem'),
        \html_module_list([ordered(false)], [])
      ])
    )
  ),
  {db_replace_novel(menu(true), [r])}.

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

