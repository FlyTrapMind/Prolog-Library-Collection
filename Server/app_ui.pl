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

:- http_handler(root(.), home, [priority(10)]).
:- http_handler(root(home), http_redirect(see_other, root(.)), []).

% /css
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- html_resource(css('app.css'), []).
:- html_resource('http://yui.yahooapis.com/pure/0.3.0/pure-min.css', []).

% /img
:- db_add_novel(http:location(img, root(img), [])).
:- db_add_novel(user:file_search_path(img, server(img))).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

:- multifile(user:head//2).
:- multifile(user:body//2).



user:body(app_style, Content) -->
  html(
    body(
      div(id=page,
        div(id=page2,[\header,\nav,\banner,\content(Content),\footer])
      )
    )
  ).

banner -->
  {http_absolute_location(img('banner.jpg'), RelativeURI, [])},
  html(h1(img([alt='Banner',height='110',src=RelativeURI,width='950'], []))).

content(Content) -->
  html(div(id=content, [\content_group(Content),\content_sub])).

content_group(Content) -->
  html(div(id='content-group',div(id='content-main',Content))).

content_sub -->
  html(div(id='content-sub', [\subnav_admin,\subnav_api])).

footer -->
  html(
    div(id=footer, [
      hr([]),
      p('Developed between 2012/05 and 2013/11 by Wouter Beek.')
    ])
  ).

user:head(app_style, Head) -->
  html(
    head([
      \html_requires(css('app.css')),
      \html_requires('http://yui.yahooapis.com/pure/0.3.0/pure-min.css'),
      style(
        type='text/css',
        'li.lst {\c
          margin: 5px 20px 5px 20px;\c
          font-size: 18px;\c
          list-style: square;\c
        }\c
        ul.innerlst {margin-bottom: 0;}'
      )
    |
      Head
    ])
  ).

header -->
  html(div(id=header,[\logo,\login])).

home(_Request):-
  reply_html_page(app_style, [], []).

login -->
  html(
    form([id=login,action='/'],
      div([
        input([class=text,id=username,size='10',type=text]),
        br([]),
        input([class=text,id=password,size='10',type=password]),
        br([]),
        input([alt='Login',class=btn,src='img/login.gif',type=image])
      ])
    )
  ).

logo -->
  {http_absolute_location(img('logo.jpg'), RelativeURI, [])},
  html(div(id=logo, img([alt=logo,src=RelativeURI],[]))).

nav -->
  html_module_list([id=nav,ordered(false)]).

subnav_admin -->
  html(
    div(class=subnav, [
      h3('Administration'),
      \html_list(
        [ordered(false)],
        [],
        [
          a(href=users_api, 'Users'),
          a(href=settings_api, 'Settings'),
          a(href=statistics_api, 'Statistics')
        ]
      )
    ])
  ).

subnav_api -->
  {http_absolute_location(img('rdf_w3c_icon.128.gif'), RelativeURI, [])},
  html([
    div(class=subnav, [
      h3('API Explorers'),
      \html_list(
        [ordered(false)],
        [],
        [
          a(href=login, 'Login'),
          a(href=admin, 'Admin'),
          a(href=rdfdb, 'RDF DB'),
          a(href=session_db, 'Session DB'),
          a(href=session_eq, 'Session EQ')
        ]
      )
    ]),
    a(
      [
        href='http://www.w3.org/RDF/',
        style='position:absolute;top:775px;margin-left:20px;',
        title='RDF Resource Description Framework'
      ],
      img(
        [
          alt='RDF Resource Description Framework Icon',
          border='0',
          src=RelativeURI
        ],
        []
      )
    )
  ]).

