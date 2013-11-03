:- module(app_home, []).

/** <module> SWAPP WWW home

The home page for the SWAPP Website.

@author Wouter Beek
@version 2013/10-2013
*/

:- use_module(generics(db_ext)).
:- use_module(html(html_list)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).

:- http_handler(root(.), home, [priority(10)]).
:- http_handler(root(home), http_redirect(see_other, root(.)), []).

% /css
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css(.)), [prefix]).
:- html_resource(css('app.css'), []).

% /img
:- db_add_novel(http:location(img, root(img), [])).
:- http_handler(img(.), serve_files_in_directory(server(img)), [prefix]).

:- multifile(user:head//2).
:- multifile(user:body//2).



user:body(default_style, Content) -->
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
      p('© Copyright 2009 Torbjörn Lager and Jan Wielemaker')
    ])
  ).

user:head(default_style, Head) -->
  html(
    head([
      \html_requires(css('app.css')),
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
  reply_html_page(app_style, [title('APP')], \home).

home -->
  html(
    div(id='content-main', [
      h2('Welcome to the Logic Programmable Web!'),
      p(id=intro, br([])),
      p([style='margin:5px 0 10px 0;font-weight:bold;font-size:14px;'], [
        'Targeting ',
        a(
          href='http://en.wikipedia.org/wiki/Ajax_(programming)',
          'Ajax'
        ),
        ' and ',
        a(
          href='http://en.wikipedia.org/wiki/Comet_(programming)',
          'Comet'
        ),
        ' developers wishing to build intelligent knowledge-based',
        ' applications as well as ',
        a(
          href='http://en.wikipedia.org/wiki/Prolog',
          'Prolog'
        ),
        ' program',&(shy),'mers and ',
        a(
          href='http://www.w3.org/2001/sw/',
          'Semantic Web'
        ),
        ' developers wanting to equip applications with highly',
        ' interactive Web-based user interfaces, SWAPP attempts to',
        ' provide a ',
        a(
          href='http://en.wikipedia.org/wiki/Representational_State_Transfer',
          'RESTful'
        ),
        ' API to (in principle) all aspects of the ',
        a(
          href='http://www.swi-prolog.org/',
          'SWI-Prolog programming system'
        ),
        ' including its mature and efficient ',
        a(
          href='http://www.swi-prolog.org/pldoc/package/semweb.html',
          'Semantic Web libraries'
        ),
        '.'
      ]),
      h2('SWAPP features'),
      p(id=intro, [br([])]),
      \html_list(
        [ordered(false)],
        [class=big],
        [
          ['Fast in-memory RDF triple store, scalable to approx. 25',
           ' million triples on 32-bit and virtually unlimited on 64-bit',
           ' hardware.'],
          ['Persistent storage using journal files.'],
          ['A RESTful web API for querying and managing data.'],
          ['Querying of RDF graphs through Prolog queries.'],
          ['Efficient a-tuple-at-a-time generation of query results.'],
          ['Query results as Prolog variable bindings encoded in JSON.'],
          ['Rule-based inferencing well-integrated with application code.'],
          ['Rule-based ad-hoc inferencing in the context of querying.'],
          ['Simulated server-push of events for purposes such as',
           ' monitoring.'],
          ['Convenient API Explorers aiding in development and testing.'],
          ['Compatible with any JavaScript library such as YUI, jQuery or',
           ' Dojo']
        ]
      ),
      p([
        'For more information, see the ',
        a(href='about.html',about),
        ' page and then the ',
        a(href='documentation.html',documentation),
        ' page. Or skip ahead to the ',
        a(href='demo.html',demos),
        '.'
      ])
    ])
  ).

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
  {http_absolute_location(img('logo.png'), RelativeURI, [])},
  html(div(id=logo, img([alt='SWAPP',src=RelativeURI],[]))).

nav -->
  html_list(
    [id=nav,ordered(false)],
    [],
    [
      a(href=home, 'Home'),
      a(href=about, 'About'),
      a(href=documentation, 'Documentation'),
      a(href=demo, 'Demo'),
      a(href=download, 'Download'),
      a(href=development, 'Development'),
      a(href=support, 'Support')
    ]
  ).

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

