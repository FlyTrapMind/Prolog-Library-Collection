:- module(
  web_ui,
  [
    category//1, % +Category:atom
    clear_button//1 % +Fields:list(atom)
  ]
).

/** <module> Web UI

Generic Web UI components.

This module also defines the dependencies for the JavaScript generics.

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/11
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/js_write)).

% CSS files can be loaded from the `/css` path.
:- db_add_novel(http:location(css, root(css), [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).

% Image files can be loaded from the `/img` path.
:- db_add_novel(http:location(img, root(img), [])).
:- db_add_novel(user:file_search_path(img, server(img))).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

% JavaScript files can be loaded from the `/js` path.
:- db_add_novel(http:location(js, root(js), [])).
:- db_add_novel(user:file_search_path(js, server(js))).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

:- html_resource(js('json2html.js'), []).
:- html_resource(js('jquery.json2html.js'), [requires([js('json2html.js')])]).
:- if(predicate_property(user:debug_project, visible)).
  :- html_resource(js('jquery-debug-2.0.3.js'), []).
  :- html_resource(
    js('generics.js'),
    [requires([js('jquery-debug-2.0.3.js'),js('jquery.json2html.js')])]
  ).
:- else.
  :- html_resource(js('jquery-min-2.0.3.js'), []).
  :- html_resource(
    js('generics.js'),
    [requires([js('jquery-min-2.0.3.js'),js('jquery.json2html.js')])]
  ).
:- endif.



category(C1) -->
  {upcase_atom(C1, C2)},
  html(tr(td(valign=bottom, p(class=c1,C2)))).

%! clear_button(+Fields:list(atom))// is det.

clear_button(Fields) -->
  {dcg_phrase(js_call(clearFields(Fields)), OnClickEvent)},
  html(
    input([
      class='pure-button',
      onclick=OnClickEvent,
      style='float:right;',
      type=button,
      value='Clear'
    ])
  ).

menu -->
  {http_absolute_location(img('api_explorer.png'), RelativeURI, [])},
  html(
    tr(
      td([
        a([href='home',target='_top'],
          img([style='border:0;float:left;',src=RelativeURI])
        ),
        span(
          style=
              'float:right;\c
               font-family:verdana;\c
               font-size:10px;\c
               font-weight:bold;',
          [
            a([href=login,target='_top'],'Login'),
            ' | ',
            a([href=admin,target='_top'],'Admin'),
            ' | ',
            a([href=rdf_db,target='_top'],'RDF DB'),
            ' | ',
            a([href=session_db,target='_top'],'Session DB'),
            ' | ',
            a([href=session_eq,target='_top'],'Session EQ'),
            ' | ',
            a([href=help,target=display],'Help')
          ]
        )
      ])
    )
  ).

