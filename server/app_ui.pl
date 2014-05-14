:- module(
  app_ui,
  [
    html_external_link//1 % +Url:url
  ]
).

/** <module> Application UI

The Web UI for the PGC application server.

This provides a simple menu with the loaded Web modules.

# Centering content

Sometimes I want to center content (not just text).

~~~{.css}
margin-left: auto;
margin-right: auto;
width: 50em;
~~~

@author Wouter Beek
@version 2013/11-2014/03, 2014/05
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/js_write)).

:- use_module(html(html_form)).
:- use_module(html(html_list)).
:- use_module(server(app_server)). % Make sure there is an application server.
:- use_module(server(web_modules)).

:- http_handler(root(.), home, []).

% CSS files can be loaded from the `/css` path.
http:location(css, root(css), []).
user:file_search_path(css, server(css)).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).

% Image files can be loaded from the `/img` path.
http:location(img, root(img), []).
user:file_search_path(img, server(img)).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

% JavaScript files can be loaded from the `/js` path.
http:location(js, root(js), []).
user:file_search_path(js, server(js)).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

% JSON-2-HTML dependencies.
:- html_resource(js('json2html.js'), []).
:- html_resource(js('jquery.json2html.js'), [requires([js('json2html.js')])]).

% CSS and JS dependencies.
:- if(predicate_property(user:debug_mode, visible)).
  :- html_resource(css('pure-debug-0.4.2.css'), []).
  :- html_resource(css('app_ui.css'), [requires([css('pure-debug-0.4.2.css')])]).
  :- html_resource(js('jquery-debug-2.0.3.js'), []).
  :- html_resource(js('generics.js'),
      [requires([js('jquery-debug-2.0.3.js'),js('jquery.json2html.js')])]).
:- else.
  :- html_resource(css('pure-min-0.4.2.css'), []).
  :- html_resource(css('app_ui.css'), [requires([css('pure-min-0.4.2.css')])]).
  :- html_resource(js('jquery-min-2.0.3.js'), []).
  :- html_resource(js('generics.js'),
      [requires([js('jquery-min-2.0.3.js'),js('jquery.json2html.js')])]).
:- endif.

:- multifile(user:head//2).
:- multifile(user:body//2).



% Main application HTML page.

:- meta_predicate(main(//,?,?)).

user:body(app_style, Content) -->
  html(
    body([
      div([class='pure-g-r',id=layout], [
        \menulink,
        \menu,
        \main(Content)
      ]),
      % Animates the menu.
      \js_script({|javascript(_)||
        (function (window, document) {
          var layout = document.getElementById('layout'),
            menu = document.getElementById('menu'),
            menuLink = document.getElementById('menuLink');
          function toggleClass(element, className) {
            var classes = element.className.split(/\s+/),
              length = classes.length,
              i = 0;
            for(; i < length; i++) {
              if (classes[i] === className) {
              classes.splice(i, 1);
              break;
              }
            }
            // The className is not found
            if (length === classes.length) {
              classes.push(className);
            }
            element.className = classes.join(' ');
          }
          menuLink.onclick = function (e) {
            var active = 'active';
            e.preventDefault();
            toggleClass(layout, active);
            toggleClass(menu, active);
            toggleClass(menuLink, active);
          };
        }(this, this.document));
      |})
    ])
  ).

content(Content) -->
  html(div([class=content,id=content], Content)).

footer -->
  html(
    footer(class=footer,
      div(class=['pure-menu','pure-menu-horizontal','pure-menu-open'],
        ul(li('Developed between 2012/05 and 2014/01 by Wouter Beek.'))
      )
    )
  ).

user:head(app_style, Head) -->
  html(head([\html_requires(css('app_ui.css'))|Head])).

home(_Request):-
  reply_html_page(app_style, [], []).

main(Content) -->
  html(div([class='pure-u-1',id=main], \content(Content))).

menu -->
  html(
    div([class='pure-u',id=menu],
      div(class=['pure-menu','pure-menu-open'], [
        a([class='pure-menu-heading',href='/'], 'PraSem'),
        \html_web_modules_list
      ])
    )
  ).

menulink -->
  html(a([class='menu-link',href='#menu',id=menuLink], span([]))).



% Reusable components

%! html_external_link(+Url:url)// is det.
% Add an HTML link to a URL, using a small anchor image.

html_external_link(Url) -->
  {http_absolute_location(img('url.gif'), LinkImage, [])},
  html(a(href=Url, img(src=LinkImage))).

