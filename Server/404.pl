:- module(
  '404',
  [
    '404'/1 % +Request:list
  ]
).

/** <module> 404

Support for displaying 404 Web pages.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

% All paths that do not have handlers are 404's.
:- http_handler(root(.), '404', [prefix,priority(100)]).



'404'(_Request):-
  reply_html_page(default_style, [title('404')], [h1('404')]).

