:- module(pl_dev, []).

/** <module> plDev

Web-based tools for Prolog development.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(pl_web(pl_module)).
:- use_module(server(web_modules)).

http:location(pl, root(pl), []).

:- http_handler(pl(dev), pl_dev, []).

:- web_module_add('plDev', pl_dev).



pl_dev(_Request):-
  reply_html_page(app_style, title('plDev'), \pl_modules).


