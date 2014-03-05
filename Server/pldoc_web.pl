:- module(pldoc_web, []).

/** <module> plDoc handler

Handle requests to view plDoc via a Web interface.

@author Wouter Beek
@version 2013/10, 2014/03
*/

:- use_module(library(doc_http)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(server('404')).
:- use_module(server(web_modules)).

% If you remove the priority option, then this gives errors
% due to conflicting HTTP location declarations.
% SWI-Prolog asserts `pldoc=root(.)`.
http:location(pldoc, root(pldoc_web), [priority(1)]).
:- http_handler(root(pldoc_web), pldoc_web, []).

:- web_module_add('plDoc', pldoc_web).



%! pldoc_web(+Request:list) is det.
% Handles the display of plDoc documentation via a Web interface.
%
% Since documentation may provide hints to hackers we do not display it
% in production evironments (where we show a 404 instread).
%
% @tbd Open tab in the current browser.

pldoc_web(_Request):-
  predicate_property(user:debug_project, visible), !,
  reply_html_page(app_style, [], []).
pldoc_web(Request):-
  '404'(Request).

