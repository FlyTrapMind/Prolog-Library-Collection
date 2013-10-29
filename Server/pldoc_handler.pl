:- module(pldoc_handler, []).

/** <module> plDoc handler

Handle requests to view plDoc via a Web interface.

@author Wouter Beek
@version 2013/10
*/

:- use_module(generics(db_ext)).
:- use_module(library(doc_http)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(server('404')).

:- db_add_novel(http:location(debug, root(debug), [])).
% If you remove the priority option, then this gives errors
% due to conflicting HTTP location declarations.
% SWI-Prolog asserts =|pldoc=root(.)|=.
:- db_add_novel(http:location(pldoc, debug(help), [priority(1)])).

:- http_handler(pldoc(.), documentation, [priority(1)]).



%! documentation(+Request:list) is det.
% Handles the display of plDoc documentation via a Web interface.
%
% Since documentation may provide hints to hackers we do not display it
% in production evironments (where we show a 404 instread).

documentation(_Request):-
  user:debug_project, !,
  doc_browser,
  true.
  %home(Request).
documentation(Request):-
  '404'(Request).

