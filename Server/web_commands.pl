:- module(
  web_commands,
  [
    clear_web/1, % -Markup:list
    documentation_web/1, % -Markup:list
    fail_web/1, % -Markup:list
    help_web/1, % -Markup:list
    request_web/2, % +Request:list
                   % -Markup:list
    web_modules_web/1 % -Markup:list
  ]
).

/** <module> Web commands

A collection of generic Web commands
that can be issued via the Web interface.

@author Wouter Beek
@version 2012/10, 2013/02-2013/06, 2013/11
*/

:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
:- use_module(library(doc_http)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(server(web_modules)).



%! clear_web(-Markup:list) is det.
% Clears the output region of the PraSem Web interface.

clear_web([]).

%! documentation_web(-Markup:list) is det.
% Opens a client browser for the documentation server (plDoc).

documentation_web([element(p,[],['Documentation was opened.'])]):-
  doc_browser.

fail_web([element(h1,[],['False'])]).

help_web([element(ul,[],ModuleItems)]):-
  setoff(
    element(li,[],[
      element(p,[],
        [element(b,[],[ExternalName]), ':', element(ol,[],PredicateItems)])]),
    (
      web_module(InternalName, ExternalName, _PathName),
      module_property(InternalName, exports(WebPredicates)),
      setoff(
        element(li,[],[Label]),
        (
          member(WebPredicate/WebArity, WebPredicates),
          atom_concat(Predicate, '_web', WebPredicate),
          DisplayArity is WebArity - 1,
          format(atom(Label), '~w/~w', [Predicate,DisplayArity])
        ),
        PredicateItems
      )
    ),
    ModuleItems
  ).

%! request_web(+Request:list, -Markup:list) is det.
% Returns a table markup element representing the header of
% the given request.
%
% @param Request A compound term representing an HTTP header.
% @param Markup A compound term encoding an (X)HTML table.

request_web(
  Request,
  [
    element(
      table,
      [border=1, summary='This table shows an HTTP header.'],
      [
        element(caption, [], ['An HTTP header'])
      |
        Rows
      ]
    )
  ]
):-
  findall(
    element(tr, [], [element(td, [], [AName]), element(td, [], [AValue])]),
    (
      member(NameValuePair, Request),
      NameValuePair =.. [Name, Value],
      maplist(term_to_atom, [Name, Value], [AName, AValue])
    ),
    Rows
  ).

web_modules_web([
  element(
    table,
    [border=1,summary='The currently registered modules.'],
    [element(caption,[],['The currently registered modules.'])|Rows]
  )
]):-
  web_modules(Pairs),
  pairs_values(Pairs, Modules),
  findall(
    element(tr,[],[element(td, [], [Module])]),
    member(Module, Modules),
    Rows
  ).

