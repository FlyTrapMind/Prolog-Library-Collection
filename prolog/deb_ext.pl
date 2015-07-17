:- module(
  deb_ext,
  [
    emphasis/1, % +Message:atom
    normal/1, % +Message:atom
    notification/1, % +Message:atom
    success/1, % +Message:atom
    verbose_call/1, % :Goal_0
    verbose_call/2, % +Message:atom
                    % :Goal_0
    warning/1 % +Message:atom
  ]
).

/** <module> Debug extensions

Tools that ease debugging SWI-Prolog programs.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(ansi_term)).

:- meta_predicate(verbose_call(0)).
:- meta_predicate(verbose_call(+,0)).





emphasis(X):-
  ansi_format([bold], '~a', [X]).



normal(X):-
  ansi_format([], '~a', [X]).



notification(X):-
  ansi_format([bold,fg(yellow)], '~a', [X]).



success(X):-
  ansi_format([bold,fg(green)], '~a', [X]).



%! verbose_call(:Goal_0) is det.

verbose_call(Goal_0):-
  term_to_atom(Goal_0, Msg),
  verbose_call(Msg, Goal_0).


%! verbose_call(+Message:atom, :Goal_0) is det.

verbose_call(Msg, Goal_0):-
  setup_call_catcher_cleanup(
    start(Msg),
    Goal_0,
    E,
    (   E == true
    ->  success(Msg)
    ;   failure(Msg)
    )
  ).

failure(Msg):-
  warning('[FAILURE]'),
  normal(' Process '),
  normal(Msg),
  normal(':'),
  nl,
  normal(E).

start(Msg):-
  normal('Starting process '),
  normal(Msg),
  normal('.').

success(Msg):-
  success('[SUCCESS]'),
  normal(' Ending process '),
  normal(Msg),
  normal('.').



warning(X):-
  ansi_format([bold,fg(red)], '~a', [X]).
