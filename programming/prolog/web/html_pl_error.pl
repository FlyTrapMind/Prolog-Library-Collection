:- module(
  html_pl_error,
  [
    html_error_context//1, % +Context:compound
    html_error_formal//2 % +Formal:compound
                         % +NaturalLanguage:boolean
  ]
).

/** <module> HTML Prolog error

Generates HTML descriptions of Prolog error terms.

# Exception format

~~~{.pl}
error(
  ErrorType(ErrorSubtype, ErrorTerm),
  context(Predicates, ContextMessage)
)
~~~

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_module(generics(error_ext)).
:- use_module(http(rfc2616_status_line)).
:- use_module(library(http/html_write)).
:- use_module(pl_web(html_pl_generic)).



html_error_action(Action) -->
  html(div(class=action, Action)).


html_error_context(VAR) -->
  {var(VAR)}, !, [].
html_error_context(context(Predicates,Msg)) -->
  {is_list(Predicates)}, !,
  html(
    div(class=context, [
      \html_nested_predicate_sequence(Predicates),
      \html_error_message(Msg)
    ])
  ).
html_error_context(context(Predicate,Msg)) -->
  html(
    div(class=context, [
      \html_predicate(Predicate),
      \html_error_message(Msg)
    ])
  ).
html_error_context(Context) -->
  {atom(Context)}, !,
  html(div(class=context, Context)).


html_error_formal(VAR, _) -->
  {var(VAR)}, !, [].
% Domain error.
html_error_formal(domain_error(Type,Term), false) -->
  html(
    div(class=domain_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(domain_error(Type,Term), true) -->
  html(
    div(class=domain_error, [
      'The term',
      \html_error_term(Term),
      'is of the proper type (i.e., ',
      \html_error_type(Type),
      '), but its value is outside of the domain of supported values.'
    ])
  ).
% Existence error.
html_error_formal(existence_error(Type,Term), false) -->
  html(
    div(class=existence_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(existence_error(Type,Term), true) -->
  html(
    div(class=existence_error, [
      'Term ',
      \html_error_term(Term),
      ' is of the proper type (i.e., ',
      \html_error_type(Type),
      ') and is of the correct domain, ',
      'but there is no existing (external) resource represented by it.'
    ])
  ).
% HTTP status code.
html_error_formal(http_status(StatusCode), false) -->
  {'Status-Code'(StatusCode, Reason)},
  html(
    div(class=http_status, [
      \html_error_status_code(StatusCode),
      ': ',
      \html_error_reason(Reason)
    ])
  ).
% IO error.
% @tbd Add natural language option.
html_error_formal(io_error(Mode,Stream), false) -->
  html(
    div(class=io_error, [
      \html_error_mode(Mode),
      \html_error_stream(Stream)
    ])
  ).
% Instantiation error.
html_error_formal(instantiation_error(Term), false) -->
  html(
    div(class=instantiation_error,
      \html_error_term(Term)
    )
  ).
html_error_formal(instantiation_error(Term), true) -->
  html(
    div(class=instantiation_error, [
      'Term ',
      \html_error_term(Term),
      ' is under-instantiated. I.e. it  is not acceptable as it is,',
      ' but if some variables are  bound to appropriate values ',
      'it would be acceptable.'
    ])
  ).
% Limit exceeded.
% @tbd Add natural language option.
html_error_formal(limit_exceeded(max_errors,Max), false) -->
  html(
    div(class=limit_exceeded, [
      'Max: ',
      span(class=max_errors, Max)
    ])
  ).
% MIME error.
% @tbd Add natural language option.
html_error_formal(mime_error(_,MustBe,Is), false) -->
  html(
    span(class=mime_error, [
      'Must be ',
      \html_mime(MustBe),
      ' not ',
      \html_mime(Is)
    ])
  ).
% Permission error.
html_error_formal(permission_error(Action,Type,Term), false) -->
  html(
    div(class=permission_error, [
      \html_error_action(Action),
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(permission_error(Action,Type,Term), true) -->
  html(
    div(class=permission_error, [

      'It is not allowed to perform action ',
      \html_error_action(Action),
      ' on the object ',
      \html_error_term(Term),
      ' that is of type ',
      \html_error_type(Type),
      '.'
    ])
  ).
% Process error.
% @tbd Add natural language option.
html_error_formal(process_error(Program,exit(Status)), false) -->
  html(
    div(class=process_error, [
      \html_program(Program),
      \html_exit_status(Status)
    ])
  ).
% Representation error.
html_error_formal(representation_error(Reason), false) -->
  html(
    div(class=representation_error,
      \html_error_reason(Reason)
    )
  ).
html_error_formal(representation_error(Reason), true) -->
  html(
    div(class=representation_error, [
      'A limitation of the current Prolog implementation is breached: ',
      \html_error_reason(Reason),
      '.'
    ])
  ).
% Resource error.
% @tbd Add natural language option.
html_error_formal(resource_error(Reason), false) -->
  html(
    div(class=resource_error,
      \html_error_reason(Reason)
    )
  ).
% Shell error.
% @tbd Add formal option.
html_error_formal(shell_error(Culprit), true) -->
  html(
    div(class=shell_error, [
      'The shell encountered the following error: ',
      code(Culprit)
    ])
  ).
% Socket error.
% @tbd Add natural language option.
html_error_formal(socket_error(Reason), false) -->
  html(
    div(class=socket_error,
      \html_error_reason(Reason)
    )
  ).
% Syntax error.
html_error_formal(syntax_error(Culprit), false) -->
  html(
    div(class=syntax_error,
      Culprit
    )
  ).
html_error_formal(syntax_error(Culprit), true) -->
  html(
    div(class=syntax_error, [
      'The following contains invalid syntax: ',
      code(Culprit),
      '.'
    ])
  ).
% Timeout error.
% @tbd Add natural language option.
html_error_formal(timeout_error(Mode,Stream), false) -->
  html(
    div(class=timeout_error, [
      \html_error_mode(Mode),
      \html_error_stream(Stream)
    ])
  ).
% Type error.
html_error_formal(type_error(Type,Term), false) -->
  html(
    div(class=type_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(type_error(Type,Term), true) -->
  html(
    div(class=type_error, [
      'Term ',
      \html_error_term(Term),
      ' is not of type ',
      \html_error_type(Type),
      '.'
    ])
  ).


html_error_message(VAR) -->
  {var(VAR)}, !, [].
html_error_message(Msg) -->
  html(span(class=message, Msg)).


html_error_mode(Mode) -->
  html(span(class=mode, Mode)).


html_error_reason(Reason) -->
  html(div(class=reason, Reason)).


html_error_status_code(StatusCode) -->
  html(span(class=status_code, StatusCode)).


%html_error_stream(Stream) -->
%  {stream_property(Stream, alias(Alias))}, !,
%  html(span(class=stream, Alias)).
%html_error_stream(Stream) -->
%  {stream_property(Stream, file_name(FileName))}, !,
%  html(span(class=stream, FileName)).
html_error_stream(Stream) -->
  {with_output_to(atom(Atom), write_canonical(Stream))},
  html(span(class=stream, Atom)).


html_error_term(Term) -->
  {with_output_to(atom(Atom), write_canonical(Term))},
  html(div(class=term, Atom)).


html_error_type(Type) -->
  html(span(class=error_type, Type)).


html_exit_status(Status) -->
  {exit_code_reason(Status, Reason)},
  html(
    div(class=exit_status, [
      'Status: ',
      span(class=exit_status_code, Status),
      \html_error_reason(Reason)
    ])
  ).


html_mime(MIME) -->
  html(span(class=mime, MIME)).


html_nested_predicate_sequence([]) --> [].
html_nested_predicate_sequence([X]) --> !,
  html_predicate(X).
html_nested_predicate_sequence([H|T]) -->
  html([
    \html_predicate(H),
    ' --> ',
    \html_nested_predicate_sequence(T)
  ]).

