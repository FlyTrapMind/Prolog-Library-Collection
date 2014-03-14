:- module(
  pl_term_html,
  [
    pl_term_html//1, % +PL_Term
% CONSTITUENTS
    html_file//1, % +File:atom
    html_files//1, % +Files:list(atom)
    html_mime//1, % +MIME:atom
    html_nvpairs//1 % +NVPairs:list(nvpair)
  ]
).

/** <module> HTML Prolog term

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_meta)).
:- use_module(generics(error_ext)).
:- use_module(http(rfc2616_status_line)).
:- use_module(library(http/html_write)).
:- use_module(math(float_ext)).
:- use_module(math(int_ext)).
:- use_module(rdf_web(rdf_term_html)).



html_arity(Arity) -->
  html(span(class=arity, Arity)).


html_error_action(Action) -->
  html(div(class=action, Action)).


html_error_context(VAR) -->
  {var(VAR)}, !,
  [].
html_error_context(context(Module:Name/Arity,Msg)) --> !,
  html(
    div(class=context, [
      \html_predicate(Module, Name, Arity),
      \html_error_message(Msg)
    ])
  ).
html_error_context(Context) -->
  {atom(Context)}, !,
  html(div(class=context, Context)).


html_error_formal(VAR) -->
  {var(VAR)}, !,
  [].
html_error_formal(domain_error(Type,Term)) --> !,
  html(
    div(class=domain_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(existence_error(Type,Term)) --> !,
  html(
    div(class=existence_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(http_status(StatusCode)) --> !,
  {'Status-Code'(StatusCode, Reason)},
  html(
    div(class=http_status, [
      \html_error_status_code(StatusCode),
      ': ',
      \html_error_reason(Reason)
    ])
  ).
html_error_formal(io_error(Mode,Stream)) -->
  html(
    div(class=io_error, [
      \html_error_mode(Mode),
      \html_error_stream(Stream)
    ])
  ).
html_error_formal(instantiation_error) --> !,
  html(
    div(class=instantiation_error,
      []
    )
  ).
html_error_formal(instantiation_error(Term)) --> !,
  html(
    div(class=instantiation_error,
      \html_error_term(Term)
    )
  ).
html_error_formal(limit_exceeded(max_errors,Max)) --> !,
  html(
    div(class=limit_exceeded, [
      'Max: ',
      span(class=max_errors, Max)
    ])
  ).
html_error_formal(mime_error(_,MustBe_MIME,Is_MIME)) --> !,
  html(
    span(class=mime_error, [
      'Must be ',
      \html_mime(MustBe_MIME),
      ' not ',
      \html_mime(Is_MIME)
    ])
  ).
html_error_formal(permission_error(Action,Type,Term)) --> !,
  html(
    div(class=permission_error, [
      \html_error_action(Action),
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).
html_error_formal(process_error(Program,exit(Status))) --> !,
  html(
    div(class=process_error, [
      \html_program(Program),
      \html_exit_status(Status)
    ])
  ).
html_error_formal(representation_error(Reason)) --> !,
  html(
    div(class=representation_error,
      \html_error_reason(Reason)
    )
  ).
html_error_formal(resource_error(Reason)) --> !,
  html(
    div(class=resource_error,
      \html_error_reason(Reason)
    )
  ).
html_error_formal(socket_error(Reason)) --> !,
  html(
    div(class=socket_error,
      \html_error_reason(Reason)
    )
  ).
html_error_formal(syntax_error(Culprit)) --> !,
  html(
    div(class=syntax_error,
      Culprit
    )
  ).
html_error_formal(timeout_error(Mode,Stream)) --> !,
  html(
    div(class=timeout_error, [
      \html_error_mode(Mode),
      \html_error_stream(Stream)
    ])
  ).
html_error_formal(type_error(Type,Term)) --> !,
  html(
    div(class=type_error, [
      \html_error_type(Type),
      \html_error_term(Term)
    ])
  ).


html_error_message(VAR) -->
  {var(VAR)}, !,
  [].
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
  {term_to_atom(Stream, Atom)},
  html(span(class=stream, Atom)).


html_error_term(Term) -->
  {term_to_atom(Term, Atom)},
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


html_file(File) -->
  html(span(class=file, File)).


%! html_files(+Files:list(atom))// is det.
% Generates an HTML description of the given file names.
%
% @tbd Find common prefixes and shorten the output accordingly,

html_files([]) --> [].
html_files([H|T]) -->
  html([
    \html_file(H),
    br([]),
    \html_files(T)
  ]).


html_functor(Functor) -->
  html(span(class=functor, Functor)).


html_functor_and_arity(Functor, Arity) -->
  html([
    \html_functor(Functor),
    '/',
    \html_arity(Arity)
  ]).


html_mime(MIME) -->
  html(span(class=mime, MIME)).


html_module(Module) -->
  html(span(class=module, Module)).


html_nvpair(Property-Value) -->
  html_nvpair(Property, Value).

html_nvpair(Property, Value) -->
  html([
    span(class=property, \pl_term_html(Property)),
    '=',
    span(class=value, \html_value(Value))
  ]).

html_nvpairs([]) --> [].
html_nvpairs([H|T]) -->
  html([
    \html_nvpair(H),
    br([]),
    \html_nvpairs(T)
  ]).


% Error term.
pl_term_html(error(Formal,Context)) --> !,
  {Formal =.. [ErrorKind|_]},
  html(
    div(class=error, [
      div(class=error_kind,
        ErrorKind
      ),
      div(class=error_formal,
        \html_error_formal(Formal)
      ),
      \html_error_context(Context)
    ])
  ).
% Integer atomic term.
pl_term_html(Term) -->
  {
    to_integer(Term, Integer), !,
    format(atom(FormattedInteger), '~:d', [Integer])
  },
  html(span(class=integer, FormattedInteger)).
% Floating point atomic term.
pl_term_html(Term) -->
  {
    to_float(Term, Float), !,
    format(atom(FormattedFloat), '~G', [Float])
  },
  html(span(class=float, FormattedFloat)).
% Atomic term: atom, blob, or string.
pl_term_html(Atom) -->
  {atomic(Atom)}, !,
  html(span(class=atomic, Atom)).
% Compound terms are converted to atom first.
pl_term_html(PL_Term) -->
  {term_to_atom(PL_Term, Atom)},
  html(span(class=compound, Atom)).


html_predicate(Functor, Arity) -->
  html(
    span(class=predicate,
      \html_functor_and_arity(Functor, Arity)
    )
  ).

html_predicate(Module, Functor, Arity) -->
  html(
    span(class=predicate, [
      \html_module(Module),
      ':',
      \html_functor_and_arity(Functor, Arity)
    ])
  ).


html_program(Program) -->
  html(
    span([], [
      'Program: ',
      span(class=program, Program)
    ])
  ).


html_value(mime(MIME)) --> !,
  html_mime(MIME).
html_value(Value) -->
  rdf_term_html(Value).

