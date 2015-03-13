:- module(
  logging,
  [
    read_log_entry/4 % ?DateTime:atom
                     % ?Kind:atom
                     % ?Term:compound
                     % ?Message:atom
  ]
).

/** <module> Logging

Logging infrastructure.

@author Wouter Beek
@version 2015/03
*/

%:- use_module(library(debug)).
:- use_module(library(persistency)).

:- use_module(plc(os/datetime_ext)).

:- persistent(
  log_entry(datetime:atom, kind:atom, term:compound, message:atom)
).

%:- dynamic(prolog:debug_print_hook/3).
%:- multifile(prolog:debug_print_hook/3).
%
%prolog:debug_print_hook(Topic, Format, Args):-
%  append_to_log(debug(Topic), Format, Args).

:- dynamic(logging:message_kind/1).
:- multifile(logging:message_kind/1).

%logging:message_kind(error).
%logging:message_kind(warning).

:- dynamic(user:message_hook/3).
:- multifile(user:message_hook/3).

user:message_hook(Term, Kind, Lines):-
  once(logging:message_kind(Kind)),
  print_message_lines(atom(Msg), '', Lines),
  iso8601_dateTime(DateTime),
  assert_log_entry(DateTime, Kind, Term, Msg).

:- initialization(init).
init:-
  absolute_file_name(data('error.log'), File, [access(write)]),
  db_attach(File, []).





%! read_log_entry(
%!   ?DateTime:atom,
%!   ?Kind:atom,
%!   ?Term:compound,
%!   ?Message:atom
%! ) is det.

read_log_entry(DateTime, Kind, Term, Msg):-
  log_entry(DateTime, Kind, Term, Msg).
