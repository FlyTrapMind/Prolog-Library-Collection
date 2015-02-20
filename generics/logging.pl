:- module(
  logging,
  [
    append_to_log/1, % +Format:atom
    append_to_log/2, % +Format:atom
                     % +Arguments:list(term)
    append_to_log/3, % +Topic:atom
                     % +Format:atom
                     % +Arguments:list(term)
    log_entry/3 % ?DateTime
                % ?Topic
                % ?Format
  ]
).

/** <module> Logging

Logging infrastructure.

@author Wouter Beek
@author Sander Latour
@version 2012/05-2012/07, 2013/03-2013/07, 2013/09, 2013/11, 2014/01-2014/02,
         2014/12
*/

:- use_module(library(ansi_term)). % Used in markup.
:- use_module(library(debug)).

:- use_module(generics(code_ext)).
:- use_module(generics(db_ext)).
:- use_module(os(datetime_ext)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- dynamic(logging:current_log_file/1).
:- dynamic(logging:current_log_stream/1).
:- dynamic(logging:situation/1).

:- db_add_novel(user:prolog_file_type(comma_separated_values, csv)).
:- db_add_novel(user:prolog_file_type(log, log)).

:- dynamic(log_entry/3).

:- dynamic(prolog:debug_print_hook/3).
:- multifile(prolog:debug_print_hook/3).

prolog:debug_print_hook(Topic, Format, Args):-
  format(user_output, Format, Args),
  format(user_output, '\n', []),
  append_to_log(Topic, Format, Args).

:- initialization(start_logging).





%! append_to_log(+Format:atom) is det.
% Logs the given message in the current log file.
%
% @see Like format/1.

append_to_log(Format):-
  append_to_log(Format, []).

%! append_to_log(+Format:atom, +Arguments:list(term)) is det.
% Logs the given message in the current log file.
%
% @see Like format/2.

append_to_log(Format, Arguments):-
  append_to_log(no_topic, Format, Arguments).

%! append_to_log(+Topic:atom, +Format:atom, +Arguments:list(term)) is det.
% Logs the given message in the current log file under the given category.

append_to_log(Topic, Format, Arguments):-
  format(atom(Msg), Format, Arguments),
  append_to_log0(Topic, Msg).

append_to_log0(Topic, Msg1):-
  logging:current_log_stream(Stream), !,
  iso8601_dateTime(DateTime),
  atom_codes(Msg1, Codes1),
  codes_remove(Codes1, [10,13], Codes2),
  atom_codes(Msg2, Codes2),
  assert(log_entry(DateTime, Topic, Msg2)),
  % The topic need not be atomic.
  term_to_atom(Topic, TopicName),
  csv_write_stream(
    Stream,
    [row(DateTime,TopicName,Msg2)],
    [file_type(comma_separated_values)]
  ),
  flush_output(Stream).
append_to_log0(Topic, Msg):-
  print_message(warning, cannot_log(Topic, Msg)).



%! close_log_stream is det.
% Closes the current log stream.

close_log_stream:-
  logging:current_log_stream(Stream), !,
  flush_output(Stream),
  close(Stream).
close_log_stream:-
  print_message(warning, no_current_log_stream).



%! end_log is det.
% Ends the current logging activity.

end_log:-
  \+ logging:current_log_file(_), !.
end_log:-
  % Last log message.
  append_to_log(build, 'Goodnight!', []),
  close_log_stream.



%! set_current_log_file(+File:atom) is det.
% Sets the current file where logging messages are stored to.
%
% @arg File The atomic name of a file.

set_current_log_file(File):-
  exists_file(File),
  db_replace(logging:current_log_file(File), [r]).



%! set_current_log_stream(+Out:stream) is det.
% Sets the current stream where logging messages are written to.
%
% @arg Stream A stream.

set_current_log_stream(Out):-
  is_stream(Out),
  db_replace(logging:current_log_stream(Out), [r]).



%! start_logging is det.
% Starts logging.
% This does nothing in case log mode is turned off.

start_logging:-
  logging:current_log_stream(_), !,
  print_message(informational, already_logging).
start_logging:-
  retractall(log_entry(_,_,_)),
  
  % Construct file.
  date_directories(data(.), Dir),
  create_directory(Dir),
  current_time(LocalName),
  create_file(Dir, LocalName, log, File),
  open(File, write, Out, [close_on_abort(true),encoding(utf8)]),
  
  % Store log file and stream.
  set_current_log_file(File),
  set_current_log_stream(Out),
  
  % First log message.
  append_to_log(logging, 'Goodday!', []),
  
  % Make sure logging stops once the program ends.
  at_halt(end_log).





% MESSAGES %

:- multifile(prolog:message/1).

prolog:message(already_logging):-
  [ansi([], 'Logging was already started. End logging before starting it again.', [])].

prolog:message(cannot_log(Kind, Msg)):-
  [
    ansi([bold], '[~w] ', [Kind]),
    ansi([], 'Could not log message "', []),
    ansi([faint], '~w', [Msg]),
    ansi([], '".', [])
  ].

prolog:message(no_current_log_file):-
  [ansi([], 'There is no current log file.', [])].

prolog:message(no_current_log_stream):-
  [ansi([], 'There is no current log stream.', [])].

