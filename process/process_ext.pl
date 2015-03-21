:- module(
  process_ext,
  [
    handle_process/3 % +Process:atom
                     % +Arguments:list
                     % +Options:list(nvpair)
  ]
).

/** <module> Process extensions

@author Wouter Beek
@version 2015/01, 2015/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(thread)).

:- use_module(plc(generics/db_ext)).

% Processes are registered so that they can be killed.
:- dynamic(current_process/1).

:- predicate_options(handle_process/3, 3, [
  output_goal(+callable),
  program(+atom),
  status(-nonneg),
  pass_to(process_create/3, 3)
]).

:- meta_predicate(handle_process(+,+,:)).

is_meta(error_goal).
is_meta(output_goal).

:- at_halt(kill_processes).





%! handle_process(
%!   +Process:atom,
%!   +Arguments:list,
%!   +Options:list(nvpair)
%! ) is det.
% Calls an external process with the given name and arguments,
% and call an internal goal on the output that is coming from
% the external process.
%
% The following options are supported:
%   - detached(+boolean)
%     Whether to run the process in a detached thread or not.
%     Default is `false`.
%   - error_goal(:Goal)
%     Call this goal on the error stream.
%   - output_goal(:Goal)
%     Call this goal on the output stream.
%   - program(+Program:atom)
%     The name of the program as displayed in debug messages.
%     Default is Process.
%   - status(-Status:nonneg)
%   - Other options are passed to process_create/3.
%
% @tbd Output and error is separate threads.

handle_process(Process, Args, Options1):-
  meta_options(is_meta, Options1, Options2),
  (   select_option(detached(true), Options2, Options3)
  ->  thread_create(
        handle_process_inner(Process, Args, Options3),
        _,
        [detached(true)]
      )
  ;   handle_process_inner(Process, Args, Options2)
  ).

handle_process_inner(Process, Args, Options1):-
  option(program(Program), Options1, Process),
  include(process_create_option, Options1, Options2),
  merge_options(
    Options2,
    [process(Pid),stderr(pipe(Error)),stdout(pipe(Output))],
    Options3
  ),
  setup_call_cleanup(
    process_create(path(Process), Args, Options3),
    (
      db_add(current_process(Pid)),
      (   option(output_goal(Goal), Options1)
      ->  call(Goal, Error)
      ;   true
      ),
      % Process the status code.
      process_wait(Pid, exit(Status)),
      exit_code_handler(Program, Status),
      (   option(status(Status0), Options1)
      ->  Status0 = Status
      ;   true
      ),
      % Process the error.
      read_stream_to_codes(Error, ErrorCodes, [])
    ),
    (
      close(Output),
      close(Error)
    )
  ),

  % Process the error stream.
  print_error(ErrorCodes).

process_create_option(cwd(_)).
process_create_option(detached(_)).
process_create_option(env(_)).
process_create_option(priority(_)).
process_create_option(window(_)).

%! exit_code_handler(+Program, +Status:or([compound,nonneg])) is det.
% Handling of exit codes given by programs that are run from the shell.
%
% @arg Program
% @arg Status Either an integer status code,
%      or an integer status code wrapped in functor `exit`.
%
% @throws shell_error Throws a shell error when a shell process exits with
%         a non-zero code.

% Unwrap code.
exit_code_handler(Program, exit(StatusCode)):- !,
  exit_code_handler(Program, StatusCode).
% Success code.
exit_code_handler(_, 0):- !.
% Error/exception code.
exit_code_handler(Program, StatusCode):-
  print_message(warning, status(Program,StatusCode)).

:- multifile(prolog:message//1).

prolog:message(status(Program,StatusCode)) -->
  ['Program ~w returned status code ~w.'-[Program,StatusCode]].

print_error([]):- !.
print_error(Codes):-
  string_codes(String, Codes),
  print_message(warning, String).



%! kill_processes is det.
% Kills all running processes by PID.

kill_processes:-
  % Make sure the PIDs are unique.
  % We do not want to kill the same process twice.
  aggregate_all(
    set(Pid),
    current_process(Pid),
    Pids
  ),
  concurrent_maplist(process_kill, Pids).
