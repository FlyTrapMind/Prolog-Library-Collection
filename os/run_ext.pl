:- module(
  run_ext,
  [
    create_process/3, % +Process:atom
                      % +Arguments:list
                      % +Options:list(nvpair)
    create_process/4, % +Process:atom
                      % +Arguments:list
                      % :Goal
                      % +Options:list(nvpair)
    exists_program/1, % +Program:atom
    exit_code_handler/2, % +Program:atom
                         % +Code:or([compound,nonneg])
    find_program_by_file_type/2, % +FileType:atom
                                 % -Predicate:atom
    list_external_programs/0,
    list_external_programs/1, % +FileType:ato
    run_program/2 % +Program:atom
                  % +Arguments:list(atom)
  ]
).

/** <module> Run extensions

Predicates for running external programs.

@author Wouter Beek
@version 2013/06-2013/07, 2013/11, 2014/01-2014/02, 2014/05, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(predicate_options)). % Declarations.
:- use_module(library(process)).
:- use_module(library(readutil)).

:- use_module(generics(db_ext)).
:- use_module(generics(error_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(os(ansi_ext)).
:- use_module(os(file_ext)).
:- use_module(os(os_ext)).

% This is used to kill the processes that are still running
% when SWI-Prolog halts.
:- dynamic(current_process/1).

% This is used to relate programs to file types.
:- dynamic(user:file_type_program/2).
:- multifile(user:file_type_program/2).

% This is used to relate programs to modules.
:- dynamic(user:module_uses/2).
:- multifile(user:module_uses/2).

:- at_halt(kill_processes).

:- meta_predicate(create_process(+,+,1,+)).

:- predicate_options(create_process/3, 3, [
     pass_to(create_process/4, 4)
   ]).
:- predicate_options(create_process/4, 4, [
     pass_to(process_create/3, 3)
   ]).



%! create_process(
%!   +Process:atom,
%!   +Arguments:list,
%!   +Options:list(nvpair)
%! ) is det.

create_process(Process, Args, Options):-
  create_process(Process, Args, idle_goal, Options).
idle_goal(_).


%! create_process(
%!   +Process:atom,
%!   +Arguments:list,
%!   :Goal,
%!   +Options:list(nvpair)
%! ) is det.
% Calls an external process with the given name and arguments,
% and call an internal goal on the output that is coming from
% the external process.
%
% This predicate is only deterministic if the given goal is deterministic.

create_process(Process, Args, Goal, Options1):-
  merge_options(
    [process(Pid),stderr(pipe(Error)),stdout(pipe(Out))],
    Options1,
    Options2
  ),
  setup_call_cleanup(
    process_create(path(Process), Args, Options2),
    (
      call(Goal, Out),
      read_stream_to_codes(Error, ErrorCodes, []),
      process_wait(Pid, Status)
    ),
    (
      close(Out),
      close(Error)
    )
  ),
  print_error(ErrorCodes),
  exit_code_handler(Process, Status).

print_error([]):- !.
print_error(Codes):-
  string_codes(String, Codes),
  print_message(warning, String).


%! exists_program(+Program:atom) is semidet.
% Succeeds if the given program can be run from PATH.

exists_program(Program):-
  catch(
    process_create(path(Program), [], [process(Pid)]),
    error(existence_error(_, _), _Context),
    fail
  ),
  process_kill(Pid).


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
  process_error(Program, StatusCode).


%! find_program_by_file_type(+FileType:atom, -Program:atom) is nondet.
% Succeeds if there is at least one existing program that is registered to
% the given file type.

find_program_by_file_type(FileType, Program):-
  user:file_type_program(FileType, Program),
  exists_program(Program), !.


%! kill_processes is det.
% Kills all running processes by PID.
%
% This is run at halt.

kill_processes:-
  forall(
    current_process(PID),
    process_kill(PID)
  ).


%! list_external_programs is det.
% Writes a list of external programs that are registered
% with some file type to the console.
%
% The list indicates whether the external programs are available or not
% and whether a file type's external dependencies are met.

list_external_programs:-
  aggregate_all(
    set(Module),
    module_uses(Module, _),
    Modules
  ),
  maplist(list_external_programs, Modules).


%! list_external_programs(+Filter:atom) is det.
% Writes a list of external programs that are registered
% with the given file type or module to the console.
%
% The list indicates whether the external programs are available or not.
% A file type's external dependencies are met if at least one
% of the external programs is available.
%
% @param Filter Either a Prolog file type or a Prolog module.

list_external_programs(FileType):-
  findall(
    Program,
    file_type_program(FileType, Program),
    Programs
  ),
  Programs \== [], !,
  list_external_programs_label(Programs, FileType, 'File type').
list_external_programs(Module):-
  findall(
    Program,
    module_uses_program(Module, Program),
    Programs
  ),
  list_external_programs_label(Programs, Module, 'Module').
module_uses_program(Module, Program):-
  user:module_uses(Module, file_type(FileType)),
  user:file_type_program(FileType, Program).
module_uses_program(Module, Program):-
  user:module_uses(Module, program(Program)).

list_external_programs_label(Programs, Content, String):-
  include(write_program_support, Programs, SupportedPrograms),
  (
    SupportedPrograms == []
  ->
    Color = red,
    SupportText = 'UNSUPPORTED'
  ;
    Color = green,
    SupportText = 'SUPPORTED'
  ),
  ansi_formatnl(
    [
      '~w '-[String],
      [bold]-'~w'-[Content],
      ' is ',
      [bold,fg(Color)]-'~w'-[SupportText],
      '.'
    ]
  ).


%! write_program_support(+Program:atom) is semidet.
% Succeeds if the program with the given name exists on PATH.
% Always writes a message to the standard user output as side effect.

write_program_support(Program):-
  exists_program(Program), !,
  indent(1),
  ansi_formatnl(
    [
      'Program ',
      [bold]-'~w'-[Program],
      ' is ',
      [fg(green)]-'supported',
      '.'
    ]
  ).
write_program_support(Program):-
  indent(1),
  ansi_formatnl(
    [
      'Program ',
      [bold]-'~w'-[Program],
      ' is not supported.'
    ]
  ), !, fail.


%! run_program(+Program:atom, +Arguments:list(atom)) is det.

run_program(Program, Args):-
  thread_create(
    (
      process_create(path(Program), Args, [process(PID)]),
      db_add(current_process(PID)),
      process_wait(PID, exit(ShellStatus)),
      exit_code_handler(Program, ShellStatus)
    ),
    _,
    [detached(true)]
  ).

