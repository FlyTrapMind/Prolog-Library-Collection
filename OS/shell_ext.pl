:- module(
  shell_ext,
  [
    shell_status/1 % +Status:integer
  ]
).

/** <module> SHELL_EXT

Communication with the shell.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(process)).



shell_error(Formal, Context):-
  throw(
    error(
      shell_error(Formal),
      context(shell_status/1, Context)
    )
  ).

%! shell_status(+Status:integer) is det.
% Handling of shell exit status codes.
%
% @throws shell_error Throws a shell error when a shell process exits with
%         a non-zero code.

shell_status(exit(StatusCode)):- !,
  shell_status(StatusCode).
shell_status(0):- !.
shell_status(StatusCode):-
  shell_status(StatusCode, Formal, Context), !,
  shell_error(Formal, Context).

shell_status(1, 'Catchall for general errors.', 'Miscellaneous errors.').
shell_status(2, 'Misuse for general errors.',
 'Seldom seen, usually defaults to exit code 1.').
shell_status(126, 'Command cannot be executed.',
  'Permission problem or command is not an executable.').
shell_status(127, 'Command not found.', 'Command could not be found.').
shell_status(128, 'Invalid argument to the exit command.',
  'The exit command takes only integer args in the range 0-255.').
shell_status(130,	'Script terminated by Control-C	', '').

%! windows_shell_command(+Command) is det.
% @tbd Test this.

windows_shell_command(Command):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C', Command],
    [stdin(std), stdout(std), stderr(std)]
  ).

%! windows_shell_command(+Command, +File:atom) is det.
% @tbd Test this.

windows_shell_command(Command, File):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C', Command, file(File)],
    [stdin(std), stdout(std), stderr(std)]
  ).

