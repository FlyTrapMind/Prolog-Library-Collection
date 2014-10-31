:- module(
  shell_ext,
  [
    windows_shell_command/1, % +Command
    windows_shell_command/2 % +Command
                            % +File:atom
  ]
).

/** <module> Shell extensions

Communication with the shell and terminal properties.

@author Wouter Beek
@version 2013/06, 2013/11, 2014/01, 2014/10
*/

:- use_module(library(process)).
:- use_module(os(os_ext)).



%! windows_shell_command(+Command) is det.
%! windows_shell_command(+Command, +File:atom) is det.
% @tbd This has never been tested.

windows_shell_command(Command):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C',Command],
    [stdin(std),stdout(std),stderr(std)]
  ).

windows_shell_command(Command, File):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C',Command,file(File)],
    [stdin(std),stdout(std),stderr(std)]
  ).
