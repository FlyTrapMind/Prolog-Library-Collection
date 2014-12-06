:- module(
  java_ext,
  [
    run_jar/2, % +Jar:atom
               % +Args:list
    run_jar/4 % +Jar:atom
              % +Args:list
              % ?Error:stream
              % ?Output:stream
  ]
).

/** <module> Java extensions

Extensions for executing Java JARs from within Prolog.

@author Wouter Beek
@version 2014/01-2014/02, 2014/12
*/

:- use_module(library(apply)).
:- use_module(library(process)).

:- use_module(generics(db_ext)).
:- use_module(os(run_ext)).

:- db_add_novel(user:prolog_file_type(jar, jar)).
:- db_add_novel(user:prolog_file_type(log, log)).





%! run_jar(+Jar:atom, +Args:list(atom)) is det.
% Runs the given JAR file with the given commandline arguments,
% but does not use the output or error stream.

run_jar(Jar, Args):-
  run_jar(Jar, Args, Output, Error),
  maplist(close, [Output,Error]).



%! run_jar(+Jar:atom, +Args:list(atom), -Error:stream, -Output:stream) is det.
% Runs the given JAR file with the given commandline arguments,
% and uses the given output streams for Java error and Java output.

run_jar(Jar, Args, Error, Output):-
  process_create(
    path(java),
    ['-jar',file(Jar)|Args],
    [process(Pid),stderr(pipe(Error)),stdout(pipe(Output))]
  ),
  process_wait(Pid, exit(ShellStatus)),
  file_base_name(Jar, JarName),
  format(atom(Program), 'Java/JAR ~a', [JarName]),
  exit_code_handler(Program, ShellStatus).
