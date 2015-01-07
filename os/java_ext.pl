:- module(
  java_ext,
  [
    run_jar/3 % +Jar:atom
              % +Args:list
              % +Options:list(nvpair)
  ]
).

/** <module> Java extensions

Extensions for executing Java JARs from within Prolog.

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).

:- use_module(generics(db_ext)).
:- use_module(os(run_ext)).

:- db_add_novel(user:prolog_file_type(jar, jar)).
:- db_add_novel(user:prolog_file_type(log, log)).

:- predicate_options(run_jar/3, 3, [
     pass_to(process_create/3, 3)
   ]).





%! run_jar(+Jar:atom, +Args:list, +Options:list(nvpair)) is det.
% Runs the given JAR file with the given commandline arguments.
%
% Options are passed to process_create/3.

run_jar(Jar, Args, Options1):-
  merge_options(Options1, [process(Pid),stderr(pipe(Error))], Options2),
  process_create(path(java), ['-jar',file(Jar)|Args], Options2),
  
  % Communicate that end-of-process has occurred.
  process_wait(Pid, exit(ShellStatus)),
  file_base_name(Jar, JarName),
  format(atom(Program), 'Java/JAR ~a', [JarName]),
  exit_code_handler(Program, ShellStatus),
  
  % Process error stream.
  read_stream_to_codes(Error, ErrorCodes),
  print_error(ErrorCodes),
  close(Error).

print_error([]):- !.
print_error(ErrorCodes):-
  string_codes(ErrorString, ErrorCodes),
  print_message(warning, process_error(ErrorString)).

:- multifile(prolog:message//1).
prolog:message(rdf_compress_error(ErrorString)) --> [ErrorString].

