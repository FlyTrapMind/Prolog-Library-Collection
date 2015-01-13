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
@version 2014/12-2015/01
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
  % Set the program option.
  file_base_name(Jar, JarName),
  format(atom(Program), 'Java/JAR ~a', [JarName]),
  merge_options([program(Program)], Options1, Options2),
  
  handle_process(java, ['-jar',file(Jar)|Args], Options2).

