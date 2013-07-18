:- module(
  run_ext,
  [
    list_external_programs/0,
    list_external_programs/1, % +FileType:atom
    open/1, % +File:atom
    open_dot/1, % +File:file
    open_in_webbrowser/1, % +URI:atom
    open_pdf/1, % +File:file
    run_script/1 % +Script:atom
  ]
).

/** <module> RUN_EXT

Predicates for running external programs.

@author Wouter Beek
@tbd Add Windows support.
@version 2013/06-2013/07
*/

:- use_module(generics(db_ext)).
:- use_module(generics(exception_handling)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(process)).
:- use_module(library(www_browser)).
:- use_module(os(ansi_ext)).
:- use_module(os(file_ext)).
:- use_module(os(os_ext)).
:- use_module(os(shell_ext)).

:- db_add_novel(user:prolog_file_type(dot,  dot)).
:- db_add_novel(user:prolog_file_type(xdot, dot)).
:- db_add_novel(user:prolog_file_type(pdf,  pdf)).

:- multifile(prolog:message/3).

% This is used to kill the processes that are still running
% when SWI-Prolog halts.
:- dynamic(current_process/1).

% This is used to relate programs to file types.
:- dynamic(user:file_type_program/2).

:- initialization(db_add_novel(at_halt(kill_processes))).



%! exists_program(+Program:atom) is semidet.
% Succeeds if the given program can be run from PATH.

exists_program(Program):-
  catch(
    process_create(path(Program), [], [process(PID)]),
    error(existence_error(_, _)),
    fail
  ),
  process_kill(PID).

%! find_program_by_file_type(+FileType:atom, -Program:atom) is nondet.
% Succeeds if there is at least one existing program that is registered to
% the given file type.

find_program_by_file_type(FileType, Program):-
  file_type_program(FileType, Program),
  exists_program(Program).

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
  setoff(FileType, prolog_file_type(_Extension, FileType), FileTypes),
  maplist(list_external_programs, FileTypes).

%! list_external_programs(+FileType:atom) is det.
% Writes a list of external programs that are registered
% with the given file type to the console.
%
% The list indicates whether the external programs are available or not.
% A file type's external dependencies are met if at least one
% of the external programs is available.

list_external_programs(FileType):-
  findall(Program, file_type_program(FileType, Program), Programs),
  maplist(write_program_support(Support), Programs),
  (Support == true -> SupportText = 'SUPPORTED' ; SupportText = 'UNSUPPORTED'),
  ansi_formatnl(['File type ~w is '-[FileType], [bold,fg(green)]-SupportText-[]]).

write_program_support(true, Program):-
  exists_program(Program), !,
  indent(1),
  ansi_formatnl(['Program ', [bold]-'~w'-[Program], ' is supported.']).
write_program_support(_False, Program):-
  indent(1),
  ansi_formatnl(
    [
      'Program ',
      [bold]-'~w'-[Program],
      ' is ',
      [bold]-'not'-[],
      ' supported.'
    ]
  ).

open(File):-
  file_name_type(_Base, FileType, File),
  generic(open, FileType, [File]).

%! open_dot(+BaseOrFile:atom) is det.
% Opens the given DOT file.
%
% @arg BaseOrFile The atomic name of a DOT file.
%
% @tbd Add support for Windows.
% @tbd Test support on Mac OS-X.

open_dot(BaseOrFile):-
  base_or_file_to_file(BaseOrFile, dot, File),
  os_dependent_call(open_dot(File)).

%! open_dot_unix(+File:atom) is det.
% Opens the DOT file with the given name in UNIX.
%
% This requires the installation of package =dotty=.

:- if((is_mac ; is_unix ; is_windows)).
:- db_add_novel(user:file_type_program(dot, dotty)).
:- db_add_novel(user:file_type_program(dot, dotx)).
open_dot_unix(File):-
  once(find_program_by_file_type(dot, Program)),
  run_program(Program, [File]).
:- endif.

%! open_in_webbrowser(+URI:uri) is det.
% Opens the given URI in the default webbrowser.

open_in_webbrowser(URI):-
  catch(
    (
      www_open_url(URI),
      print_message(informational, open_uri(URI))
    ),
    _Error,
    print_message(informational, open_uri(URI))
  ).

:- if(current_module(web_message)).
prolog:message(Message) -->
  {web_message:web_message(Message)}.
:- endif.
prolog:message(open_uri(URI)) -->
  [
    ansi([], 'Opening URI resource ', []),
    ansi([bg(yellow)], '~w', [URI]),
    ansi([], ' in Web browser.', [])
  ].

%! open_pdf(+BaseOrFile:atom) is det.
% Opens the given PDF file.
%
% @tbd Add support for Windows.
% @tbd Test support on Mac OS-X.

open_pdf(BaseOrFile):-
  base_or_file_to_file(BaseOrFile, pdf, File),
  os_dependent_call(open_pdf(File)).

%! open_pdf_unix(+File:atom) is det.

:- if(is_unix).
:- db_add_novel(user:file_type_program(pdf, xpdf)).
open_pdf_unix(File):-
  once(find_program_by_file_type(pdf, Predicate)),
  run_program(Predicate, [File]).
:- endif.

:- if(is_windows).
open_pdf_windows(_File).
%open_pdf_windows(File):-
%  process_create(path('AcroRd32'), [File], [detached(true), process(PID)]),
%  process_wait(PID, exit(ShellStatus)),
%  shell_status(ShellStatus).
:- endif.

run_program(Program, Args):-
  thread_create(
    (
      process_create(path(Program), Args, [process(PID)]),
      db_add(current_process(PID)),
      process_wait(PID, exit(ShellStatus)),
      format(atom(Message), 'Program \'~w\' threw an exception.', [Program]),
      rethrow(
        shell_status(ShellStatus),
        error(shell_error(FormalMessage), _Context),
        error(shell_error(FormalMessage), context(_Predicate, Message))
      )
    ),
    _,
    [detached(true)]
  ).

%! run_script(+Script:atom) is det.
% Runs the given script.
%
% @arg Script The atomic name of script file.

run_script(Script):-
  os_dependent_call(run_script(Script)).

:- if(is_unix).
run_script_unix(Script):-
  process_create(path(Script), [], []).
:- endif.

:- if(is_windows).
run_script_windows(Script):-
  file_name_type(Script, batch, File),
  win_exec(File, normal).
:- endif.

