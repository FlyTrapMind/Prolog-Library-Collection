:- module(
  run_ext,
  [
    open_dot/1, % +File:file
    open_in_webbrowser/1, % +URI:atom
    open_pdf/1, % +File:file
    run_script/1 % +Script:atom
  ]
).

/** <module> RUN_EXT

@author Wouter Beek
@version 2013/06-2013/07
*/

:- use_module(generics(db_ext)).
:- use_module(library(process)).
:- use_module(library(settings)).
:- use_module(library(www_browser)).
:- use_module(os(file_ext)).
:- use_module(os(os_ext)).
:- use_module(os(shell_ext)).

:- db_add_novel(user:prolog_file_type(dot,  dot)).
:- db_add_novel(user:prolog_file_type(xdot, dot)).
:- db_add_novel(user:prolog_file_type(pdf,  pdf)).

:- multifile(prolog:message/3).

:- setting(
  dot_viewers,
  list,
  [dotty,dotx],
  'The programs that are used to view DOT files, in order of preference.'
).



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
open_dot_unix(File):-
gtrace,
  setting(dot_viewers, L),
  member(Program, L),
  process_create(path(Program), [File], [detached(true)]), !.
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
%
% This requires the installation of package =xpdf=.

:- if(is_unix).
open_pdf_unix(File):-
  process_create(path(xpdf), [File], [detached(true), process(PID)]),
  process_wait(PID, exit(ShellStatus)),
  catch(
    shell_status(ShellStatus),
    error(shell_error(FormalMessage), _Context),
    throw(
      error(
        shell_error(FormalMessage),
        context(
          'os_ext:open_pdf_unix/1',
          'Command \'xpdf\' could not be found.'
        )
      )
    )
  ).
:- endif.

:- if(is_windows).
open_pdf_windows(_File).
%open_pdf_windows(File):-
%  process_create(path('AcroRd32'), [File], [detached(true), process(PID)]),
%  process_wait(PID, exit(ShellStatus)),
%  shell_status(ShellStatus).
:- endif.

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

