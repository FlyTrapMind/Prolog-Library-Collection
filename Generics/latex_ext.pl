:- module(
  latex_ext,
  [
    latex_clean/1, % +File:atom
    latex_clean_directory/1, % +Directory:atom
    latex_convert/1, % +File:atom
    latex_convert/2, % +File:atom
                     % +To:atom
    latex_convert_directory/1, % +Directory:atom
    latex_convert_directory/2 % +From:atom
                              % +To:atom
  ]
).

/** <module> LATEX

Predicates for handling LaTeX files.

@author Wouter Beek
@version 2013/06
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(os_ext)).
:- use_module(library(filesex)).
:- use_module(library(process)).
:- use_module(library(readutil)).

:- db_add_novel(user:prolog_file_type(aux, aux)).
:- db_add_novel(user:prolog_file_type(aux, latex_out)).
:- db_add_novel(user:prolog_file_type(log, log)).
:- db_add_novel(user:prolog_file_type(log, latex_out)).
:- db_add_novel(user:prolog_file_type(pdf, pdf)).
:- db_add_novel(user:prolog_file_type(pdf, latex_out)).
:- db_add_novel(user:prolog_file_type(tex, tex)).
:- db_add_novel(user:prolog_file_type(tex, latex_in)).



%! latex_clean(+File:atom) is det.
% Cleans the LaTeX output files in the given directory recursively.

latex_clean(File):-
  access_file(File, read),
  !,
  forall(
    file_type_alternative(File, latex_out, DeleteFile),
    delete_file(DeleteFile)
  ).

%! latex_clean_directory(+Directory:atom) is det.

latex_clean_directory(Directory):-
  exists_directory(Directory),
  !,
  delete_directory_contents(Directory, latex_out).

latex_convert(File):-
  latex_convert(File, []).

%! latex_convert(+File:atom) is det.

latex_convert(Entry):-
  directory_file_path(To, _File, Entry),
  latex_convert(Entry, To).

latex_convert(Entry, To):-
  is_absolute_file_name(Entry),
  access_file(Entry, read),
  access_file(To, write),
  setup_call_cleanup(
    process_create(
      path(pdflatex),
      Entry,
      [
        cwd(To),
        process(PID),
        stderr(pipe(Error)),
        stdout(pipe(Out))
      ]
    ),
    (
      read_stream_to_codes(Out, OutCodes, []),
      read_stream_to_codes(Error, ErrorCodes, []),
      process_wait(PID, Status)
    ),
    (
      close(Out),
      close(Error)
    )
  ),
  print_error(ErrorCodes, Options),
  print_output(OutCodes, Options),
  shell_status(Status).

latex_convert_directory(Directory):-
  latex_convert_directory(Directory, Directory).

latex_convert_directory(From, To):-
  access_file(From, read),
  directory_files(From, latex_in, Entries),
  access_file(To, write),
  forall(
    member(Entry, Entries),
    latex_convert(Entry, To)
  ).

print_error(Codes, _Options):-
  print_message(warning, latex(error(Codes))).
prolog:message(latex(error(Codes))) -->
  ['~s'-Codes].
