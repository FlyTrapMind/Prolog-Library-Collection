:- module(
  pdf,
  [
    open_pdf/1 % +File:atom
  ]
).

/** <module> PDF

Support for PDF files.

@author Wouter Beek
@version 2014/05, 2014/07
*/

:- use_module(os(os_ext)).
:- use_module(os(run_ext)).

:- dynamic(user:file_type_program/2).
:- multifile(user:file_type_program/2).
:- if((is_apple ; is_unix)).
   user:file_type_program(pdf, evince).
   user:file_type_program(pdf, xpdf).
:- endif.
:- if(is_windows).
   user:file_type_program(pdf, 'AcroRd32').
:- endif.



%! open_pdf(+File:atom) is det.
% Opens the given PDF file.
%
% @tbd Test support on Windows.
% @tbd Test support on OS-X.

open_pdf(File):-
  once(find_program_by_file_type(pdf, Program)),
  run_program(Program, [File]).

