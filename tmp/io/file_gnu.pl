:- module(
  file_gnu,
  [
    gnu_mv/2, % +From:atom
              % +To:atom
    split_into_smaller_files/3 % +BigFile:atom
                               % +OutputDir:atom
                               % +Prefix:atom
  ]
).

/** <module> GNU tools for file

Interface to GNU tools for file-processing.

@author Wouter Beek
@version 2011/08-2012/05, 2012/09, 2013/04-2013/06, 2013/09-2014/01, 2014/05,
         2014/08-2014/10, 2015/01-2015/03
*/

:- use_module(library(dcg/basics)).
:- use_module(library(process)).
:- use_module(library(pure_input)).

:- use_module(plc(process/process_ext)).





%! gnu_mv(+From:atom, +To:atom) is det.

gnu_mv(From, To):-
  handle_process(mv, [file(From),file(To)], []).




%! split_into_smaller_files(BigFile, OutputDir, Prefix) is det.

split_into_smaller_files(BigFile, OutputDir, Prefix):-
  % Split the big file by byte size into small files.
  % (We cannot split on the number of lines since the file is one big line.)
  handle_process(
    split,
    ['--bytes=1m','-d','--suffix-length=4',BigFile,Prefix],
    [cwd(OutputDir),program(split)]
  ),
  print_message(informational, split_file(BigFile,OutputDir)).





% MESSAGE %

prolog:message(split_file(BigFile,OutputDir)) -->
  [
    'File ',
    BigFile,
    ' was split into smaller files in directory ',
    OutputDir,
    .
  ].
