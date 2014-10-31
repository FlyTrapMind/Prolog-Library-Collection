:- module(
  file_gnu,
  [
    file_lines/2, % +File:atom
                  % -NumberOfLines:nonneg
    split_into_smaller_files/3, % +BigFile:atom
                                % +OutputDir:atom
                                % +Prefix:atom
    touch_file/1 % +File:atom
  ]
).

/** <module> File: GNU tools

Interface to GNU tools for file-processing.

@author Wouter Beek
@tbd Replace with native Prolog / OS-independent predicates
     in module [[file_ext]].
@version 2011/08-2012/05, 2012/09, 2013/04-2013/06, 2013/09-2014/01, 2014/05,
         2014/08-2014/10
*/

:- use_module(library(dcg/basics)).
:- use_module(library(process)).
:- use_module(library(readutil)).



%! file_lines(+File:atom, -NumberOfLines:nonneg) is det.
% @tbd Make this OS-independent.

file_lines(File, NumberOfLines):-
  process_create(path(wc), ['-l',file(File)], [stdout(pipe(Stream))]),
  read_stream_to_codes(Stream, Codes),
  phrase(integer(NumberOfLines), Codes).



%! split_into_smaller_files(BigFile, OutputDir, Prefix):-

split_into_smaller_files(BigFile, OutputDir, Prefix):-
  % Split the big file by byte size into small files.
  % (We cannot split on the number of lines since the file is one big line.)
  process_create(
    path(split),
    ['--bytes=1m','-d','--suffix-length=4',BigFile,Prefix],
    [cwd(OutputDir)]
  ),
  print_message(informational, split_file(BigFile,OutputDir)).



%! touch_file(+File:atom) is det.

touch_file(File):-
  process_create(path(touch), [File], [detached(true)]).



% MESSAGE

prolog:message(split_file(BigFile,OutputDir)) -->
  [
    'File ',
    BigFile,
    ' was split into smaller files in directory ',
    OutputDir,
    '.'
  ].
