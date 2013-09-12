:- module(
  list_script,
  [
    list_script/3 % :Goal
                  % +TodoFile:atom
                  % +DoneFile:atom
  ]
).

/** <module> LIST_SCRIPT

List scripting is the practice of running some arbitrary goal on items
that are read in from a list that is stored in a file.

There are two lists:
  * =Todo.txt=
    Contains all items the goal has to be run on.
  * =Done.txt=
    Contains only those items for which goal was run at some point
    in the past.

The main method read both of these files, applies goal to members of
(Todo minus Done), and add the processed item to Done.

The list stored in Todo never changes.

To process items again one should remove lines in Done.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(readutil)).

:- meta_predicate(list_script(1,+,+)).
:- meta_predicate(list_script(1,+,+,+)).



%! file_to_items(+File:atom, -Items:list) is det.
% Reads a list of items from the given text file.
%
% An item is considered to be the atomic contents of a single line
% in the text file. (There are thus as many items as there are lines.)

file_to_items(File, Items):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(text)]),
    stream_to_items(Stream, Items),
    close(Stream)
  ).

%! load_script(:Goal, +TodoFile:atom, +DoneFile:atom) is det.
% Applies the given goal to items that do occur in the TODO file,
% but that do not occur in the DONE file.
%
% @param Any unary predicate letter.
% @param TodoFile The TODO file must exist.
% @param DoneFile The DONE file may exist.

list_script(Goal, Todo, Done):-
  % The TODO file must exist.
  access_file(Todo, read),
  file_to_items(Todo, TodoItems),
  
  % The DONE file may or may not exist.
  (
    access_file(Done, read)
  ->
    file_to_items(Done, DoneItems),
    Mode = append
  ;
    DoneItems = [],
    Mode = write
  ),
  
  setup_call_cleanup(
    % The mode is either =append= (if the file already exists)
    % or =write= (if the file does not yet exist).
    open(Done, Mode, DoneStream, [encoding(utf8),type(text)]),
    list_script(Goal, TodoItems, DoneItems, DoneStream),
    close(DoneStream)
  ).

%! load_script(:Goal, +Todo:list, +Done:list, +DoneStream:stream) is det.
% @see Helper predicate for load_script/3.

list_script(Goal, TodoItems, DoneItems, DoneStream):-
  member(Item, TodoItems),
  \+ member(Item, DoneItems),
  call(Goal, Item),
  format(DoneStream, '~w\n', [Item]),
  flush_output(DoneStream),
  % Enumerate by failure.
  fail.
list_script(_Goal, _TodoItems, _DoneItems, _DoneStream).

%! stream_to_items(+Stream:stream, -Items:list) is det.
% @see Helper predicate for file_to_items/2.

stream_to_items(Stream, []):-
  at_end_of_stream(Stream),
  !.
stream_to_items(Stream, [H|T]):-
  read_line_to_codes(Stream, Codes),
  atom_codes(H, Codes),
  stream_to_items(Stream, T).

