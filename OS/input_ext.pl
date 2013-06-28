:- module(
  input_ext,
  [
    read_terms/3 % +Stream:stream
                 % -Terms:list(term)
                 % +Options:list(atom)
  ]
).

/** <module> INPUT_EXT

@author Wouter Beek
@version 2013/06
*/



%! read_terms(+Stream:stream, -Terms:list(term), +Options:list(atom)) is det.
% Returns the terms as they occur on the given stream.
%
% @arg Stream
% @arg Terms
% @arg Options

read_terms(Stream, Terms, Options):-
  read_term(Stream, Term, Options),
  read_terms0(Stream, Term, Terms, Options).

read_terms0(_Stream, end_of_file, [], _Options):-
  !.
read_terms0(Stream, Term, [Term | Terms], Options):-
  read_terms(Stream, Terms, Options).

