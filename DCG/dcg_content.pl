:- module(
  dcg_content,
  [
    arrow//1, % +Length:integer
    arrow//2, % +Options:list(nvpair)
              % +Length:integer
    graphic//1, % -Graphic:list(code)
    horizontal_line//1, % +Length:integer
    indent//1, % +Indent:integer
    void//0,
    word//1, % -Word:atom
    word//2 % -Tree:compound
            % ?Word:atom
  ]
).

:- reexport(
  library(dcg/basics),
  [
    alpha_to_lower//1,
    blank//0,
    blanks//0,
    blanks_to_nl//0,
    nonblank//1,
    nonblanks//1,
    prolog_var_name//1,
    white//0,
    whites//0
  ]
).

/** <module> DCG_CONTENT

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(
  indent_size,
  integer,
  2,
  'The number of spaces that go into one indent.'
).



arrow(L) -->
  arrow([], L).

%! arrow(+Options:list(nvpair), +Length:integer)//
% A simple ASCII arrow.
%
% The following options are supported:
%   1. `head(+HeadType:oneof([both,left,right]))`%
%
% Example:
% ~~~{.txt}
% -------->
% ~~~
%
% @arg Options A list of name-value pairs.
% @arg Length A non-negative integer.

arrow(O, L1) -->
  % Set the default arrow head.
  {option(head(Head), O, right)},

  % The left arrow head.
  (
    {arrow_left_head(Head)}
  ->
    less_than_sign,
    {L2 is L1 - 1}
  ;
    {L2 = L1}
  ),

  % The dashes in between the arrow heads.
  (
    {arrow_right_head(Head)}
  ->
    {NumberOfDashes is L2 - 1}
  ;
    {NumberOfDashes = L2}
  ),
  {NumberOfDashes >= 0},
  dcg_multi(hyphen, NumberOfDashes),

  % The right arrow head.
  (
    {arrow_right_head(Head)}
  ->
    greater_than_sign
  ;
    ""
  ).

arrow_left_head(both).
arrow_left_head(left).
arrow_right_head(both).
arrow_right_head(right).

graphic([H|T]) -->
  dcg_graph(H),
  graphic(T).
graphic([]) --> [].

horizontal_line(L) -->
  dcg_multi(hyphen, L).

indent(I) -->
  {
    setting(indent_size, Size),
    NumberOfSpaces is I * Size
  },
  dcg_multi(space, NumberOfSpaces).

void --> [].

%! word(-Word:atom)// is semidet.
% Returns the first word that occurs in the codes list.
%
% A word is defined as any sequence af alphanumeric characters
% and underscores, delimited by any other character.
%
% The delimiting character is not consumed.

word(Word) -->
  {nonvar(Word)}, !,
  {atom_codes(Word, Codes)},
  word_(Codes).
word(Word) -->
  word_(Codes),
  {atom_codes(Word, Codes)}.

%! word(-Tree:compound, ?Word:atom)//

word(word(Word), Word) -->
  word(Word).

word_([H|T]) -->
  (letter(H) ; hyphen_minus(H)),
  word_(T).
word_([]) --> [].

