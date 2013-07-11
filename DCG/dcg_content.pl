:- module(
  dcg_content,
  [
    arrow//1, % +Length:integer
    arrow//2, % +Options:list(nvpair)
              % +Length:integer
    graphic//1, % -Graphic:list(code)
    indent//1, % +Indent:integer
    word//1 % -Word:list(code)
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
% Example:
% ~~~{.txt}
% -------->
% ~~~
%
% @arg Options The following options are supported:
%      1. `head(+HeadType:oneof([both,left,right]))`
% @arg Length A non-negative integer.

arrow(O, L) -->
  {option(head(Head), O, right)},
  ({arrow_left_head(Head)} -> less_than_sign ; ""),
  dcg_multi(hyphen, L),
  ({arrow_right_head(Head)} -> greater_than_sign ; "").

arrow_left_head(both).
arrow_left_head(left).
arrow_right_head(right).

graphic([H|T]) -->
  dcg_graph(H),
  graphic(T).
graphic([]) --> [].

indent(I) -->
  {
    setting(indent_size, Size),
    NumberOfSpaces is I * Size
  },
  dcg_multi(space, NumberOfSpaces).

%! word(-Word:list(code)) is semidet.
% Returns the first word that occurs in the codes list.
%
% A word is defined as any sequence af alphanumeric characters
% and underscores, delimited by any other character.
%
% The delimiting character is not consumed.
%
% @arg Word A list of codes. Codes for uppercase letters are
%           returned as codes for lowercase letters.

word([H|T]) -->
  letter(H),
  word(T).
word([]) --> [].
