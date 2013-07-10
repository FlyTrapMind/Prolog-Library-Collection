:- module(
  dcg_content,
  [
    dcg_arrow//1, % +Length:integer
    dcg_arrow//2, % +Options:list(nvpair)
                  % +Length:integer
    dcg_graphic//1, % -Graphic:list(code)
    indent//1, % +Indent:integer
    dcg_word//1, % -Word:list(code)
    dcg_word_atom//1 % -Word:atom
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


dcg_arrow(L) -->
  dcg_arrow([], L).

%! dcg_arrow(+Options:list(nvpair), +Length:integer)//
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

dcg_arrow(O, L) -->
  {option(head(Head), O, right)},
  ({dcg_arrow_left_head(Head)} -> less_than_sign ; ""),
  dcg_multi(hyphen, L),
  ({dcg_arrow_right_head(Head)} -> greater_than_sign ; "").

dcg_arrow_left_head(both).
dcg_arrow_left_head(left).
dcg_arrow_right_head(right).

dcg_graphic([H|T]) -->
  dcg_graph(H),
  dcg_graphic(T).
dcg_graphic([]) --> [].

indent(I) -->
  {
    setting(indent_size, Size),
    NumberOfSpaces is I * Size
  },
  dcg_multi(space, NumberOfSpaces).

%! dcg_word(-Word:list(code)) is semidet.
% Returns the first word that occurs in the codes list.
%
% A word is defined as any sequence af alphanumeric characters
% and underscores, delimited by any other character.
%
% The delimiting character is not consumed.
%
% @arg Word A list of codes. Codes for uppercase letters are
%           returned as codes for lowercase letters.

dcg_word([H|T]) -->
  letter(H),
  dcg_word(T).
dcg_word([]) --> [].

dcg_word_atom(Word) -->
  dcg_word(Codes),
  {atom_codes(Word, Codes)}.
