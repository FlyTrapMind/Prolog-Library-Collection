:- module(
  dcg_content,
  [
    arrow//1, % +Length:integer
    arrow//2, % +Options:list(nvpair)
              % +Length:integer
    dcg_cicode//1, % +Code:code
    dcg_code//1, % ?Code:code
    dcg_cistring//1, % +String:string
    dcg_codes//1, % +Codes:list(code)
    graphic//1, % -Graphic:list(code)
    horizontal_line//1, % +Length:integer
    indent//1, % +Indent:integer
    spaces//0,
    dcg_void//0,
    dcg_word//1, % -Word:atom
    dcg_word//2 % -Tree:compound
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
@version 2013/07-2013/08
*/

:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
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
% @param Options A list of name-value pairs.
% @param Length A non-negative integer.

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

%! dcg_cicode(+Code:code)//
% Generates the case-insensitive variants of the given code.

dcg_cicode(Lower) -->
  {code_type(Lower, lower(Upper))}, !,
  ([Lower] ; [Upper]).
dcg_cicode(Upper) -->
  {code_type(Upper, upper(Lower))}, !,
  ([Upper] ; [Lower]).
dcg_cicode(Code) -->
  [Code].

dcg_code(C) -->
  [C].

%! dcg_string(+String:list(code))//
% Generates the case-insensitive variants of the given string.
%
% Example:
% ~~~
% ?- phrase(dcg_cistring("http"), Codes) ,atom_codes(Atom, Codes).
% Codes = "http",
% Codes = "httP",
% Codes = "htTp",
% Codes = "htTP",
% Codes = "hTtp",
% Codes = "hTtP",
% Codes = "hTTp",
% Codes = "hTTP",
% Codes = "Http",
% Codes = "HttP",
% Codes = "HtTp",
% Codes = "HtTP",
% Codes = "HTtp",
% Codes = "HTtP",
% Codes = "HTTp",
% Codes = "HTTP",
% false.
% ~~~

dcg_cistring(Codes) -->
  dcg_multi(dcg_cicode, _, Codes, []).

dcg_codes([]) -->
  [].
dcg_codes([H|T]) -->
  [H],
  dcg_codes(T).

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

spaces -->
  space, !,
  spaces.
spaces -->
  [].

dcg_void --> [].

%! dcg_word(-Word:atom)// is semidet.
% Returns the first word that occurs in the codes list.
%
% A word is defined as any sequence af alphanumeric characters
% and underscores, delimited by any other character.
%
% The delimiting character is not consumed.

dcg_word(Word) -->
  {nonvar(Word)}, !,
  {atom_codes(Word, Codes)},
  dcg_word_(Codes).
dcg_word(Word) -->
  dcg_word_(Codes),
  {atom_codes(Word, Codes)}.

%! dcg_word(-Tree:compound, ?Word:atom)//

dcg_word(word(Word), Word) -->
  dcg_word(Word).

dcg_word_([H|T]) -->
  letter(H),
  dcg_word_(T).
dcg_word_([]) --> [].

