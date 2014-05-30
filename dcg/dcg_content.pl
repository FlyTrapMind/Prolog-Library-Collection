:- module(
  dcg_content,
  [
    arrow//2, % ?Head:oneof([both,left,right])
              % ?Length:nonneg
    atom//1, % ?Atom:atom
    atom//2, % +Atom:atom
             % +Ellipsis:positive_integer
    between_dec//2, % +LowDecimal:nonneg
                    % +HighDecimal:nonneg
    between_dec//3, % +LowDecimal:nonneg
                    % +HighDecimal:nonneg
                    % ?Code:code
    between_hex//2, % +LowHex:atom
                    % +HighHex:atom
    between_hex//3, % +LowHex:atom
                    % +HighHex:atom
                    % ?Code:code
    bracketed//1, % :DCG
    bracketed//2, % +Type:oneof([angular,curly,round,square])
                  % :DCG
    capitalize//0,
    ci_code//1, % ?Code:code
    ci_string//1, % ?String:string
    code//1, % ?Code:code
    codes//1, % +Codes:list(code)
    end_of_line//0,
    graphic//1, % ?Codes:list(code)
    hex_code//1, % +HexadecimalDigit:atom
    hex_code//2, % +HexadecimalDigit:atom
                 % ?Code:code
    horizontal_line//0,
    horizontal_line//1, % ?Length:nonneg
    indent//0,
    indent//1, % +Indent:nonneg
    indent//2, % +Indent:nonneg
               % :DCG
    nl//0,
    pl_term//1, % +PrologTerm
    quoted//1, % :Content
    quoted//2, % +Quote:oneof([double_quote,single_quote,triple_quote(double_quote),triple_quote(single_quote)])
               % :Content
    transition//2, % :From
                   % :To
    triple_quote//1, % :Quote
    void//0,
    word//1 % ?Word:atom
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

/** <module> DCG Content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2013/07-2013/09, 2013/11-2014/05
*/

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_unicode)).
:- use_module(generics(atom_ext)).
:- use_module(math(radix)).
:- use_module(os(shell_ext)).
:- use_module(pl(pl_log)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The number of spaces that go into one indent.'
).

:- meta_predicate(indent(+,//,?,?)).
:- meta_predicate(bracketed(//,?,?)).
:- meta_predicate(bracketed(+,//,?,?)).
:- meta_predicate(quoted(//,?,?)).
:- meta_predicate(quoted(?,//,?,?)).
:- meta_predicate(transition(//,//,?,?)).
:- meta_predicate(triple_quote(//,?,?)).



%! arrow(?Head:oneof([both,left,right]), ?Length:nonneg)// .
% A simple ASCII arrow with a left head, a right head, or left and right heads.
%
% ### Ambiguity
%
% The notation for these ASCII arrows is ambiguous, for example:
% ~~~{.pl}
% ?- phrase((arrow(Head1, Length1), arrow(Head2, Length2)), `<--->`).
% Head1 = left,
% Length1 = 4,
% Head2 = right,
% Length2 = 1 ;
% Head1 = left,
% Length1 = 3,
% Head2 = right,
% Length2 = 2 ;
% Head1 = left,
% Length1 = 2,
% Head2 = right,
% Length2 = 3 ;
% Head1 = left,
% Length1 = 1,
% Head2 = right,
% Length2 = 4 ;
% ~~~

arrow(Head, Length) -->
  arrow_left_head(Head),
  arrow_horizontal_line(Head, Length),
  arrow_right_head(Head).

% ! arrow_horizontal_line(Head:oneof([both,left,right]), ?Length:nonneg)// .

arrow_horizontal_line(Head, L1) -->
  {nonvar(L1)}, !,
  {arrow_length_correction(Head, L1, L2)},
  horizontal_line(L2).
arrow_horizontal_line(Head, L1) -->
  horizontal_line(L2),
  {arrow_length_correction(Head, L1, L2)}.

%! arrow_head(?Head:oneof([both,left,right])) is multi.

arrow_head(both).
arrow_head(left).
arrow_head(right).

%! arrow_left_head(?Head:oneof([both,left,right]))// .

arrow_left_head(Head) -->
  {arrow_head(Head), Head \== right},
  opening_angular_bracket.
arrow_left_head(Head) -->
  {arrow_head(Head), Head == right},
  [].

%! arrow_length_correction(+Head:oneof([both,left,right]), +Length1:nonneg,
%!   -Length2:nonneg) is det.
%! arrow_lenght_correction(+Head:oneof([both,left,right]), -Length1:nonneg,
%!   +Length2:nonneg) is det.

arrow_length_correction(Head, L1, L3):-
  (  nonvar(L1)
  -> arrow_length_correction_left(Head, L1, L2),
     arrow_length_correction_right(Head, L2, L3)
  ;  arrow_length_correction_right(Head, L2, L3),
     arrow_length_correction_left(Head, L1, L2)
  ).

arrow_length_correction_left(Head, L1, L2):-
  arrow_head(Head),
  Head \== right, !,
  succ(L2, L1).
arrow_length_correction_left(_, L, L).

arrow_length_correction_right(Head, L1, L2):-
  arrow_head(Head),
  Head \== left, !,
  succ(L2, L1).
arrow_length_correction_right(_, L, L).

%! arrow_right_head(?Head:oneof([both,left,right]))// .

arrow_right_head(Head) -->
  {arrow_head(Head), Head \== left},
  closing_angular_bracket.
arrow_right_head(Head) -->
  {arrow_head(Head), Head == left},
  [].


atom(Atom, Head, Tail):-
  atom(Atom), !,
  format(codes(Head, Tail), '~w', [Atom]).
atom(Atom) -->
  codes(Codes),
  {atom_codes(Atom, Codes)}.


%! atom(+Atom:atom, +Ellipsis:positive_integer)// .

atom(Atom1, Ellipsis) -->
  {atom_truncate(Atom1, Ellipsis, Atom2)},
  atom(Atom2).


%! between_dec(+LowDecimal:nonneg, +HighDecimal:nonneg)// .
% @see Wrapper around between_dec//3.

between_dec(LowDec, HighDec) -->
  between_dec(LowDec, HighDec, _).

%! between_dec(+LowDecimal:nonneg, +HighDecimal:nonneg, ?Code:code)// .
% Parses or generates a decimal integer between the given limits.
%
% @tbd Support for negative integers and zero.
% @tbd Support for `minf` and `inf`.

between_dec(LowDec, HighDec, C) -->
  [C],
  {between(LowDec, HighDec, C)}.


%! between_hex(+LowHexadecimal:atom, +HighHexadecimal:atom)// .
% @see Wrapper around between_hex//3.

between_hex(LowHex, HighHex) -->
  between_hex(LowHex, HighHex, _).

%! between_hex(+LowHexadecimal:atom, +HighHexadecimal:atom, ?Code:code)// .

between_hex(LowHex, HighHex, Code) -->
  {maplist(hex_value, [LowHex,HighHex], [LowDec,HighDec])},
  between_dec(LowDec, HighDec, Code).


%! bracketed(:DCG)// .
%! bracketed(+Type:oneof([angular,curly,round,square]), :DCG)// .

bracketed(DCG) -->
  bracketed(round, DCG).

bracketed(Type, DCG) -->
  dcg_between(
    opening_bracket(_, Type),
    DCG,
    closing_bracket(_, Type)
  ),
  % Remove choicepoints for brackets of other types in [dcg_ascii].
  !.


%! capitalize// .

capitalize, [Upper] -->
  [Lower],
  {code_type(Upper, to_upper(Lower))}, !,
  dcg_copy.
capitalize -->
  dcg_end.


%! ci_code(?Code:code)// .
% Generates the case-insensitive variants of the given code.

ci_code(Code) -->
  {nonvar(Code)}, !,
  (
    {code_type(Code, lower(Upper))}
  ->
    ([Code] ; [Upper])
  ;
    {code_type(Code, upper(Lower))}
  ->
    ([Code] ; [Lower])
  ;
    [Code]
  ).
ci_code(CI_Code) -->
  % This allows ci_string//1 to use this DCG rule for reading words
  % in a case-sensitive way.
  u_graphic(Code),
  {(
    code_type(Code, upper(CI_Code))
  ;
    CI_Code = Code
  )}, !.


%! ci_string(?String:list(code))// .
% Generates the case-insensitive variants of the given string.
%
% Example:
% ~~~
% ?- phrase(ci_string("http"), Codes) ,atom_codes(Atom, Codes).
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

ci_string([]) --> [].
ci_string([H|T]) -->
  ci_code(H),
  ci_string(T).


%! code(+Code:code)// .

code(C) -->
  [C].


%! codes(+Codes:list(code))// .

codes([]) -->
  [].
codes([H|T]) -->
  [H],
  codes(T).


%! end_of_line// .

end_of_line -->
  carriage_return,
  line_feed, !.
end_of_line -->
  line_feed, !.
end_of_line -->
  carriage_return.


%! graphic(?Codes:list(code))// .

graphic([H|T]) -->
  u_graphic(H),
  graphic(T).
graphic([]) --> [].


hex_code(Hex) -->
  hex_code(Hex, _).

hex_code(Hex, C) -->
  {hex_value(Hex, C)},
  [C].


%! horizontal_line// .

horizontal_line -->
  {terminal_screen_width(ScreenWidth)},
  horizontal_line(ScreenWidth).

%! horizontal_line(?Length:nonneg)// .
% @throws domain_error if length is not an integer.
% @throws type_error if length is a negative integer.

horizontal_line(Length) -->
  '#'(Length, hyphen_minus).


%! indent// is det.
%! indent(+Indent:nonneg)// is det.
%! indent(+Indent:nonneg, :DCG)// is det.

indent -->
  indent(1).

indent(I) -->
  {
    setting(indent_size, Size),
    NumberOfSpaces is I * Size
  },
  'm*n'(NumberOfSpaces, NumberOfSpaces, ` `).

indent(I, DCG) -->
  indent(I),
  dcg_call(DCG).


%! nl// is det.

nl -->
  line_feed.


%! pl_term(+PrologTerm)// is det.

pl_term(PrologTerm) -->
  {with_output_to(codes(Codes), write_canonical_blobs(PrologTerm))},
  Codes.


%! quoted(:Dcg)// .

quoted(Dcd) -->
  dcg_between(double_quote, Dcd).

%! quoted(
%!   ?Quote:oneof([double_quote,single_quote,triple_quote(double_quote),triple_quote(single_quote)]),
%!   :Dcg
%! )// .
% Typical values for `Quote` are:
%   * `double_quote`
%     Result: `"..."`
%   * `single_quote`
%     Result: `'...'`
%   * `triple_quote(double_quote)`
%     Result: `"""..."""`
%   * `triple_quote(single_quote)`
%     Result: `'''...'''`

quoted(Quote, Dcg) -->
  {member(Quote, [
      double_quote,
      single_quote,
      triple_quote(double_quote),
      triple_quote(single_quote)
  ])},
  dcg_between(Quote, Dcg).


%! transition(:From, :To)// is det.

transition(From, To) -->
  dcg_call(From),
  dcg_between(space, arrow(right, 2)),
  dcg_call(To).


%! triple_quote(:Quote)// .
% Typical values for `Quote` are:
%   * `double_quote`
%     Result: `"""`
%   * `single_quote`
%     Result: `'''`

triple_quote(Quote) -->
  Quote,
  Quote,
  Quote.


void --> [].


%! word(?Word:atom)// .
% Returns the first word that occurs in the codes list.
%
% A word is defined as any sequence af alphanumeric characters
% and underscores, delimited by any other character.
%
% The delimiting character is not consumed.

word(Word) -->
  {nonvar(Word)}, !,
  {atom_codes(Word, Codes)},
  word_codes(Codes).
word(Word) -->
  word_codes(Codes),
  {atom_codes(Word, Codes)}.

word_codes([H|T]) -->
  u_letter(H),
  word_codes(T).
word_codes([]) -->
  [].

