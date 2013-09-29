:- module(
  dcg_content,
  [
    arrow//1, % +Length:integer
    arrow//2, % +Options:list(nvpair)
              % +Length:integer
    between//3, % +Low:positive_integer
                % +High:positive_integer
                % -Code:code
    between_hex//3, % +LowHex:atom
                    % +HighHex:atom
                    % -Code:code
    crlf//0,
    ci_code//1, % ?Code:code
    ci_string//1, % ?String:string
    code//1, % ?Code:code
    codes//1, % +Codes:list(code)
    collection//2, % +Options:list(nvpair)
                   % +Elements:list
    end_of_line//0,
    graphic//1, % -Graphic:list(code)
    horizontal_line//0,
    horizontal_line//1, % +Length:integer
    indent//1, % +Indent:integer
    nvpair//3, % +Options:list(nvpair)
               % :Name:dcg
               % :Value:dcg
    pair//3,  % +Options:list(nvpair)
              % +Element1
              % +Element2
    proof//2, % +Options:list(nvpair)
              % +Proof:compound
    set//2,  % +Options:list(nvpair)
             % +Elements:list
    spaces//0,
    quote//1, % :DCG_Body
    tuple//2, % +Options:list(nvpair)
              % +Elements:list
    void//0,
    word//1, % ?Word:atom
    word//2 % -Tree:compound
            % ?Word:atom
  ]
).

:- reexport(
  library(dcg/basics),
  [
    alpha_to_lower//1,
    atom//1, % +Atom:atom
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
@version 2013/07-2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(dcg(dcg_unicode)).
:- use_module(generics(option_ext)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(math(radix)).

:- meta_predicate(collection(:,+,?,?)).
:- meta_predicate(list(:,+,?,?)).
:- meta_predicate(nvpair(:,+,+,?,?)).
:- meta_predicate(pair(:,+,+,?,?)).
:- meta_predicate(proof(:,+,?,?)).
:- meta_predicate(quote(//,?,?)).
:- meta_predicate(set(:,+,?,?)).
:- meta_predicate(tuple(:,+,?,?)).

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

between(Low, High, Code) -->
  [Code],
  {between(Low, High, Code)}.

between_hex(LowHex, HighHex, Code) -->
  {number_to_decimal(LowHex, 16, Low)},
  {number_to_decimal(HighHex, 16, High)},
  between(Low, High, Code).

crlf -->
  carriage_return,
  line_feed.

%! ci_code(?Code:code)//
% Generates the case-insensitive variants of the given code.

ci_code(Code) -->
  {nonvar(Code)}, !,
  (
    {code_type(Lower, lower(Upper))}
  ->
    ([Lower] ; [Upper])
  ;
    {code_type(Upper, upper(Lower))}
  ->
    ([Upper] ; [Lower])
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

%! ci_string(?String:list(code))//
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

ci_string(Codes) -->
  dcg_multi1(ci_code, Codes), !.

code(C) -->
  [C].

codes([]) -->
  [].
codes([H|T]) -->
  [H],
  codes(T).

%! collection(+Options:list(nvpair), +Collection:list) is det.
% The following options are supported:
%   1. =|begin(+Begin:atom)|=
%   2. =|end(+End:atom)|=
%   3. =|separator(+Separator:atom)|=
%   4. =|ordering(:Pred)|=
%      The binary predicate that is applied to the collection
%      to determine the order in which its elements occur.
%   5. =|write_method(:Pred)|=
%      The unary predicate that is used for writing the individual items
%      in the collection.

collection(O1, Collection1) -->
  {meta_options(is_meta, O1, O2)},

  % E.g., list -> set.
  {
    option(ordering(P), O2, =),
    once(call(P, Collection1, Collection2))
  },

  % Open a collection.
  {option(begin(Begin), O2)},
  % First we try to include the options.
  % If this DCG rule does not exist, then we exclude the options.
  dcg_catch(
    dcg_call(Begin, O2),
    error(existence_error(procedure,_Predicate1),_Context1),
    dcg_call(Begin)
  ),

  % The contents of the collection.
  collection_(O2, Collection2),

  % End a collection.
  {option(end(End), O2)},

  % First we try to include the options.
  % If this DCG rule does not exist, then we exclude the options.
  dcg_catch(
    dcg_call(End, O2),
    error(existence_error(procedure,_Predicate2),_Context2),
    dcg_call(End)
  ).

% Done!
collection_(_O1, []) --> !.
% Nested collection.
collection_(O1, [H1|T]) -->
  {is_list(H1)}, !,

  % Notice that set members that are sets may contain multiple occurrences,
  % since they will first be explicitly converted to ordset format.
  {
    option(ordering(P), O1, =),
    once(call(P, H1, H2))
  },

  collection(O1, H2),

  collection_(O1, T).
% Next set member.
collection_(O1, [H|T]) -->
  {option(write_method(P), O1, atom)},
  dcg_call(P, H),

  % The separator does not occur after the last collection member.
  {option(separator(Separator), O1)},
  (
    {T == []}, !
  ;
    dcg_call(Separator)
  ),

  collection_(O1, T).

end_of_line -->
  carriage_return, line_feed, !.
end_of_line -->
  line_feed, !.
end_of_line -->
  carriage_return.

graphic([H|T]) -->
  u_graphic(H),
  graphic(T).
graphic([]) --> [].

horizontal_line -->
  % Use the `termcap` library.
  {tty_get_capability(co, number, ScreenWidth)},
  horizontal_line(ScreenWidth).

horizontal_line(L) -->
  dcg_multi(hyphen, L).

indent(I) -->
  {
    setting(indent_size, Size),
    NumberOfSpaces is I * Size
  },
  dcg_multi(space, NumberOfSpaces).

% Meta-options used by predicates in this module.
is_meta(begin).
is_meta(end).
is_meta(ordering).
is_meta(separator).
is_meta(write_method).

langle(O1) -->
  {option(brackets(ascii), O1, ascii)},
  "<".
langle(O1) -->
  {option(brackets(html), O1, ascii)},
  "&lang;".

%! list(+Options:list(nvpair), +List:list)// is det.
% Lists are printed recursively, using indentation relative to the given
% indentation level.

list(O1, List) -->
  {
    meta_options(is_meta, O1, O2),
    merge_options(
      O2,
      [
        begin(opening_square_bracket),
        end(closing_square_bracket),
        separator(comma)
      ],
      O3
    )
  },
  collection(O3, List).

nvpair(O1, N, V) -->
  {
    meta_options(is_meta, O1, O2),
    merge_options(O2, [begin(''),end(';'),separator(': ')], O3)
  },
  collection(O3, [N,V]).

%! pair(+Options:list(nvpair), +X, +Y)// is det.
% Prints the given pair.
%
% The following options are supported:
%   * =|brackets(+Brackets:oneof([ascii,html]))|=
%     The brackets that are printed for this tuple,
%     either using the ASCII characters `<` and `>` (value `ascii`, default)
%     or using the HTML escape sequences `&lang;` and `&rang;`
%     (value `html`).
%   * The options that are supported for print_collection/2.

pair(O1, X, Y) -->
  {
    merge_options(O1, [begin(langle),end(rangle),separator(comma)], O2),
    meta_options(is_meta, O2, O3)
  },
  collection(O3, [X,Y]).

proof(O1, Proof) -->
  {
    meta_options(is_meta, O1, O2),
    default_option(O2, indent, 0, O3)
  },
  proof_(O3, Proof).

proof_(O1, Proof) -->
  {Proof =.. [Rule,Premises,Conclusion]},

  % Indentation.
  {update_option(O1, indent, succ, I, O2)},
  indent(I),

  % The name of the rule that was used for deduction.
  "[", atom(Rule), "]",

  % Separator between rule name and conclusion.
  space,

  % The conclusion.
  proposition(O1, Conclusion),
  newline,

  % Print premises / subproofs.
  dcg_multi1(proof(O2), Premises).

proposition(O1, Proposition) -->
  {
    option(transformation(Predicate), O1, identity),
    call(Predicate, Proposition, Atom)
  },
  atom(Atom).

quote(DCG_Body) -->
  double_quote,
  dcg_call(DCG_Body),
  double_quote.

rangle(O1) -->
  {option(brackets(ascii), O1, ascii)},
  ">".
rangle(O1) -->
  {option(brackets(html), O1, ascii)},
  "&rang;".

set(O1, List) -->
  {
    merge_options(
      O1,
      [
        begin(opening_curly_bracket),
        end(closing_curly_bracket),
        ordering(list_to_ord_set),
        separator(comma)
      ],
      O2
    ),
    meta_options(is_meta, O2, O3)
  },
  collection(O3, List).

spaces -->
  space, !,
  spaces.
spaces -->
  [].

%! tuple(+Options:list(nvpair), +List:list)// is det.
% The following options are supported:
%   * =|brackets(+Brackets:oneof([ascii,html]))|=
%     The brackets that are printed for this tuple,
%     either using the ASCII signs `<` and `>` (value `ascii`, default)
%     or using the HTML escape sequences `&lang;` and `&rang;`
%     (value `html`).
%   * The options that are supported for print_collection/2.

tuple(O1, List) -->
  {
    meta_options(is_meta, O1, O2),
    merge_options(O2, [begin(langle),end(rangle),separator(comma)], O3)
  },
  collection(O3, List).

void -->
  [].

%! word(?Word:atom)// is semidet.
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
  u_letter(H),
  word_(T).
word_([]) --> [].

