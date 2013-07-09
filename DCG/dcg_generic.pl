:- module(
  dcg_generic,
  [
% AGGREGATES
    dcg_arrow//1, % +Length:integer
    dcg_arrow//2, % +Options:list(nvpair)
                  % +Length:integer
    dcg_graphic//1, % -Graphic:list(code)
    dcg_indent//1, % +Indent:integer
    dcg_word//1, % -Word:list(code)
    dcg_word_atom//1, % -Word:atom
% ALL/UNTIL
    dcg_all//0,
    dcg_all//1, % -Codes:list(code)
    dcg_all_atom//1, % -Atom:atom
    dcg_until//2, % :End:dcg
                  % -Codes:list(code)
    dcg_until_atom//2, % :End:dcg
                       % -Atom:atom
    dcg_without//2, % :End:dcg
                    % -Codes:list(code)
    dcg_without_atom//2, % :End:dcg
                         % -Atom:atom
% DEBUG
    dcg_debug//0,
% LIST
    dcg_separated_list//2, % :Separator:dcg
                           % -Codess:list(list(codes))
% MULTIPLE OCCURRENCES
    dcg_multi//2, % :DCG_Body:dcg
                  % ?Occurrences:integer
    dcg_multi_list//2, % :DCG_Body:dcg
                       % +List:list
% PEEK
    dcg_peek//1, % ?Code:code
    dcg_peek_atom//1, % -Atom:atom
    dcg_peek_char//1, % ?Char:char
    dcg_peek_length//2, % ?Length:integer
                        % ?Codes:list(code)
% PHRASE EXTENSION
    dcg_phrase/2, % :DCG_Body:dcg
                  % ?In:atom
    dcg_phrase/3, % :DCG_Body:dcg
                  % ?In:atom
                  % ?Out:atom
% RE
    dcg_plus//1, % :DCG_Body:dcg
    dcg_questionmark//1, % :DCG_Body:dcg
    dcg_star//1, % :DCG_Body:dcg
% REPLACE
    dcg_replace//2, % +From:list(code)
                    % +To:list(code)
% WRAPPING
    dcg_wrap//0,
    dcg_wrap//1, % +Options
    dcg_line_wrap//0,
    dcg_line_wrap//1, % +Options
    dcg_word_wrap//0,
    dcg_word_wrap//1 % +Options
  ]
).
:- reexport(
  library(dcg/basics),
  [
    alpha_to_lower//1,
    atom//1,
    blank//0,
    blanks//0,
    blanks_to_nl//0,
    nonblank//1,
    nonblanks//1,
    prolog_var_name//1,
    string//1,
    string_without//2,
    white//0,
    whites//0
  ]
).

/** <module>

Generic DCG clauses.

Shall we use DCGs for parsing or REs?

We need DCGs, because they allow us to define a complex grammar in a
modular way.

REs allow easy quantification using the _Wilcards_ =|*|=, =|+|=, =|?|=,
and the positive integers. This is why we add the DCG rules:
  * dcg_plus//1
  * dcg_questionmark//1
  * dcg_star//1

@author Wouter Beek
@tbd Ask about the combination of meta_predicate/1 and rdf_meta/1.
@tbd Ask about the combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(cowspeak)).
:- use_module(generics(list_ext)).
:- use_module(html(html)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(os(os_ext)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).
:- setting(
  maxmimum_line_width,
  integer,
  80,
  'The default maximum line width, after which line wrapping occurs.'
).

% ALL/UNTIL %
:- meta_predicate(dcg_until(//,-,+,-)).
:- meta_predicate(dcg_until_atom(//,-,+,-)).
:- meta_predicate(dcg_without(//,-,+,-)).
:- meta_predicate(dcg_without_atom(//,-,+,-)).
% LIST %
:- meta_predicate(dcg_separated_list(//,-,+,-)).
% MULTIPLE OCCURRENCES %
:- meta_predicate(dcg_multi(//,?,?,?)).
:- meta_predicate(dcg_multi_list(3,?,?,?)).
%:- meta_predicate(dcg_multi_list(//,?,?,?)).
:- meta_predicate(dcg_multi_nonvar(//,?,?,?)).
:- meta_predicate(dcg_multi_var(//,?,?,?)).
% PEEK %
:- meta_predicate(dcg_peek_length(?,?,?,?)).
:- meta_predicate(dcg_peek_length(+,?,?,?,?)).
% PHRASE EXTENSIONS %
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).
% RE %
:- meta_predicate(dcg_plus(//,?,?)).
:- meta_predicate(dcg_questionmark(//,?,?)).
:- meta_predicate(dcg_star(//,?,?)).
% REPLACE %
:- meta_predicate(dcg_replace(//,//,?,?)).



% AGGREGATES %

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

dcg_indent(I) -->
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



% ALL/UNTIL %

dcg_all --> dcg_all(_).

dcg_all([]) --> [].
dcg_all([H|T]) --> [H], dcg_all(T).

dcg_all_atom(Atom) -->
  {var(Atom)},
  dcg_all(Codes),
  {atom_codes(Atom, Codes)}.
dcg_all_atom(Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_all(Codes).

%! dcg_until(+End:dcg, -Codes:list(code))// is det.
% Returns the codes that occur before =End= can be consumed.
%
% =End= is not an arbitrary DCG body, since disjunction does not play out
% well (note that =End= occurs before and fater the =|-->|=).
%
% We enforce determinism after the first occurrence of =End= is consumed.
%
% @tbd There are problems with list elements: meta_predicate/1 cannot
%      identity the modules for DCGs in this case.

dcg_until(End, []), End --> End, !.
dcg_until(End, [H|T]) -->
  [H],
  dcg_until(End, T).

%! dcg_until_atom(+End:dcg, -Atom:atom)// is det.
% @see dcg_until//2

dcg_until_atom(End, Atom) -->
  {var(Atom)},
  dcg_until(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_until_atom(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_until(End, Codes).

%! dcg_without(+End:dcg, -Codes:list(code))// is det.

dcg_without(End, []) --> End, !.
dcg_without(End, [H|T]) -->
  [H],
  dcg_without(End, T).

dcg_without_atom(End, Atom) -->
  {var(Atom)},
  dcg_without(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_without_atom(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_without(End, Codes).



% DEBUG %

dcg_debug(Codes, []):-
  atom_codes(Atom, Codes),
  format(
    atom(Text),
    'Wouter, I have the feeling that something is wrong here.\n\c
     Unable to phrase <~w>\n',
    [Atom]
  ),
  thread_create(cowspeak(Text), _ID, []),
  gtrace. %DEB



% LIST %

dcg_separated_list(Separator, [H|T]) -->
  string(H),
  % Allow symmetric spaces.
  (Separator ; blank, Separator, blank),
  !,
  dcg_separated_list(Separator, T).
dcg_separated_list(_Separator, [H]) -->
  string(H).



% MULTIPLE OCCURRENCES %

%! dcg_multi(:DCG_Body, ?Occurrences:integer)
% Counts the consecutive occurrences of the given DCG body.
% Or produces the given number of occurrences of the given DCG body.

dcg_multi(DCG_Body, N) -->
  {nonvar(N)}, !,
  dcg_multi_nonvar(DCG_Body, N).
dcg_multi(DCG_Body, N) -->
  {var(N)}, !,
  dcg_multi_var(DCG_Body, N).

dcg_multi_nonvar(_DCGBody, 0) --> !, [].
dcg_multi_nonvar(DCG_Body, N) -->
  DCG_Body, !,
  {NewN is N - 1},
  dcg_multi_nonvar(DCG_Body, NewN).

dcg_multi_var(DCG_Body, N) -->
  DCG_Body, !,
  dcg_multi_var(DCG_Body, N_),
  {N is N_ + 1}.
dcg_multi_var(_DCGBody, 0) --> [].

%! dcg_multi_list(:DCG_Body, +List:list)//

dcg_multi_list(_DCG_Body, []) --> [].
dcg_multi_list(DCG_Body1, [H|T], In, Out):-
  strip_module(DCG_Body1, Module, Plain_DCG_Body1),
  Plain_DCG_Body1 =.. [P | Args1],
  append(Args1, [H], Args2),
  DCG_Body2 =.. [P | Args2],
  call(Module:DCG_Body2, In, Temp),
  dcg_multi_list(DCG_Body1, T, Temp, Out).



% PEEK %

%! dcg_peek(-X:code) is semidet.
% Returns the next code in the codes list, if any.
% Does not consume anything.

dcg_peek(X), [X] --> [X].

%! dcg_peek_atom(+Atom:atom) is semidet.
% Succeeds if the given atom occurs next in the codes list,
% but does not consume anything.

dcg_peek_atom(Atom), Codes -->
  {atom_codes(Atom, Codes)},
  Codes.

%! dcg_peek_char(?Char) is semidet.

dcg_peek_char(Char), [Code] -->
  {nonvar(Char)},
  {char_code(Char, Code)},
  [Code].
dcg_peek_char(Char), [Code] -->
  {var(Char)},
  [Code],
  {char_code(Char, Code)}.

%! dcg_peek_length(?Length:integer, ?Peek:list(code)) is nondet.

% In order to prevent the generative call of DCG rule to result in an
% infinite loop over increasingly bigger lengths in length/2, we
% explicitly instigate an upper bound for length.
% This upper bound is the length of the code list that is parsed.
dcg_peek_length(Length, Peek, In, Out):-
  length(In, MaxLength),
  dcg_peek_length(MaxLength, Length, Peek, In, Out).
dcg_peek_length(MaxLength, Length, Peek), Peek -->
  {
    % Effectuate the upper bound to length.
    % In the generative call this is the predicate that backtracks, causing
    % the peek list to grow until it reaches the maximum allowed size.
    between(0, MaxLength, Length),
    % The length is always instantiated at this point.
    length(Peek, Length)
  },
  Peek.



% PHRASE EXTENSION

% Codes match codes.
dcg_phrase(DCG_Body, In):-
  is_list(In), !,
  dcg_phrase(DCG_Body, In, []).
% Atom matches atom.
dcg_phrase(DCG_Body, In):-
  dcg_phrase(DCG_Body, In, '').

dcg_phrase(DCG_Body, In, Out):-
  is_list(In), !,
  phrase(DCG_Body, In, Out).
dcg_phrase(DCG_Body, In1, Out1):-
  atomic(In1), !,
  atom_codes(In1, In2),
  dcg_phrase(DCG_Body, In2, Out2),
  atom_codes(Out1, Out2).



% RE %

%! dcg_plus(:DCG_Body:dcg) is nondet.
% Applies the given DCG_Body one or more times to the codes list.
%
% The longest codes list that can be parsed by the given DCG body
% is returned first. The singleton codes list is retuned last.
%
% @compat Inspired by the regular expression operator =+=.

dcg_plus(DCG_Body) -->
  DCG_Body,
  dcg_plus(DCG_Body).
dcg_plus(DCG_Body) -->
  DCG_Body.

dcg_questionmark(DCG_Body) -->
  (DCG_Body ; "").

%! dcg_star(:DCG_Body:dcg) is nondet.
% Applies the given DCG_Body zero or more times to the codes list.
%
% The longest codes list that can be parsed by the given DCG body
% is returned first. The empty codes list is retuned last.
%
% @compat Inspired by the regular expression operator =*=.

dcg_star(DCG_Body) -->
  DCG_Body,
  dcg_star(DCG_Body).
dcg_star(_DCGBody) --> [].



% REPLACE %

dcg_end([], []).

%! dcg_replace(:From:dcg, :To:dcg) is det.
% @author http://stackoverflow.com/users/1613573/mat
% @see http://stackoverflow.com/questions/6392725/using-a-prolog-dcg-to-find-replace-code-review

dcg_replace(_From, _To) -->
  dcg_end, !.
dcg_replace(From, To), To -->
  From, !,
  dcg_replace(From, To).
dcg_replace(From, To), [X] -->
  [X],
  dcg_replace(From, To).



% WRAPPING %

%! dcg_wrap//
% @see dcg_wrap//1

dcg_wrap -->
  dcg_wrap([]).

%! dcg_wrap(+Options)//

dcg_wrap(Options) -->
  {select_option(wrap(Mode), Options, RestOptions, word)},
  dcg_wrap_(Mode, RestOptions).
dcg_wrap_(line, Options) --> dcg_line_wrap(Options), !.
dcg_wrap_(none, _Options) --> dcg_all, !.
dcg_wrap_(word, Options) --> dcg_word_wrap(Options), !.

%! dcg_line_wrap//
% @see dcg_line_wrap//1

dcg_line_wrap --> dcg_line_wrap([]).

%! dcg_line_wrap(+Options)//
% Return the parsed codes list with newlines using line wrap.
%
% @arg Options A list of name-value pairs.
%      The following options are supported:
%      * maximum_line_width(+MaximumLineWidth:integer)
%        The maxmim width of a line of characters.
%        This is the length at which line wrapping occurs.

dcg_line_wrap(Options) -->
  {
    setting(maxmimum_line_width, DefaultMaximumLineWidth),
    option(
      maximum_line_width(MaximumLineWidth),
      Options,
      DefaultMaximumLineWidth
    )
  },
  dcg_line_wrap(MaximumLineWidth, MaximumLineWidth).

dcg_line_wrap(0, MaximumLineWidth), newline --> !,
  dcg_line_wrap(MaximumLineWidth, MaximumLineWidth).
dcg_line_wrap(Remaining, MaximumLineWidth), [Code] -->
  [Code],
  {NewRemaining is Remaining - 1}, !,
  dcg_line_wrap(NewRemaining, MaximumLineWidth).
dcg_line_wrap(_Remaining, _MaximumLineWidth) --> [], !.

%! dcg_word_wrap//
% @see dcg_word_wrap//1

dcg_word_wrap --> dcg_word_wrap([]).

%! dcg_word_wrap(+Options)//
% Return the parsed codes list with newlines using word wrap.
%
% @arg Options A list of name-value pairs.
%      The following options are supported:
%      * maximum_line_width(+MaximumLineWidth:integer)
%        The maxmim width of a line of characters.
%        This is the length at which line wrapping occurs.

dcg_word_wrap(Options) -->
  {
    setting(maxmimum_line_width, DefaultMaximumLineWidth),
    option(
      maximum_line_width(MaximumLineWidth),
      Options,
      DefaultMaximumLineWidth
    )
  },
  dcg_word_wrap(MaximumLineWidth, MaximumLineWidth).

dcg_word_wrap(_Remaining, _MaximumLineWidth) --> dcg_end, !.
dcg_word_wrap(Remaining, MaximumLineWidth), (Emit, EmitPostfix) --> !,
  dcg_graphic(Word), (" "  -> {EmitPostfix = " "} ; {EmitPostfix = ""}), !,
  {
    length(Word, WordLength),
    (
      WordLength > MaximumLineWidth
    ->
      split_list_by_size(Word, MaximumLineWidth, SubWords),
      phrase(newline, Newline),
      list_separator_concat(SubWords, Newline, WrappedWord),
      Emit = (newline, WrappedWord),
      last(SubWords, LastSubWord),
      length(LastSubWord, LastSubWordLength),
      NewRemaining is MaximumLineWidth - LastSubWordLength
    ;
      WordLength > Remaining
    ->
      Emit = (newline, Word),
      NewRemaining is MaximumLineWidth - WordLength
    ;
      Emit = Word,
      NewRemaining is Remaining - WordLength
    )
  },
  dcg_word_wrap(NewRemaining, MaximumLineWidth).

