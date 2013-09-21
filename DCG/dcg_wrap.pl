:- module(
  dcg_wrap,
  [
    dcg_wrap//1, % +Options:list(nvpair)
    dcg_line_wrap//1, % +Options:list(nvpair)
    dcg_word_wrap//1 % +Options:list(nvpair)
  ]
).

/** <module> DCG_WRAP

DCG rules for wrapping text.

There are various uses of wrapping text:
  1. Wrap text with newlines and padd with spaces.
     This is used for the speech bubble in cowspeak.
     The newline suffices for terminal output.
     The padding with spaces is needed in order to have
     the `|` appear at the right horizontal position
     in order to form the right hand side of the speech bubble.
  2. Wrap text into separate lists without padding.
     This is useful if a following predicates needs to perform
     arbitrary operations on the splitted lines of text.
     Since the display device may not be a terminal,
     the padded spaces may not be necessary.
  3. Wrap text with linebreak tags, i.e. =|<br/>|=.
     HTML does not display newlines.

All these uses taken care of by this module,
by supporting the following options:
  1. =|padding(+Padding:boolean)|=
     Whether padding using spaces occurs at the right hand side
     of each line.
  2. =|separator(:DCG_Body)|=
     The separation between the wrapped lines.
  3. =|wrap_margin(+WrapMargin:integer)|=
     The maxmim width of a line of characters.
     This is the length at which line wrapping occurs.
  4. =|wrap_mode(line,none,word)|=
     Whether word wrapping or line wrapping is used.

@author Wouter Beek
@version 2013/07, 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(list_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(wrap_margin, integer, 80, 'The default wrap margin.').

% Meta-option `separator`.
:- meta_predicate(dcg_wrap(:,?,?)).
% Meta-option `separator`.
:- meta_predicate(dcg_line_wrap(:,?,?)).
% Meta-argument (DCG rule) `separator`.
:- meta_predicate(dcg_line_wrap(+,//,+,+,?,?)).
% Meta-option `separator`.
:- meta_predicate(dcg_word_wrap(:,?,?)).
% Meta-argument (DCG rule) `separator`.
:- meta_predicate(dcg_word_wrap(+,//,+,+,?,?)).



%! dcg_wrap(+Options)//
% The following options are supported:
%   1. =|padding(+Padding:boolean)|=
%      Whether padding using spaces occurs at the right hand side
%      of each line.
%   2. =|separator(:DCG_Body)|=
%      The separation between the wrapped lines.
%   3. =|wrap_margin(+WrapMargin:integer)|=
%      The maxmim width of a line of characters.
%      This is the length at which line wrapping occurs.
%   4. =|wrap_mode(line,none,word)|=
%      Whether word wrapping or line wrapping is used.

dcg_wrap(O1) -->
  {
    meta_options(is_meta, O1, O2),
    option(wrap_mode(Mode), O2, word)
  },
  (
    {Mode == line}
  ->
    dcg_line_wrap(O2)
  ;
    {Mode == word}
  ->
    dcg_word_wrap(O2)
  ;
    dcg_all
  ).

%! dcg_line_wrap(+Options)//
% Return the parsed codes list with newlines using line wrap.
%
% Line wrapping ends a line after the given number of characters
% has been parsed, or once there are no more characters left.
%
% The following options are supported:
%   1. =|padding(+Padding:boolean)|=
%      Whether padding using spaces occurs at the right hand side
%      of the last line (default `false`).
%   2. =|separator(:DCG_Body)|=
%      The separation between the wrapped lines (default `newline`).
%   3. =|wrap_margin(+WrapMargin:integer)|=
%      The maxmim width of a line of characters (default `80`).
%      This is the length at which line wrapping occurs.
%
% @param Options A list of name-value pairs.

dcg_line_wrap(O1) -->
  {
    meta_options(is_meta, O1, O2),
    option(padding(Padding), O2, false),
    option(separator(Separator), O2, newline),
    setting(wrap_margin, DefaultWrapMargin),
    option(wrap_margin(WrapMargin), O2, DefaultWrapMargin)
  },
  dcg_line_wrap(Padding, Separator, WrapMargin, WrapMargin).

% The number of characters for one line have been parsed,
% so it is time for a separator.
% Also, reset the character counter and start parsing the next line.
dcg_line_wrap(Padding, Separator, 0, WrapMargin), Separator --> !,
  dcg_line_wrap(Padding, Separator, WrapMargin, WrapMargin).
% In the middle of parsing a line: process the next character; up the counter.
dcg_line_wrap(Padding, Separator, Remaining, WrapMargin), [Code] -->
  [Code],
  {NewRemaining is Remaining - 1}, !,
  dcg_line_wrap(Padding, Separator, NewRemaining, WrapMargin).
% The last character was consumed, no space padding.
dcg_line_wrap(false, _Separator, _Remaining, _WrapMargin) --> !, [].
% The last character was consumed, add space padding.
dcg_line_wrap(true, _Separator, Remaining, _WrapMargin), codes(Spaces) --> !,
  % `32` is the US-ASCII character code for space.
  {repeating_list(32, Remaining, Spaces)}.

%! dcg_word_wrap(+Options)//
% Returns the parsed codes list with newlines using word wrap.
%
% Word wrap means that a line split never occurs within a word.
%
% The following options are supported:
%   1. =|padding(+Padding:boolean)|=
%      Whether padding using spaces occurs at the right hand side
%      of the last line (default `false`).
%   2. =|separator(:DCG_Body)|=
%      The separation between the wrapped lines (default `newline`).
%   3. =|wrap_margin(+WrapMargin:positive_integer)|=
%      The wrap margin of a line of characters (default `80`).
%      This is the length at which line wrapping occurs.
%
% @param Options A list of name-value pairs.
% @tbd Use a dictionary and a language tag in order to wrap at
%      word boundaries.

dcg_word_wrap(O1) -->
  {
    meta_options(is_meta, O1, O2),
    option(padding(Padding), O2, false),
    option(separator(Separator), O2, newline),
    setting(wrap_margin, DefaultWrapMargin),
    option(wrap_margin(WrapMargin), O2, DefaultWrapMargin)
  },
  dcg_word_wrap(Padding, Separator, WrapMargin, WrapMargin),
  % Prevent backtracking on codes/1 that appears in the head!
  !.

% No more characters: do not add space padding.
dcg_word_wrap(false, _Separator, _Remaining, _WrapMargin) -->
  dcg_end, !.
% No more characters: add space padding.
dcg_word_wrap(true, _Separator, Remaining, _WrapMargin), codes(Spaces) -->
  dcg_end, !,
  {repeating_list(32, Remaining, Spaces)}.
% Process another character. Notice that there are several options here.
dcg_word_wrap(Padding, Separator, Remaining, WrapMargin),
    codes(Word2), Postfix -->
  % The behavior of word wrapping depends on properties of
  % the upcoming word in the parsed string.
  % We therefore peek at this upcoming string.
  dcg_peek(graphic(Word1)),
  {length(Word1, WordLength)},

  (
    % Case 0: There is no word.
    % This happens in the serializations of languages I do not understand,
    % e.g. Chinese (language tag `zh`).
    % @tbd This is only a quick fix.
    {WordLength == 0}
  ->
    {
      gtrace,
      Word2 = [],
      Postfix = Separator
    }
  ;
    % Case 1: The word is too long to ever occur on a single line.
    % Therefore, we might as well split it now.
    % Insert the word prefix that fits in the current line.
    % Insert a newline directly afterwards (i.e. no space).
    % Consume the placed word prefix, but not the rest of the word (yet).
    {WordLength > WrapMargin}
  ->
    {
      length(Word2, Remaining),
      append(Word2, _Word3, Word1),
      Postfix = Separator,
      NewRemaining = WrapMargin
    },
    codes(Word2)
  ;
    % Case 2: What a nice accident! The word happens to fit exactly
    % into the remaining positions of the current line.
    % Place the word, and insert the split dirrectly after the word.
    % Also, skip any directly following white characters from the stream
    % (since they would otherwise start the next line).
    {WordLength == Remaining}
  ->
    {
      Word2 = Word1,
      Postfix = Separator,
      NewRemaining = WrapMargin
    },
    codes(Word1),
    whites
  ;
    % Case 3: The word is too long to fit on the current line,
    % but it would fit on a new line.
    % Fill the rest of the line with spaces (depending on
    % the `padding` option) and insert the separator after that.
    % Process the entire word later.
    {WordLength > Remaining}
  ->
    {
      (  Padding == true
      -> repeating_list(32, Remaining, Word2)
      ;  true ),
      Postfix = Separator,
      NewRemaining = WrapMargin
    }
  ;
    % Case 4: The 'normal' case.
    % The word fits in the current line, and ont the current line
    % there will be at least one character position left after it.
    % Place the word, and consume it.
    {Word2 = Word1},
    codes(Word1),

    % Whether a space should be inserted after the word, depends on
    % whether such a space occurs in the processed string.
    % This is not always the case, e.g. when the word occurs
    % at the end of the string.
    % We need to do some bookkeeping in order to get this right.
    (  dcg_peek(space)
    -> space,
      {Postfix = space,
       SpaceLength = 1}
    ; {Postfix = void,
       SpaceLength = 0}),
    {NewRemaining is Remaining - WordLength - SpaceLength}
  ),

  % No regrets.
  % Actually, this does not prevent Prolog from backtracking on codes/1
  % in the head!
  !,
  dcg_word_wrap(Padding, Separator, NewRemaining, WrapMargin).

is_meta(separator).

