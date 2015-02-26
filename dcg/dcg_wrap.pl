:- module(
  dcg_wrap,
  [
    dcg_wrap//1, % +Options:list(nvpair)
    dcg_line_wrap//1, % +Options:list(nvpair)
    dcg_word_wrap//1 % +Options:list(nvpair)
  ]
).

/** <module> DCG: Line wrapping

Grammar for wrapping lines of text.

There are various uses of wrapping text:
  1. Wrapping text with newlines and padding them with spaces.
     This is e.g. used for the speech bubble in cowspeak.
     The newline suffices for terminal output.
     The padding with spaces is needed in order to have
     the `|` appear at the right horizontal position,
      forming the right hand side of the speech bubble.
  2. Wrapping text into separate lists without padding.
     This is useful if another predicate needs to perform
      arbitrary operations on the splitted lines of text.
     Since the display device may not be a terminal,
      the padding with spaces may not be necessary.
  3. Wrap text with HTML linebreak tags, i.e. `<br/>`,
      since HTML does not display newlines.

@author Wouter Beek
@version 2013/07, 2013/09, 2014/01, 2014/03, 2014/08-2014/11
*/

:- use_module(library(lists), except([delete/3])).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(generics(list_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_peek)).

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

is_meta(separator).

:- predicate_options(dcg_wrap//1, 1, [
     wrap_mode(+oneof([line,none,word])),
     pass_to(dcg_line_wrap//1, 1),
     pass_to(dcg_word_wrap//1, 1)
   ]).
:- predicate_options(dcg_line_wrap//1, 1, [
     padding(+boolean),
     separator(+callable),
     wrap_margin(+positive_integer)
   ]).
:- predicate_options(dcg_word_wrap//1, 1, [
     padding(+boolean),
     separator(+callable),
     wrap_margin(+positive_integer)
   ]).



%! dcg_wrap(+Options:list(nvpair))// is det.
% The following options are supported:
%   1. `wrap_mode(+oneof([line,none,word]))`
%      Whether word wrapping (`word`, default), line wrapping (`line`)
%      or not wrapping at all (`none`) is used.
%   2. Other options are passed to dcg_line_wrap//1 or dcg_word_wrap//1.

dcg_wrap(Options1) -->
  {
    meta_options(is_meta, Options1, Options2),
    select_option(wrap_mode(WrapMode), Options2, Options3, word)
  },
  (   {WrapMode == line}
  ->  dcg_line_wrap(Options3)
  ;   {WrapMode == word}
  ->  dcg_word_wrap(Options3)
  ;   dcg_all
  ).


%! dcg_line_wrap(+Options:list(nvpair))// is det.
% Emits the parsed codes list with interspersed separators using line wrap.
%
% Line wrapping ends a line after the given number of characters
%  has been parsed, or once there are no more characters left.
%
% The following options are supported:
%   1. `padding(+boolean)`
%      Whether padding occurs at the right hand side of the last line
%      Spaces are used for padding.
%      Default: `false`.
%   2. `separator(:Dcg)`
%      The separator that is emitted between the wrapped lines
%      Default: `newline`.
%   3. `wrap_margin(+positive_integer)`
%      The maxmim width of a line of characters (default `80`).
%      This is the length at which line wrapping occurs.

dcg_line_wrap(Options1) -->
  {
    meta_options(is_meta, Options1, Options2),
    option(padding(Padding), Options2, false),
    option(separator(Separator), Options2, line_feed),
    setting(wrap_margin, DefaultWrapMargin),
    option(wrap_margin(WrapMargin), Options2, DefaultWrapMargin)
  },
  dcg_line_wrap(Padding, Separator, WrapMargin, WrapMargin).

% The last character was consumed and no space padding occurs (option).
dcg_line_wrap(false, _Separator, _Remaining, _WrapMargin) --> !, dcg_end.
% The last character was consumed add space padding occurs (option).
dcg_line_wrap(true, _Separator, Remaining, _WrapMargin),
    '#'(Remaining, space, []) --> !, dcg_end.
% The number of characters for one line have been parsed,
%  so it is time for a separator.
% Also, reset the character count and start parsing the next line.
dcg_line_wrap(Padding, Separator, 0, WrapMargin), Separator --> !,
  dcg_line_wrap(Padding, Separator, WrapMargin, WrapMargin).
% In the midst of parsing a line.
% Process the next character and decrease the counter.
dcg_line_wrap(Padding, Separator, Remaining1, WrapMargin), [Code] -->
  [Code],
  {Remaining2 is Remaining1 - 1}, !,
  dcg_line_wrap(Padding, Separator, Remaining2, WrapMargin).


%! dcg_word_wrap(+Options:list(nvpair))// is det.
% Returns the parsed codes list with newlines using word wrap.
%
% Word wrap means that a line split never occurs within a word.
%
% The following options are supported:
%   1. `padding(+boolean)`
%      Whether padding using spaces occurs at the right hand side
%      of the last line (default `false`).
%   2. `separator(:Dcg)`
%      The separation between the wrapped lines.
%      Default: `newline`.
%   3. `wrap_margin(+positive_integer)`
%      The wrap margin of a line of characters (default `80`).
%      This is the length at which line wrapping occurs.
%
% @tbd Use a natural language dictionary and a language tag
%      in order to wrap at word boundaries.

dcg_word_wrap(Options1) -->
  {
    meta_options(is_meta, Options1, Options2),
    option(padding(Padding), Options2, false),
    option(separator(Separator), Options2, line_feed),
    setting(wrap_margin, DefaultWrapMargin),
    option(wrap_margin(WrapMargin), Options2, DefaultWrapMargin)
  },
  dcg_word_wrap(Padding, Separator, WrapMargin, WrapMargin),
  % Prevent backtracking on codes/1 that appears in the head!
  !.

% No more characters and do not add space padding (option).
dcg_word_wrap(false, _Separator, _Remaining, _WrapMargin) --> dcg_end, !.
% No more characters and add space padding (option).
dcg_word_wrap(true, _Separator, Remaining, _WrapMargin),
    '#'(Remaining, space, []) --> dcg_end, !.
% Process another character. Notice that there are several options here.
dcg_word_wrap(Padding, Separator, Remaining, WrapMargin),
    '*'(code, Word2, []),
    Postfix -->
  % The behavior of word wrapping depends on properties of
  %  the upcoming word in the parsed string.
  % We therefore peek at this upcoming string.
  dcg_peek(graphic(Word1)),
  {length(Word1, WordLength)},

  (   % Case 1: The word is too long to ever occur on a single line.
      % Therefore, we might as well split it now.
      % Insert the word prefix that fits in the current line.
      % Insert a newline directly afterwards (i.e. no space).
      % Consume the placed word prefix, but not the rest of the word (yet).
      {WordLength > WrapMargin}
  ->  {
        length(Word2, Remaining),
        append(Word2, _, Word1),
        Postfix = Separator,
        NewRemaining = WrapMargin
      },
      '*'(code, Word2, [])
  ;   % Case 2: What a nice accident! The word happens to fit exactly
      % into the remaining positions of the current line.
      % Place the word, and insert the split dirrectly after the word.
      % Also, skip any directly following white characters from the stream
      % (since they would otherwise start the next line).
      {WordLength == Remaining}
  ->  {
        Word2 = Word1,
        Postfix = Separator,
        NewRemaining = WrapMargin
      },
      '*'(code, Word1, []),
      whites
  ;   % Case 3: The word is too long to fit on the current line,
      % but it would fit on a new line.
      % Fill the rest of the line with spaces (depending on
      % the `padding` option) and insert the separator after that.
      % Process the entire word later.
      {WordLength > Remaining}
  ->  {
        (   Padding == true
        ->  repeating_list(32, Remaining, Word2)
        ;   Word2 = []
        ),
        Postfix = Separator,
        NewRemaining = WrapMargin
      }
  ;   % Case 4: The 'normal' case.
      % The word fits in the current line, and ont the current line
      % there will be at least one character position left after it.
      % Place the word, and consume it.
      {Word2 = Word1},
      '*'(code, Word1, []),

      % Whether a space should be inserted after the word, depends on
      % whether such a space occurs in the processed string.
      % This is not always the case, e.g. when the word occurs
      % at the end of the string.
      % We need to do some bookkeeping in order to get this right.
      (   dcg_peek(space)
      ->  " ",
          {
            Postfix = space,
            SpaceLength = 1
          }
      ;   {
            Postfix = void,
            SpaceLength = 0
          }
      ),
      {NewRemaining is Remaining - WordLength - SpaceLength}
  ),

  % No regrets.
  % Actually, this does not prevent Prolog from backtracking
  % on `'*'(code, Codes, [])` in the head!
  % The calling context must enforce determinism.
  !,
  dcg_word_wrap(Padding, Separator, NewRemaining, WrapMargin).
