:- module(
  dcg_wrap,
  [
    dcg_wrap//0,
    dcg_wrap//1, % +Options:list(nvpair)
    dcg_line_wrap//0,
    dcg_line_wrap//1, % +Options:list(nvpair)
    dcg_word_wrap//0,
    dcg_word_wrap//1 % +Options:list(nvpair)
  ]
).

/** <module> DCG_WRAP

DCG rules for wrapping text.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(list_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(
  maxmimum_line_width,
  integer,
  80,
  'The default maximum line width, after which line wrapping occurs.'
).



%! dcg_wrap//
% @see dcg_wrap//1

dcg_wrap -->
  dcg_wrap([]).

%! dcg_wrap(+Options)//

dcg_wrap(Options) -->
  {select_option(wrap(Mode), Options, RestOptions, word)},
  dcg_wrap_(Mode, RestOptions).
dcg_wrap_(line, Options) -->
  dcg_line_wrap(Options), !.
dcg_wrap_(none, _Options) -->
  dcg_all, !.
dcg_wrap_(word, Options) -->
  dcg_word_wrap(Options), !.

%! dcg_line_wrap//
% @see dcg_line_wrap//1

dcg_line_wrap -->
  dcg_line_wrap([]).

%! dcg_line_wrap(+Options)//
% Return the parsed codes list with newlines using line wrap.
%
% @arg Options A list of name-value pairs.
%      The following options are supported:
%      * `maximum_line_width(+MaximumLineWidth:integer)`
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

dcg_word_wrap -->
  dcg_word_wrap([]).

%! dcg_word_wrap(+Options)//
% Return the parsed codes list with newlines using word wrap.
%
% @arg Options A list of name-value pairs.
%      The following options are supported:
%      * `maximum_line_width(+MaximumLineWidth:integer)`
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

dcg_word_wrap(_Remaining, _MaximumLineWidth) -->
  dcg_end, !.
dcg_word_wrap(Remaining, MaximumLineWidth), (Emit, EmitPostfix) --> !,
  dcg_graphic(Word),
  % Whether or now a space follows the word. Specifically, the last word
  % in the codes list need not be followed by a space.
  (
    " "
  ->
    {EmitPostfix = " "}
  ;
    {EmitPostfix = ""}
  ),
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
