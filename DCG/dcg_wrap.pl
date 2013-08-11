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
:- use_module(dcg(dcg_content)).
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
% The following options are supported:
%   1. `maximum_line_width(+MaximumLineWidth:integer)`
%      The maxmim width of a line of characters.
%      This is the length at which line wrapping occurs.
%
% @param Options A list of name-value pairs.

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
% The following options are supported:
%   1. `maximum_line_width(+MaximumLineWidth:integer)`
%      The maxmim width of a line of characters.
%      This is the length at which line wrapping occurs.
%
% @param Options A list of name-value pairs.

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

dcg_word_wrap(Remaining, _MaximumLineWidth), dcg_codes(Spaces) -->
  dcg_end, !,
  {repeating_list(32, Remaining, Spaces)}.

dcg_word_wrap(Remaining, MaximumLineWidth), dcg_codes(Word2), Postfix -->
  dcg_peek(graphic(Word1)),
  {length(Word1, WordLength)},

  (
    % Split occurs within word.
    % Insert part of the word and consume this part.
    % Insert a newline, no space.
    % Consume the rest of the word later.
    {WordLength > MaximumLineWidth}
  ->
    {
      length(Word2, Remaining),
      append(Word2, _Word3, Word1),
      Postfix = newline,
      NewRemaining = MaximumLineWidth
    },
    dcg_codes(Word2)
  ;
    % Split occurs right after word.
    % Insert word and insert newline right after it, no space.
    {WordLength == Remaining}
  ->
    {
      Word2 = Word1,
      Postfix = newline,
      NewRemaining = MaximumLineWidth
    },
    dcg_codes(Word1)
  ;
    % Fill the rest of the line with spaces and insert a newline
    % Process the word later.
    {WordLength > Remaining}
  ->
    {
      repeating_list(32, Remaining, Word2),
      Postfix = newline,
      NewRemaining = MaximumLineWidth
    }
  ;
    {Word2 = Word1},
    (
      dcg_peek(space)
    ->
      space,
      {Postfix = space, SpaceLength = 1}
    ;
      {Postfix = dcg_void, SpaceLength = 0}
    ),
    {NewRemaining is Remaining - WordLength - SpaceLength},
    dcg_codes(Word1)
  ),

  dcg_word_wrap(NewRemaining, MaximumLineWidth).

