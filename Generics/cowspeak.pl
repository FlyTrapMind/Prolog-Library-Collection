:- module(
  cowspeak,
  [
    cowspeak/1, % +Content
    cowspeak/2, % +Options
                % +Content
    cowspeak_web/2, % +Content
                    % -Markup
    cowspeak_web/3 % +Options
                   % +Content
                   % -Markup
  ]
).

/** <module> Cowspeak

A funny cow for communicating with the user.

Based on the old cowsay by Tony Monroe,
in combination with the open source speech synthesizer eSpeak.

@author Wouter Beek
@see http://en.wikipedia.org/wiki/Cowsay pointers to cowsay resources.
@see http://espeak.sourceforge.net/ home of eSpeak.
@version 2012/09-2012/10, 2013/05-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(atom_ext)).
:- use_module(generics(list_ext)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(os(os_ext)).
:- use_module(os(tts_ext)).

:- debug(cowspeak).

% The automated finding of meta-predicates seems to be over-eager.
:- meta_predicate(dcg_speech_bubble_line(+,+,?,?)).

:- setting(
  default_max_width,
  integer,
  50,
  'The default width of the speech bubble in the number characters.'
).



cow_atom(Format-Arguments, Atom):- !,
  format(atom(Atom), Format, Arguments).
cow_atom(Atom, Atom):-
  atom(Atom), !.
cow_atom(Term, Atom):-
  term_to_atom(Term, Atom).

cowspeak(Content):-
  cowspeak([], Content).

%! cowspeak(+Options, +TermOrTerms) is det.
% Turns the given text into a cowified message, displaying the given
% text in the cow's speech bubble.
%
% Meet the cow:
% ~~~{.txt}
%    ^__^
%    (oo)|_____
%    (__)|     )/|/
%      ||----w |
%     ||       ||
% ~~~
%
% @arg Text Either an atomic text message or a list of atomic lines
%      constituting a message.
% @arg Options A list of name-value pairs.
%      The following options are supported:
%      * maximum_width(?MaximumWidth:integer)
%        The maximum number of characters the speech bubble is allowed to
%        have.
%        If the maximum width is exceeded by any content line, then the
%        wrap option -- if set -- is used.
%     * output(+Output)
%       The same output alternatives that apply to with_output_to/1.
%       The default value is =|stream(user_output)|=.
%     * speech(+OnOrOff:boolean)
%     * wrap(+Wrap:oneof([line,word]))
%       Whether line wrapping or word wrapping (the default) should be applied.
%
% @tbd Split lines by words (in whitespace). Add this to module ATOM_EXT.
% @tbd When tabs are used in cowspeak/2 the width of the speech balloon
%      cannot be reliable ascertained right now.

cowspeak(Options, Contents):-
  is_list(Contents), !,
  maplist(cow_atom, Contents, Atoms),
  cowspeak_(Options, Atoms).
% Since we work with lists, we create a singleton list for single terms.
cowspeak(Options, Content):-
  cowspeak(Options, [Content]).

cowspeak_(Options, Atoms):-
  % Establish the maximum width of the speech bubble.
  setting(default_max_width, DefaultMaxWidth),
  option(maximum_width(MaximumWidth), Options, DefaultMaxWidth),
  % Some characters are needed to display the speech bubble itself.
  MaximumEffectiveWidth is MaximumWidth - 4,

  findall(
    CodeLine3,
    (
      member(Atom, Atoms),
      % Single atoms may contain newlines.
      split_atom_exclusive('\n', Atom, Lines1),
      % Now were are taling about individual lines proper.
      member(Line1, Lines1),
      % Some lines may exceed the maximum allowed width.
      % These lines are further split into lines.
      % The way in which this is done depends on
      % the type of wrapping that is used.
      atom_codes(Line1, CodeLine1),
      % Word wrapping.
      phrase(
        dcg_word_wrap([maximum_line_width(MaximumEffectiveWidth)]),
        CodeLine1,
        CodeLine2
      ),
      % We need a list for each line in order to determine
      % the speech bubble width.
      phrase(dcg_separated_list(newline, CodeLines1), CodeLine2),
      member(CodeLine3, CodeLines1)
    ),
    CodeLines2
  ),

  % Establish the width of the speech bubble.
  maplist(length, CodeLines2, LineLengths),
  max_list(LineLengths, LineWidth),

  % Cow DCG.
  phrase(dcg_cowsay(LineWidth, CodeLines2), CowCodes),

  % It can talk!
  option(speech(Speech), Options, false),
  if_then(
    Speech == true,
    text_to_speech(Atoms)
  ),

  % Write to the given stream.
  option(output(Output), Options, user_output),
  atom_codes(CowAtom, CowCodes),
  with_output_to(Output, write(CowAtom)).

dcg_cow -->
  " |  ^__^", newline,
  "  - (oo)______", newline,
  "    (__)      )/|/", newline,
  "      ||----w||", newline,
  "     ||       ||", newline.

dcg_cowsay(LineWidth, CodeLines) -->
  newline,
  dcg_speech_bubble(LineWidth, CodeLines),
  dcg_multi(dcg_speech_bubble_stick, 3),
  dcg_cow,
  newline.

%! dcg_speech_bubble(+LineWidth:integer, +CodeLines:list(list(code)))//
% Draws a speech bubble with the given content,
% and whose content lines have the given length.

dcg_speech_bubble(LineWidth, CodeLines) -->
  dcg_speech_bubble_top(LineWidth), newline,
  dcg_speech_bubble_lines(LineWidth, CodeLines),
  dcg_speech_bubble_bottom(LineWidth), newline.

dcg_speech_bubble_bottom(LineWidth) -->
  backslash, hyphen,
  dcg_multi(hyphen, LineWidth),
  hyphen_minus, forward_slash.

dcg_speech_bubble_line(LineWidth, CodeLine) -->
  vertical_bar, space,
  CodeLine,
  {
    length(CodeLine, ContentLength),
    NumberOfSpaces is LineWidth - ContentLength
  },
  dcg_multi(space, NumberOfSpaces),
  space, vertical_bar,
  newline.

dcg_speech_bubble_lines(_LineWidth, []) --> !, [].
dcg_speech_bubble_lines(LineWidth, [CodeLine|CodeLines]) -->
  dcg_speech_bubble_line(LineWidth, CodeLine),
  dcg_speech_bubble_lines(LineWidth, CodeLines).

dcg_speech_bubble_stick -->
  space, vertical_bar, newline.

dcg_speech_bubble_top(LineWidth) -->
  forward_slash, hyphen,
  dcg_multi(hyphen, LineWidth),
  hyphen, backslash.

%! cowspeak_web(+Content, -Markup:list) is det.

cowspeak_web(Content, Markup):-
  cowspeak_web([], Content, Markup).

cowspeak_web(
  Options1,
  Content,
  [element(title, [], ['Cow says'])]/[element(pre, [], [Atom])]
):-
  select_option(output(_Output), Options1, Options2),
  merge_options([output(atom(Atom))], Options2, Options3),
  cowspeak(Options3, Content).

