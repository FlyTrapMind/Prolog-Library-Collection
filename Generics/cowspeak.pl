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
:- use_module(dcg(dcg_wrap)).
:- use_module(generics(atom_ext)).
:- use_module(generics(codes_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(apply)).
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
  40,
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
% The following options are supported:
%   * `eyes(+Eyes:list(code))`
%   * `maximum_width(?MaximumWidth:integer)`
%     The maximum number of characters the speech bubble is allowed to have.
%     If the maximum width is exceeded by any content line, then the
%     wrap option -- if set -- is used.
%   * `mode(+Mode:oneof(['Borg',dead,greedy,paranoia,stoned,tired,wired,youth]))`
%     The following process_modes are supported: `Borg`, `dead`, `greedy`,
%     `paranoia`, `stoned`, `tired`, `wired`, `youth`.
%   * `output(+Output)`
%     The same output alternatives that apply to with_output_to/2.
%     The default value is =|stream(user_output)|=.
%   * `speech(+OnOrOff:boolean)`
%   * `wrap(+Wrap:oneof([line,none,word]))`
%     Whether line wrapping or word wrapping (the default)
%     should be applied, or neither of those (e.g. for ASCII art).
%
% @param Text Either an atomic text message or a list of atomic lines
%      constituting a message.
% @param Options A list of name-value pairs.
%
% @tbd Split lines by words (in whitespace). Add this to module ATOM_EXT.
% @tbd When tabs are used in cowspeak/2 the width of the speech balloon
%      cannot be reliable ascertained right now.

cowspeak(O, Contents):-
  is_list(Contents), !,
  maplist(cow_atom, Contents, Atoms),
  cowspeak_(O, Atoms).
% Since we work with lists, we create a singleton list for single terms.
cowspeak(O, Content):-
  cowspeak(O, [Content]).

cowspeak_(O1, Atoms):-
  % Establish the maximum width of the speech bubble.
  setting(default_max_width, DefaultMaxWidth),
  option(maximum_width(MaximumWidth), O1, DefaultMaxWidth),
  % Some characters are needed to display the speech bubble itself.
  MaximumEffectiveWidth is MaximumWidth - 4,
  merge_options([maximum_line_width(MaximumEffectiveWidth)], O1, O2),
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
      phrase(dcg_wrap(O2), CodeLine1, CodeLine2),
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
  phrase(dcg_cowsay(O2, LineWidth, CodeLines2), CowCodes),

  % Write to the given stream.
  option(output(Output), O2, user_output),
  atom_codes(CowAtom, CowCodes),
  with_output_to(Output, write(CowAtom)),

  % It can talk!
  option(speech(Speech), O2, true),
  if_then(
    Speech == true,
    text_to_speech(Atoms)
  ).

dcg_cow(O1) -->
  {
    process_modes(O1, O2),
    Indent = 8,
    AddToIndent = 4,
    AddedIndent is Indent + AddToIndent,
    CowLength = 4
  },

  % First line.
  dcg_multi(space, Indent), backslash, "   ^__^", newline,

  % Second line.
  dcg_multi(space, Indent), space, backslash,
  "  (", dcg_cow_eyes(O2), ")",
  backslash, "___", dcg_multi(underscore, CowLength), newline,

  % Third line.
  dcg_multi(space, AddedIndent),
  "(__)", backslash, "   ", dcg_multi(space, CowLength), ")",
  dcg_cow_tail, newline,

  % Fourth line.
  dcg_multi(space, AddedIndent),
  " ", dcg_cow_tongue(O2),
  " ||", dcg_multi(hyphen, CowLength), "w |", newline,

  % Fifth line.
  dcg_multi(space, AddedIndent),
  "    ", "||", dcg_multi(space, CowLength), " ||", newline.

dcg_cow_eyes(O) -->
  {
    option(eyes(Eyes1), O, "oo"),
    to_codes(Eyes1, Eyes2),
    Eyes2 = [X, Y | _], !
  },
  [X, Y].
dcg_cow_eyes(_O) --> "oo".

dcg_cow_tail -->
  backslash, forward_slash, backslash.

dcg_cow_tongue(O) -->
  {
    option(tongue(Tongue1), O, "  "),
    to_codes(Tongue1, Tongue2),
    Tongue2 = [X, Y | _], !
  },
  [X, Y].
dcg_cow_tongue(_O) --> "  ".

dcg_cowsay(O, LineWidth, CodeLines) -->
  newline,
  dcg_speech_bubble(LineWidth, CodeLines),
  dcg_cow(O),
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

dcg_speech_bubble_top(LineWidth) -->
  forward_slash, hyphen,
  dcg_multi(hyphen, LineWidth),
  hyphen, backslash.

%! cowspeak_web(+Content, -Markup:list) is det.
% @see Like cowspeak/1, but returns the result in markup.

cowspeak_web(Content, Markup):-
  cowspeak_web([], Content, Markup).

%! cowspeak_web(+Options, +Content, -Markup:list) is det.
% @see Like cowspeak/2, but returns the result in markup.

cowspeak_web(
  O1,
  Content,
  [element(title, [], ['Cow says'])]/[element(pre, [], [Atom])]
):-
  select_option(output(_Output), O1, O2),
  merge_options([output(atom(Atom))], O2, O3),
  cowspeak(O3, Content).

mode('Borg', [eyes("==")]).
mode(dead, [eyes("XX"), tongue("U")]).
mode(greedy, [eyes("$$")]).
mode(paranoia, [eyes("@@")]).
mode(stoned, [eyes("**"), tongue("U")]).
mode(tired, [eyes("--")]).
mode(wired, [eyes("OO")]).
mode(youth, [eyes("..")]).

process_modes(O1, O2):-
  option(mode(Mode), O1),
  mode(Mode, ModeO), !,
  merge_options(ModeO, O1, O2).
process_modes(O, O).

