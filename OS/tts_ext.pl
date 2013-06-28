:- module(
  tts_ext,
  [
    text_to_speech/1 % +Text:atom
  ]
).

/** <module> TTS

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(process)).
:- use_module(os(os_ext)).



%! text_to_speech(+Text:atom) is det.
% Creates a process for a program that can turn text into speech.
%
% This is currently implemented for Unix systems with espeak in the PATH.
%
% @tbd Add support for Windows.
% @tbd Test support on Mac OS-X.

text_to_speech(Text):-
  os_dependent_call(text_to_speech(Text)).

:- if(is_unix).
text_to_speech_unix(Text):-
  process_create(path(espeak), [Text], [detached(true)]).
:- endif.

:- if(is_windows).
text_to_speech_windows(_Text).
:- endif.

