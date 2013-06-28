:- module(
  term_ext,
  [
    ansi_format/4 % +Stream:stream
                  % +Attributes
                  % +Format:atom
                  % +Args:list
  ]
).

/** <module> TERM_EXT

Terminal extensions.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(ansi_term)).



%! ansi_format(+Stream:stream, +Attributes, +Format:atom, +Args:list) is det.
% Like the swipl builtin ansi_format/3, but allows writing to an arbitrary
% output stream.
%
% @tbd Test and extend.

ansi_format(Stream, Attributes, Format, Args):-
  setup_call_cleanup(
    (
      current_output(MainStream),
      set_output(Stream)
    ),
    ansi_format(Attributes, Format, Args),
    (
      flush_output(Stream),
      set_output(MainStream)
    )
  ).

