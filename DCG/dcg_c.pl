:- module(
  dcg_c,
  [c_convert//0]
).

/** <module> DCG_C

Conversions to make strings compatible with C syntax.

@author Wouter Beek
@version 2013/02, 2013/06
*/

:- use_module(dcg(dcg_ascii)).



% Replace the bell character with '\b'.
c_convert, bell -->
  backslash, b_lowercase, !,
  c_convert.
% Replace the line feed character with '\n'.
c_convert, line_feed -->
  backslash, n_lowercase, !,
  c_convert.
% Replace the horizontal tab character with '\t'.
c_convert, horizontal_tab -->
  backslash, t_lowercase, !,
  c_convert.
% Other characters do not need to be replaced.
c_convert, [X] -->
  [X],!,
  c_convert.
c_convert --> [].
