:- module(
  dcg_c,
  [
    c_convert//0,
    c_name//0
  ]
).

/** <module> DCG rules for the C programming language.

@author Wouter Beek
@version 2013/02, 2013/06, 2014/01-2014/02
*/

:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_meta)).
:- use_module(plDcg(dcg_replace)).



%! c_convert// is det.
% Replace the bell character for `\b`.
% Replace the line feed character for `\n`.
% Replace the horizontal tab character for `\t`.
%
% ### Example
%
% ```prolog
% ?- use_module(generics(code_ext)).
% ?- phrase(c_convert, `aaa\bbbb\nccc\tddd`, X), put_codes(current_output, X).
% aaabbb
% cccddd
% ```

c_convert -->
  dcg_maplist(dcg_replace, [`\b`,`\n`,`\t`], [bell,line_feed,horizontal_tab]).


%! c_name// is nondet.
% ### Example
%
% ```prolog
% ?- once(phrase(c_name, `appe- lenSappP$`, CName)).
% CName = "appe__lensappp_" .
% ```

c_name -->
  dcg_end.
c_name, [C] -->
  letter_lowercase(C),
  c_name.
c_name, [C] -->
  decimal_digit(C),
  c_name.
c_name, [C2] -->
  letter_uppercase(C1),
  {to_lower(C1, C2)},
  c_name.
c_name, "_" -->
  [_],
  c_name.

