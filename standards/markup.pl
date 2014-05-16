:- module(
  markup,
  [
    format_number/3, % +Unit:unit
                     % +Number:float
                     % -Atom:atom
    integer_sequence/2, % +Sequence:list(integer)
                        % -Markup:compound
    sonnet/2 % +Sentences:list(atom)
             % -Markup:compound
  ]
).

/** <module> Markup

@author Wouter Beek
@version 2012/10, 2013/07
*/

:- use_module(standards(css), [attribute_value/3 as css_attribute_value]).



%! format_number(+Unit:unit, +Number:float, -FormattedNumber:atom) is det.
% Formats a number according to a certain unit scale.
%
% @arg Unit An atomic unit descriptor.
% @arg Number Any number (e.g., integer, float).
% @arg FormattedNumber The atomic result of formatting the number.
%
% @tbd Make sure the values for unit are registered with
%      specific markup languages.

format_number(Unit, Number, FormattedNumber):-
  atom_number(Atom, Number),
  atomic_concat(Atom, Unit, FormattedNumber).

%! integer_sequence(+Sequence:list(integer), -Markup:compound) is det.

integer_sequence([], []):- !.
integer_sequence([H | T], [element(span, [style=Style], [H1]) | Markup]):-
  FontSize is 100 + 10 * H,
  atom_number(H1, H),
  format_number('%', FontSize, FontSize_pct),
  css_attribute_value('font-size', FontSize_pct, Style),
  integer_sequence(T, Markup).

