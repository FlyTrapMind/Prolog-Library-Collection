:- module(
  markup,
  [
    integer_sequence/2 % +Sequence:list(integer)
                       % -Markup:compound
  ]
).

:- use_module(standards(css), [attribute_value/3 as css_attribute_value]).

%! integer_sequence(+Sequence:list(integer), -Markup:compound) is det.

integer_sequence([], []):- !.
integer_sequence([H | T], [element(span, [style=Style], [H1]) | Markup]):-
  FontSize is 100 + 10 * H,
  atom_number(H1, H),
  atomic_list_concat([FontSize,'%'], FontSize_pct),
  css_attribute_value('font-size', FontSize_pct, Style),
  integer_sequence(T, Markup).

