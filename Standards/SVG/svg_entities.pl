:- module(
  svg_entities,
  [
    svg_rectangle//3 % -Tree:compound
                     % :DCG_Namespace
                     % ?Attributes:list(compound)
  ]
).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(plunit)).
:- use_module(xml(xml_dcg)).

:- meta_predicate(svg_rectangle(-,//,?,?,?)).



%! svg_rectangle(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The following attributes are supported:
%   1. =|height(Amount:float,Unit:oneof([cm]))|=
%   2. =|fill(?Fill:oneof([none]))|=
%   3. =|stroke(?Color:oneof([blue]))|=
%   4. =|stroke_width(?Amount:float,?Unit:oneof([cm]))|=
%   5. =|width(?Amount:float,?Unit:oneof([cm]))|=
%   6. =|x(?Amount:float,?Unit:oneof([cm]))|=
%   7. =|y(?Amount:float,?Unit:oneof([cm]))|=

svg_rectangle(Tree, DCG_Namespace, Attrs1) -->
  {xml_inject_attributes(DCG_Namespace, Attrs1, Attrs2, Trees)},
  xml_entity(DCG_Namespace, word(rect), Attrs2),
  {parse_tree(rect, Trees, Tree)}.



% PLUNIT %

:- begin_tests(svg_entities).

:- use_module(dcg(dcg_content)).
:- use_module(generics(print_ext)).
:- use_module(gv(gv_file)).

test(svg_rectangle, []):-
  once(phrase(svg_rectangle(Tree, void, [x(0.5,cm),y(1.5,cm)]), Codes)),
  atom_codes(Atom, Codes),
  formatnl(Atom),
  convert_tree_to_gv([], Tree, dot, pdf, File),
  formatnl(File).

:- end_tests(svg_entities).

