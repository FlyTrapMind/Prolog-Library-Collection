:- module(
  svg_dcg,
  [
    svg_document//3, % -Tree:compound
                     % :DCG_Namespace
                     % ?SVG_DCGs:list(dcg)
    svg_fragment_base//2, % -Tree:compound
                          % +SVG_DCGs:list(dcg)
    svg_fragment_nonbase//2, % -Tree:compound
                             % +SVG_DCGs:list(dcg)
    svg_header//3, % -Tree:compound
                   % :DCG_Namespace
                   % ?Attributes:list(compound)
    svg_rect//3 % -Tree:compound
                % :DCG_Namespace
                % ?Attributes:list(compound)
  ]
).

/** <module> SVG_DCG

DCG rules implementing the SVG 1.1 Second Edition standard.

SVG is a language for describing two-dimensional graphics in XML.

SVG allows for three types of graphic objects:
  1. Vector graphic shapes
  2. Images
  3. Text

The use of DTDs for validating XML documents is known to be problematic.
In particular, DTDs do not handle namespaces gracefully.
It is *not* recommended that a DOCTYPE declaration be included
in SVG documents.

# Using SVG in Web pages

SVG can be included in Web pages in the following ways:
  1. Stand-alone SVG Web page.
  2. Embed by reference:
    1. Element `img`
    2. Element `object`, allowing different formats to be given
       through nesting.
    3, Element `applet`.
  3. Embed inline.
  4. External link, using element `a`.
  5. Referenced from a CSS or XSL property (e.g., `background-image`,
     `list-style-image`).

# Graphics rendering

**Grouped elements** are rendered in the following steps:
  1. A temporary separate canvas is initialized in transparent black
     onto which child elements are painted.
  2. Filter effects are applied after painting.
  3. The temporary canvas is composited into the background.
     Clipping, masking, and opacity are taken into account.

Individual graphics elements are rendered as if they are singleton groups.

The fundamental graphics elements types:
  1. **Shapes**, lines and curves.
  2. **Text**, character glyphs.
  3. **Raster images**, array of values (paint color and opacity).

Shapes and text can be filled and/or stroked (along the outline,
after filling).

Shapes can contain  **marker symbols** at selected vertices (after stroking).

Raster images have their original sample resampled to the output device.

@author Wouter Beek
@see SVG 1.1 (Second Edition) http://www.w3.org/TR/2011/REC-SVG11-20110816/
@version 2013/07
*/

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(generics(db_ext)).
:- use_module(library(plunit)).
:- use_module(rfc(rfc_2396)).
:- use_module(svg(svg_attributes)).
:- use_module(svg(svg_colors)).
:- use_module(xml(xml_dcg)).

:- dynamic(user:mime_type/2).
:- dynamic(user:public_identifier/2).
:- dynamic(user:system_identifier/2).

:- meta_predicate(svg_document(//,?,?)).
:- meta_predicate(svg_fragment_base(-,//,?,?)).
:- meta_predicate(svg_fragment_nonbase(-,//,?,?)).
:- meta_predicate(svg_rect(-,//,?,?,?)).

:- db_add_novel(user:prolog_file_type(svgz, gzip)).
:- db_add_novel(user:prolog_file_type(svgz, svg)).
:- db_add_novel(user:mime_type(svg, 'image/svg+xml')).
:- db_add_novel(
  user:public_identifier(svg, 'PUBLIC "-//W3C//DTD SVG 1.1//EN"')).
:- db_add_novel(
  user:system_identifier(
    svg,
    'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'
  )
).

% A special file extension is used on Macintosh HFS file systems.
:- if(is_mac).
:- db_add_novel(user:prolog_file_type('svg ', svg)).
:- endif.
:- if((is_unix ; is_windows)).
:- db_add_novel(user:prolog_file_type(svg, svg)).
:- endif.



%! svg_document(-Tree:compound, ?SVG_DCGs:list(dcg))//

svg_document(T0, DCG_Namespace, SVG_DCGs) -->
  xml_header(T1, DCG_Namespace, [version(1,0),standalone(true)]),
  xml_entities(SVG_DCGs, Ts, DCG_Namespace),
  {parse_tree(document, [T1|Ts], T0)}.

%! svg_fragment_base(-Tree:compound, +SVG_DCGs:list(dcg))//
% An `xmlns` attribute without a namespace prefix could be specified on an
% `svg` element, which means that SVG is the default namespace for all
% elements within the scope of the element with the `xmlns` attribute.
%
% ~~~{.xml}
% <svg xmlns="http://www.w3.org/2000/svg" …>
%   <rect …/>
% </svg>
% ~~~

svg_fragment_base(T0, SVG_DCGs) -->
  xml_entity(word(svg), svg_xmlns_base(T1, void)),
  xml_entities(SVG_DCGs, Ts, void),
  {parse_tree(fragment, [T1|Ts], T0)}.

%! svg_fragment_nonbase(-Tree:compound, +SVG_DCGs:list(dcg))//
% If a namespace prefix is specified on the `xmlns` attribute
% (e.g., =|xmlns:svg="http://www.w3.org/2000/svg"|=),
% then the corresponding namespace is not the default namespace,
% so an explicit namespace prefix must be assigned to the elements.
%
% ~~~{.xml}
% <svg:svg xmlns:svg="http://www.w3.org/2000/svg" …>
%   <svg:rect …/>
% </svg:svg>
% ~~~

svg_fragment_nonbase(T0, SVG_DCGs) -->
  xml_entity(word(svg), word(svg), svg_xmlns_nonbase(T1, word(xmlns))),
  xml_entities(SVG_DCGs, Ts, word(svg)),
  {parse_tree(fragment, [T1|Ts], T0)}.

%! svg_header(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The following attrobutes are supported:
%   1. =|standalone(?Alone:boolean)|=
%   2. =|version(?Major:integer,?Minor:integer)|=

svg_header(Tree, DCG_Namespace, Attrs1) -->
  {xml_inject_attributes(DCG_Namespace, Attrs1, Attrs2, Trees)},
  xml_entity(DCG_Namespace, word(svg), Attrs2),
  {parse_tree(svg, Trees, Tree)}.

%! svg_rect(-Tree:compound, :DCG_Namespace, ?Attributes:list)//
% The following attributes are supported:
%   1. =|height(Amount:float,Unit:oneof([cm]))|=
%   2. =|fill(?Fill:oneof([none]))|=
%   3. =|stroke(?Color:oneof([blue]))|=
%   4. =|stroke_width(?Amount:float,?Unit:oneof([cm]))|=
%   5. =|width(?Amount:float,?Unit:oneof([cm]))|=
%   6. =|x(?Amount:float,?Unit:oneof([cm]))|=
%   7. =|y(?Amount:float,?Unit:oneof([cm]))|=

svg_rect(Tree, DCG_Namespace, Attrs1) -->
  {xml_inject_attributes(DCG_Namespace, Attrs1, Attrs2, Trees)},
  xml_entity(DCG_Namespace, word(rect), Attrs2),
  {parse_tree(rect, Trees, Tree)}.

svg_xmlns_base(xmlns(T1), _DCG_Namespace) -->
  xml_attribute(
    void,
    word(xmlns),
    uri_reference(
      T1,
      http,
      authority(_User,[www,w3,org],_Port),
      [['2000'],[svg]],
      _Query,
      _Fragment
    )
  ).

svg_xmlns_nonbase(xmlns(T1), DCG_Namespace) -->
  xml_attribute(
    DCG_Namespace,
    word(svg),
    uri_reference(
      T1,
      http,
      authority(_User,[www,w3,org],_Port),
      [['2000'],[svg]],
      _Query,
      _Fragment
    )
  ).



:- begin_tests(svg_dcg).

:- use_module(dcg(dcg_content)).
:- use_module(generics(print_ext)).
:- use_module(gv(gv_file)).

test(svg_rect, []):-
  once(phrase(svg_rect(Tree, void, [x(0.5,cm),y(1.5,cm)]), Codes)),
  atom_codes(Atom, Codes),
  formatnl(Atom),
  convert_tree_to_gv([], Tree, dot, pdf, File),
  formatnl(File).

:- end_tests(svg_dcg).
