:- module(
  svg,
  [
% DCGs
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
% FILE
    file_to_svg/2, % +File:atom
                   % -SVG:dom
    stream_to_svg/2, % +Stream:stream
                     % -SVG:dom
% DOM HEAD
    svg_head/2, % +Size:size
                % -Head:list
    svg_head/3 % +Width:number
               % +Height:number
               % -Head:list
  ]
).

/** <module> SVG

Predictaes that allow vector graphics to be drawn according to
the SVG standards.
Including DCG rules implementing the SVG 1.1 Second Edition standard.

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
@version 2012/10, 2013/01-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rfc(rfc_2396)).
:- use_module(standards(markup)).
:- use_module(svg(svg_attributes)).
:- use_module(svg(svg_colors)).
:- use_module(xml(xml_dcg)).
:- use_module(xml(xml_namespace)).

:- dynamic(user:mime_type/2).
:- dynamic(user:public_identifier/2).
:- dynamic(user:system_identifier/2).

% DTD file location.
:- db_add_novel(user:file_search_path(dtd, svg(.))).
% GZipped SVG file extension.
:- db_add_novel(user:prolog_file_type(svgz, gzip)).
:- db_add_novel(user:prolog_file_type(svgz, svg)).
% SVG MIME type.
:- db_add_novel(user:mime_type(svg, 'image/svg+xml')).
% SVG public identifier.
:- db_add_novel(
  user:public_identifier(svg, 'PUBLIC "-//W3C//DTD SVG 1.1//EN"')).
% SVG system identifier.
:- db_add_novel(
  user:system_identifier(
    svg,
    'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'
  )
).

% A special SVG file extension is used on Macintosh HFS file systems.
:- if(is_mac).
:- db_add_novel(user:prolog_file_type('svg ', svg)).
:- endif.
% The default SVG file extension is used on Unix and Windows.
:- if((is_unix ; is_windows)).
:- db_add_novel(user:prolog_file_type(svg, svg)).
:- endif.

% SVG namespace.
:- xml_register_namespace(svg, 'http://www.w3.org/2000/svg').

:- meta_predicate(svg_document(-,//,?,?,?)).
:- meta_predicate(svg_fragment_base(-,//,?,?)).
:- meta_predicate(svg_fragment_nonbase(-,//,?,?)).
:- meta_predicate(svg_header(-,//,?,?,?)).
:- meta_predicate(svg_xmlns_nonbase(-,//,?,?)).



% DCGs %

%! svg_document(-Tree:compound, :DCG_Namespace, ?SVG_DCGs:list(dcg))//

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
  xml_entity(word(svg), svg_xmlns_base(T1)),
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
%   1. =|base_profile(?ProfileName:atom)|=
%   2. =|content_script_type(MediaType:atom)|=
%   3. =|content_style_type(MediaType:atom)|=
%   4. =|height(?Number:float,?Unit:atom)|=
%   5. =|preserve_aspect_ratio(?Defer:boolean,?Align:compound,?MeetOrSlice:oneof([meet,slice]))|=
%   6. =|stroke(?Color:atom)|=
%   7. =|stroke_width(?Number:float,?Unit:atom)|=
%   1. =|standalone(?Alone:boolean)|=
%   2. =|version(?Major:integer,?Minor:integer)|=

svg_header(Tree, DCG_Namespace, Attrs1) -->
  {xml_inject_attributes(DCG_Namespace, Attrs1, Attrs2, Trees)},
  xml_entity(DCG_Namespace, word(svg), Attrs2),
  {parse_tree(svg, Trees, Tree)}.

svg_xmlns_base(xmlns(T1)) -->
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



% FILE %

file_to_svg(File, SVG):-
  open(File, read, Stream),
  stream_to_svg(Stream, SVG).

stream_to_svg(Stream, SVG):-
  load_structure(
    stream(Stream),
    SVG,
    [
      dialect(xmlns),
      max_errors(-1),
      shorttag(false),
      space(default),
      syntax_errors(quiet)
    ]
  ).



% DOM HEAD %

%! svg_head(+Size:size, -Head:list) is det.
% Returns the markup for the SVG head for graphics with the given 2D size.
%
% @see Wrapper around svg_head/3.

svg_head(size(2,[Width,Height]), Head):-
  svg_head(Width, Height, Head).

%! svg_head(+Width:integer, +Height:integer, -Head:list) is det.
% Returns the markup for the SVG head for graphics with the given
% height and width.

svg_head(Width, Height, [height=Height_cm, width=Width_cm]):-
  format_number(cm, Width, Width_cm),
  format_number(cm, Height, Height_cm).

