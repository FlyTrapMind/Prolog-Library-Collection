:- module(
  svg,
  [
% FETCHING
    file_to_svg/2, % +File:atom
                   % -SVG:dom
    stream_to_svg/2, % +Stream:stream
                     % -SVG:dom

% GENERATING
    svg_head/2, % +Size:size
                % -Head:list
    svg_head/3, % +Width:number
                % +Height:number
                % -Head:list

% PARSING
    parse_attributes_svg/3, % +Context:oneof([circle,line])
                            % +Attributes:list(nvpair)
                            % -ParsedAttributes:list(nvassignment)

% SPECIFIC SHAPES
    circle/6, % +Options:list(nvpair)
              % +X0:number
              % +Y0:number
              % +R:number
              % +Tooltip:atom
              % -Circle:element
    line/7 % +Options:list(nvpair)
           % +X1:number
           % +Y1:number
           % +X2:number
           % +Y2:number
           % +Tooltip:atom
           % -Line:element
  ]
).

/** <module> SVG

Predictaes that allow vector graphics to be drawn according to
the SVG standards.

# Prolog datatypes

## RGB

A compound term =rgb(Red, Green, Blue)=, where the three color parts are
represent by an integer between 0 and 255 (inclusive).

@author Wouter Beek
@version 2012/10, 2013/01-2013/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(standards(markup)).
:- use_module(xml(xml_namespace)).

:- discontiguous(attribute0(_Name, _Type, _Contexts)).
:- discontiguous(attribute0(_Name, _Type, _Contexts, _Default)).

:- meta_predicate(svg_typecheck(2,+)).

% Assert DTD file locations.
:- db_add_novel(user:file_search_path(dtd, svg(.))).

:- xml_register_namespace(svg, 'http://www.w3.org/2000/svg').



% FETCHING %

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



attribute(Name,  Type, Scopes):-
  attribute0(Name,  Type, Scopes).

attribute(Name,  Type, Scopes):-
  attribute0(Name,  Type, Scopes, _Default).

attribute0(cx, coordinate, [circle], '0').

attribute0(cy, coordinate, [circle], '0').

attribute0(fill, paint, [circle], black).

attribute0(r, svg_length, [circle]).

attribute0(stroke, paint, [circle, line], none).

attribute0('stroke-width', svg_length, [circle, line]).

attribute0(x1, coordinate, [line], '0').
attribute0(x2, coordinate, [line], '0').
attribute0(y1, coordinate, [line], '0').
attribute0(y2, coordinate, [line], '0').



% GENERICS %

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



% PARSING %

char0 -->
  [X],
  % [comma, round_brackets]
  {\+ member(X, [40, 41, 44])}.

% SVG color.
svg_color -->
  number_sign,
  hexdigit_mod_3.
svg_color -->
  "rgb(", (wsp ; ""),
    svg_integer,
  comma0,
    svg_integer,
  comma0,
    svg_integer,
  (wsp ; ""), ")".
svg_color -->
  "rgb(", (wsp ; ""),
    svg_integer, "%",
  comma0,
    svg_integer, "%",
  comma0,
    svg_integer, "%",
  (wsp ; ""), ")".
svg_color --> color_keyword.

% SVG color names defined in the specification.
color_keyword -->
  {
    svg_color(Color, _RGB),
    atom_codes(Color, Codes)
  },
  Codes.

% Comma, respecting whitespace.
comma0 --> (wsp ; ""), comma, (wsp ; "").

comma_wsp --> wsp.
comma_wsp --> wsp, comma, (wsp ; "").
comma_wsp --> comma, (wsp ; "").

comma_wsp_numberZ -->
  comma_wsp,
  number0.
comma_wsp_numberZ -->
  comma_wsp_numberZ,
  comma_wsp,
  number0.

% Coordiantes in SVG.
coordinate --> svg_length.

% Functional notation for an IRI.
funciri --> "url(", iri, ")".

hexdigit_mod_3 --> dcg_multi(hexadecimal_digit, 3), (hexdigit_mod_3 ; "").

% ICC color specification. References a =|color-profile|= element, and one
% or more color component values.

icccolor --> "icc-color(", name_svg, comma_wsp_numberZ, ")".

% Signed integers in SVG.
svg_integer --> (sign ; ""), dcg_plus(decimal_digit).

% @tbd Check whether this is a reliable check for IRIs.
iri --> string(_).

svg_length --> number0.
svg_length --> number0, svg_length_measure.

svg_length_measure --> "%".
svg_length_measure --> "cm".
svg_length_measure --> "em".
svg_length_measure --> "ex".
svg_length_measure --> "in".
svg_length_measure --> "mm".
svg_length_measure --> "pc".
svg_length_measure --> "pt".
svg_length_measure --> "px".

% SVG names are sequences of characters not containing ',', '(', ')'.
name_svg --> char0.
name_svg --> char0, name_svg.

% Real numbers in SVG.
number0 --> svg_integer, exponent.
number0 -->
  (sign ; ""),
  (decimal_digit ; ""),
  ".",
  (decimal_digit ; ""),
  (exponent ; "").

paint --> paint0.
paint --> funciri, (paint0 ; "").
paint --> "inherit".

paint0 --> "none".
paint0 --> "currentColor".
paint0 --> svg_color.
paint0 --> svg_color, icccolor.

% Whitespace in SVG:
wsp --> carriage_return.
wsp --> horizontal_tab.
wsp --> line_feed.
wsp --> space.

%! parse_attribute(
%!   +Context:oneof([circle,line]),
%!   +Attribute:nvpair,
%!   -ParsedAttribute:nvassignment
%! ) is semidet.
% Succeeds if the given attribute can be parsed within the given context.

parse_attribute(Context, Name=Value, Name=Value):-
  parse_attribute0(Context, Name, Value),
  !.
parse_attribute(Context, Attribute, Name=Value):-
  Attribute =.. [Name, Value],
  parse_attribute0(Context, Name, Value).

parse_attribute0(Context, Name, Value):-
  attribute(Name, Type, Contexts),
  memberchk(Context, Contexts),
  !,
  svg_typecheck(Type, Value).

parse_attributes_svg(Context, Attributes, ParsedAttributes):-
  maplist(parse_attribute(Context), Attributes, ParsedAttributes).

% DCG defined types
svg_typecheck(Type, Value):-
  atom_codes(Value, ValueCodes),
  call(Type, ValueCodes-[]),
  !.



% SPECIFIC SHAPES %

circle(O1, X0, Y0, R, Tooltip, Element):-
  merge_options([cx(X0), cy(Y0), r(R)], O1, O2),
  parse_attributes_svg(circle, O2, Attrs),
  Element = element(circle, Attrs, [Tooltip]).

line(O1, X1, Y1, X2, Y2, Tooltip, Element):-
  merge_options([x1(X1), y1(Y1), x2(X2), y2(Y2)], O1, O2),
  parse_attributes_svg(line, O2, Attrs),
  Element = element(line, Attrs, [Tooltip]).

