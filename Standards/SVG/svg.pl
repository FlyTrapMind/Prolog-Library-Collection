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
    line/7, % +Options:list(nvpair)
            % +X1:number
            % +Y1:number
            % +X2:number
            % +Y2:number
            % +Tooltip:atom
            % -Line:element
% COLORS
  svg_colors/1 % -Colors:list(atom)
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
% @see svg_head/3

svg_head(size(2, [Width, Height]), Head):-
  svg_head(Width, Height, Head).

%! svg_head(+Width:integer, +Height:integer, -Head:list) is det.
% Returns the markup for the SVG head for graphics with the given
% height and width.

svg_head(Width, Height, [height=Height_cm, width=Width_cm]):-
  format_number(Width, cm, Width_cm),
  format_number(Height, cm, Height_cm).



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
    svg_color0(Color),
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

circle(Options, X0, Y0, R, Tooltip, Element):-
  Attributes = [cx(X0), cy(Y0), r(R) | Options],
  parse_attributes_svg(circle, Attributes, ParsedAttributes),
  Element = element(circle, ParsedAttributes, [Tooltip]).

line(Options, X1, Y1, X2, Y2, Tooltip, Element):-
  Attributes = [x1(X1), y1(Y1), x2(X2), y2(Y2) | Options],
  parse_attributes_svg(line, Attributes, ParsedAttributes),
  Element = element(line, ParsedAttributes, [Tooltip]).



% SVG COLOR SPACE %

% svg_color0(?Color:name) is nondet.
% @see svg_color0/2

svg_color0(Color):-
  svg_color0(Color, _RGB).

%! svg_color(?Name:atom, ?RGB:rgb) is nondet.

svg_color0(aliceblue, rgb(240, 248, 255)).
svg_color0(antiquewhite, rgb(250, 235, 215)).
svg_color0(aqua, rgb(0, 255, 255)).
svg_color0(aquamarine, rgb(127, 255, 212)).
svg_color0(azure, rgb(240, 255, 255)).
svg_color0(beige, rgb(245, 245, 220)).
svg_color0(bisque, rgb(255, 228, 196)).
svg_color0(black, rgb(0, 0, 0)).
svg_color0(blanchedalmond, rgb(255, 235, 205)).
svg_color0(blue, rgb(0, 0, 255)).
svg_color0(blueviolet, rgb(138, 43, 226)).
svg_color0(brown, rgb(165, 42, 42)).
svg_color0(burlywood, rgb(222, 184, 135)).
svg_color0(cadetblue, rgb(95, 158, 160)).
svg_color0(chartreuse, rgb(127, 255, 0)).
svg_color0(chocolate, rgb(210, 105, 30)).
svg_color0(coral, rgb(255, 127, 80)).
svg_color0(cornflowerblue, rgb(100, 149, 237)).
svg_color0(cornsilk, rgb(255, 248, 220)).
svg_color0(crimson, rgb(220, 20, 60)).
svg_color0(cyan, rgb(0, 255, 255)).
svg_color0(darkblue, rgb(0, 0, 139)).
svg_color0(darkcyan, rgb(0, 139, 139)).
svg_color0(darkgoldenrod, rgb(184, 134, 11)).
svg_color0(darkgray, rgb(169, 169, 169)).
svg_color0(darkgreen, rgb(0, 100, 0)).
svg_color0(darkgrey, rgb(169, 169, 169)).
svg_color0(darkkhaki, rgb(189, 183, 107)).
svg_color0(darkmagenta, rgb(139, 0, 139)).
svg_color0(darkolivegreen, rgb(85, 107, 47)).
svg_color0(darkorange, rgb(255, 140, 0)).
svg_color0(darkorchid, rgb(153, 50, 204)).
svg_color0(darkred, rgb(139, 0, 0)).
svg_color0(darksalmon, rgb(233, 150, 122)).
svg_color0(darkseagreen, rgb(143, 188, 143)).
svg_color0(darkslateblue, rgb(72, 61, 139)).
svg_color0(darkslategray, rgb(47, 79, 79)).
svg_color0(darkslategrey, rgb(47, 79, 79)).
svg_color0(darkturquoise, rgb(0, 206, 209)).
svg_color0(darkviolet, rgb(148, 0, 211)).
svg_color0(deeppink, rgb(255, 20, 147)).
svg_color0(deepskyblue, rgb(0, 191, 255)).
svg_color0(dimgray, rgb(105, 105, 105)).
svg_color0(dimgrey, rgb(105, 105, 105)).
svg_color0(dodgerblue, rgb(30, 144, 255)).
svg_color0(firebrick, rgb(178, 34, 34)).
svg_color0(floralwhite, rgb(255, 250, 240)).
svg_color0(forestgreen, rgb(34, 139, 34)).
svg_color0(fuchsia, rgb(255, 0, 255)).
svg_color0(gainsboro, rgb(220, 220, 220)).
svg_color0(ghostwhite, rgb(248, 248, 255)).
svg_color0(gold, rgb(255, 215, 0)).
svg_color0(goldenrod, rgb(218, 165, 32)).
svg_color0(gray, rgb(128, 128, 128)).
svg_color0(grey, rgb(128, 128, 128)).
svg_color0(green, rgb(0, 128, 0)).
svg_color0(greenyellow, rgb(173, 255, 47)).
svg_color0(honeydew, rgb(240, 255, 240)).
svg_color0(hotpink, rgb(255, 105, 180)).
svg_color0(indianred, rgb(205, 92, 92)).
svg_color0(indigo, rgb(75, 0, 130)).
svg_color0(ivory, rgb(255, 255, 240)).
svg_color0(khaki, rgb(240, 230, 140)).
svg_color0(lavender, rgb(230, 230, 250)).
svg_color0(lavenderblush, rgb(255, 240, 245)).
svg_color0(lawngreen, rgb(124, 252, 0)).
svg_color0(lemonchiffon, rgb(255, 250, 205)).
svg_color0(lightblue, rgb(173, 216, 230)).
svg_color0(lightcoral, rgb(240, 128, 128)).
svg_color0(lightcyan, rgb(224, 255, 255)).
svg_color0(lightgoldenrodyellow, rgb(250, 250, 210)).
svg_color0(lightgray, rgb(211, 211, 211)).
svg_color0(lightgreen, rgb(144, 238, 144)).
svg_color0(lightgrey, rgb(211, 211, 211)).
svg_color0(lightpink, rgb(255, 182, 193)).
svg_color0(lightsalmon, rgb(255, 160, 122)).
svg_color0(lightseagreen, rgb(32, 178, 170)).
svg_color0(lightskyblue, rgb(135, 206, 250)).
svg_color0(lightslategray, rgb(119, 136, 153)).
svg_color0(lightslategrey, rgb(119, 136, 153)).
svg_color0(lightsteelblue, rgb(176, 196, 222)).
svg_color0(lightyellow, rgb(255, 255, 224)).
svg_color0(lime, rgb(0, 255, 0)).
svg_color0(limegreen, rgb(50, 205, 50)).
svg_color0(linen, rgb(250, 240, 230)).
svg_color0(magenta, rgb(255, 0, 255)).
svg_color0(maroon, rgb(128, 0, 0)).
svg_color0(mediumaquamarine, rgb(102, 205, 170)).
svg_color0(mediumblue, rgb(0, 0, 205)).
svg_color0(mediumorchid, rgb(186, 85, 211)).
svg_color0(mediumpurple, rgb(147, 112, 219)).
svg_color0(mediumseagreen, rgb(60, 179, 113)).
svg_color0(mediumslateblue, rgb(123, 104, 238)).
svg_color0(mediumspringgreen, rgb(0, 250, 154)).
svg_color0(mediumturquoise, rgb(72, 209, 204)).
svg_color0(mediumvioletred, rgb(199, 21, 133)).
svg_color0(midnightblue, rgb(25, 25, 112)).
svg_color0(mintcream, rgb(245, 255, 250)).
svg_color0(mistyrose, rgb(255, 228, 225)).
svg_color0(moccasin, rgb(255, 228, 181)).
svg_color0(navajowhite, rgb(255, 222, 173)).
svg_color0(navy, rgb(0, 0, 128)).
svg_color0(oldlace, rgb(253, 245, 230)).
svg_color0(olive, rgb(128, 128, 0)).
svg_color0(olivedrab, rgb(107, 142, 35)).
svg_color0(orange, rgb(255, 165, 0)).
svg_color0(orangered, rgb(255, 69, 0)).
svg_color0(orchid, rgb(218, 112, 214)).
svg_color0(palegoldenrod, rgb(238, 232, 170)).
svg_color0(palegreen, rgb(152, 251, 152)).
svg_color0(paleturquoise, rgb(175, 238, 238)).
svg_color0(palevioletred, rgb(219, 112, 147)).
svg_color0(papayawhip, rgb(255, 239, 213)).
svg_color0(peachpuff, rgb(255, 218, 185)).
svg_color0(peru, rgb(205, 133, 63)).
svg_color0(pink, rgb(255, 192, 203)).
svg_color0(plum, rgb(221, 160, 221)).
svg_color0(powderblue, rgb(176, 224, 230)).
svg_color0(purple, rgb(128, 0, 128)).
svg_color0(red, rgb(255, 0, 0)).
svg_color0(rosybrown, rgb(188, 143, 143)).
svg_color0(royalblue, rgb(65, 105, 225)).
svg_color0(saddlebrown, rgb(139, 69, 19)).
svg_color0(salmon, rgb(250, 128, 114)).
svg_color0(sandybrown, rgb(244, 164, 96)).
svg_color0(seagreen, rgb(46, 139, 87)).
svg_color0(seashell, rgb(255, 245, 238)).
svg_color0(sienna, rgb(160, 82, 45)).
svg_color0(silver, rgb(192, 192, 192)).
svg_color0(skyblue, rgb(135, 206, 235)).
svg_color0(slateblue, rgb(106, 90, 205)).
svg_color0(slategray, rgb(112, 128, 144)).
svg_color0(slategrey, rgb(112, 128, 144)).
svg_color0(snow, rgb(255, 250, 250)).
svg_color0(springgreen, rgb(0, 255, 127)).
svg_color0(steelblue, rgb(70, 130, 180)).
svg_color0(tan, rgb(210, 180, 140)).
svg_color0(teal, rgb(0, 128, 128)).
svg_color0(thistle, rgb(216, 191, 216)).
svg_color0(tomato, rgb(255, 99, 71)).
svg_color0(turquoise, rgb(64, 224, 208)).
svg_color0(violet, rgb(238, 130, 238)).
svg_color0(wheat, rgb(245, 222, 179)).
svg_color0(white, rgb(255, 255, 255)).
svg_color0(whitesmoke, rgb(245, 245, 245)).
svg_color0(yellow, rgb(255, 255, 0)).
svg_color0(yellowgreen, rgb(154, 205, 50)).

%! svg_colors(-Colors:list(atom)) is det.
% Returns the list with supported color names.
%
% @arg Colors A list with atomic color names.

svg_colors(Colors):-
  findall(Color, svg_color0(Color), Colors).
