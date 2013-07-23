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
    svg_head/3 % +Width:number
               % +Height:number
               % -Head:list
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

