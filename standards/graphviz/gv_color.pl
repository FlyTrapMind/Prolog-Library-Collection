:- module(
  gv_color,
  [
    gv_color/2, % ?Colorscheme:oneof([svg,x11])
                % ?Color:atom
    color//1, % +Color:compound
    colorList//1 % +Pairs:list(pair(compound,float))
  ]
).

/** <module> GraphViz color

@author Wouter Beek
@tbd Color value `transparent` is only available in the output formats
     ps, svg, fig, vmrl, and the bitmap formats.
@version 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(xpath)).

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(sparql(sparql_char)).

:- use_module(plHtml(html)).
:- use_module(plHtml(html_table)).

%! gv_color(?Colorscheme:oneof([svg,x11]), ?Color:atom) is nondet.

:- dynamic(gv_color/2).

:- initialization(gv_color_download).



% color(+Color:compound)// .
% A *color* is represented by a compound term of one of the following forms:
%   1. `rgb(Red:nonneg,Green:nonneg,Blue:nonneg)`
%   2. `rgba(Red:nonneg,Green:nonneg,Blue:nonneg,Alpha:nonneg)`
%   3. `hsv(Hue:between(0.0,1.0),Saturation:between(0.0,1.0),Value:between(0.0,1.0))`

color(rgb(Red,Green,Blue)) -->
  `#`,
  '#'(3, hex_color, [Red,Green,Blue]).
color(rgbs(Red,Green,Blue,Alpha)) -->
  `#`,
  '#'(4, hex_color, [Red,Green,Blue,Alpha]).
color(hsv(Hue,Saturation,Value)) -->
  '#'(3, hsv_color, [Hue,Saturation,Value]).

hex_color(I) -->
  {W1 is I / 16},
  'HEX'(W1),
  {W2 is I mod 16},
  'HEX'(W2).

hsv_color(D, Head, Tail):-
  format(codes(Head,Tail), '~2f', [D]).


%! colorList(+Pairs:list(pair(compound,float)))// .

colorList(Pairs) -->
  '+'(wc, Pairs).

wc(Color-Float) -->
  color(Color),
  '?'(wc_weight(Float)).

wc_weight(Float) -->
  `;`,
  float(Float).



% Initialization.

gv_color_url('http://www.graphviz.org/doc/info/colors.html').

gv_color_download:-
  gv_color_url(Url),
  download_html(Url, Dom, [html_dialect(html4)]),
  xpath(Dom, //table(1), TableDom1),
  xpath(Dom, //table(2), TableDom2),
  maplist(assert_color_table, [x11,svg], [TableDom1,TableDom2]).

assert_color_table(Colorscheme, TableDom):-
  html_to_table(TableDom, _, Rows),
  append(Rows, Cells),
  forall(
    member(Cell, Cells),
    assert(gv_color(Colorscheme, Cell))
  ).

