:- module(
  gv_attr_type,
  [
    gv_attr_type/1, % ?Type:atom
    addDouble//1, % +Double:float
    addPoint//1, % +Point:compound
    arrowType//1, % +ArrowType:atom
    bool//1, % +Boolean:boolean
    clusterMode//1, % +ClusterMode:atom
    dirType//1, % +DirectionType:oneof([back,both,forward,none])
    double//1, % +Double:float
    doubleList//1, % +Doubles:list(float)
    escString//1,
    %layerList//1,
    %layerRange//1,
    lblString//1,
    int//1, % +Integer:integer
    outputMode//1, % +OutputMode:atom
    %packMode//1,
    pagedir//1, % +Pagedir:atom
    point//1, % +Point:compound
    pointList//1, % +Points:list(compound)
    %portPos//1,
    quadType//1, % +QuadType:atom
    rankType//1, % +RankType:atom
    rankdir//1, % +RankDirection:atom
    rect//1, % +Rectangle:compound
    shape//1,
    smoothType//1, % +SmoothType:atom
    %splineType//1,
    %startType//1,
    style//2 % +Context:oneof([cluster,edge,node])
             % +Style:atom
    %viewPort//1
  ]
).
:- reexport(
  library(dcg/basics),
  [
    string//1 % +String:atom
  ]
).
:- reexport(
  gv(gv_color),
  [
    color//1, % +Color:compound
    colorList//1 % +ColorList:list(compound)
  ]
).

/** <module> GraphViz attribute types

@author Wouter Beek
@version 2014/06
*/

:- use_module(dcg(dcg_abnf)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(gv(gv_html)).



%! gv_attr_type(?Type:atom) is nondet.

gv_attr_type(addDouble).
gv_attr_type(addPoint).
gv_attr_type(arrowType).
gv_attr_type(bool).
gv_attr_type(color).
gv_attr_type(colorList).
gv_attr_type(clusterMode).
gv_attr_type(dirType).
gv_attr_type(double).
gv_attr_type(doubleList).
gv_attr_type(escString).
gv_attr_type(layerList).
gv_attr_type(layerRange).
gv_attr_type(lblString).
gv_attr_type(int).
gv_attr_type(outputMode).
gv_attr_type(packMode).
gv_attr_type(pagedir).
gv_attr_type(point).
gv_attr_type(pointList).
gv_attr_type(portPos).
gv_attr_type(quadType).
gv_attr_type(rankType).
gv_attr_type(rankdir).
gv_attr_type(rect).
gv_attr_type(shape).
gv_attr_type(smoothType).
gv_attr_type(splineType).
gv_attr_type(startType).
gv_attr_type(string).
gv_attr_type(style).
gv_attr_type(viewPort).


%! addDouble(+Float:float)// .
% An *addDouble* is represented by a Prolog float.

addDouble(Float) -->
  '?'(`+`),
  double(Float).


%! addPoint(+Point:compound)// .
% An *addPoint* is represented by a compound of the following form:
% `point(X:float,Y:float,InputOnly:boolean)`.

addPoint(Point) -->
  '?'(`+`),
  point(Point).


%! arrowType(+ArrowType:atom)// .

arrowType(V) -->
  {arrowType(V)},
  atom(V).

arrowType(V):-
  primitive_shape(V).
arrowType(V):-
  derived(V).
arrowType(V):-
  backwards_compatible(V).

primitive_shape(box).
primitive_shape(crow).
primitive_shape(circle).
primitive_shape(diamond).
primitive_shape(dot).
primitive_shape(inv).
primitive_shape(none).
primitive_shape(normal).
primitive_shape(tee).
primitive_shape(vee).

derived(odot).
derived(invdot).
derived(invodot).
derived(obox).
derived(odiamond).

backwards_compatible(ediamond).
backwards_compatible(empty).
backwards_compatible(halfopen).
backwards_compatible(invempty).
backwards_compatible(open).


bool(false) --> `false`.
bool(false) --> `no`.
bool(true) --> `true`.
bool(true) --> `yes`.


%! clusterMode(+ClusterMode:atom)// .

clusterMode(V) -->
  {clusterMode(V)},
  atom(V).

clusterMode(global).
clusterMode(local).
clusterMode(none).


%! dirType(+DirectionType:oneof([back,both,forward,none]))// .

dirType(DirType) -->
  {dirType(DirType)},
  atom(DirType).

dirType(back).
dirType(both).
dirType(forward).
dirType(none).


double(Double1) -->
  % float//1 will check for float type.
  {Double2 is Double1 * 1.0},
  float(Double2).


doubleList([H|T]) -->
  double(H),
  '*'(doubleList1, T).

doubleList1(Float) -->
  `:`,
  double(Float).


%! escString(+String:atom)// ,
% @tbd Support for context-dependent replacements.

escString(String) -->
  atom(String).


% @tbd layerList


% @tbd layerRange


lblString(V) -->
  escString(V).
lblString(V) -->
  gv_html_like_label(V).


int(V) -->
  integer(V).


outputMode(V) -->
  {outputMode(V)},
  atom(V).

outputMode(breadthfirst).
outputMode(edgesfirst).
outputMode(nodesfirst).


% @tbd packMode


pagedir(V) -->
  {pagedir(V)},
  atom(V).

pagedir('BL').
pagedir('BR').
pagedir('LB').
pagedir('LT').
pagedir('RB').
pagedir('RT').
pagedir('TL').
pagedir('TR').


%! point(+Point:compound)// .
% A *point* is represented by a compound of the following form:
% `point(X:float,Y:float,InputOnly:boolean)`.

point(point(X,Y,InputOnly)) -->
  float(X),
  `,`,
  float(Y),
  input_only(InputOnly).

input_only(false) --> [].
input_only(true) --> `!`.


pointList(Points) -->
  '*'(point, Points).


% @tbd portPos


quadType(V) -->
  {quadType(V)},
  atom(V).

quadType(fast).
quadType(none).
quadType(normal).


rankType(V) -->
  {rankType(V)},
  atom(V).

rankType(max).
rankType(min).
rankType(same).
rankType(sink).
rankType(source).


rankdir(V) -->
  {rankdir(V)},
  atom(V).

rankdir('BT').
rankdir('LR').
rankdir('RL').
rankdir('TB').


rect(rect(LowerLeftX,LowerLeftY,UpperRightX,UpperRightY)) -->
  float(LowerLeftX), `,`,
  float(LowerLeftY), `,`,
  float(UpperRightX), `,`,
  float(UpperRightY).


shape(V) -->
  {polygon_based_shape(V)},
  atom(V).

polygon_based_shape(assembly).
polygon_based_shape(box).
polygon_based_shape(box3d).
polygon_based_shape(cds).
polygon_based_shape(circle).
polygon_based_shape(component).
polygon_based_shape(diamond).
polygon_based_shape(doublecircle).
polygon_based_shape(doubleoctagon).
polygon_based_shape(egg).
polygon_based_shape(ellipse).
polygon_based_shape(fivepoverhang).
polygon_based_shape(folder).
polygon_based_shape(hexagon).
polygon_based_shape(house).
polygon_based_shape(insulator).
polygon_based_shape(invhouse).
polygon_based_shape(invtrapezium).
polygon_based_shape(invtriangle).
polygon_based_shape(larrow).
polygon_based_shape(lpromoter).
polygon_based_shape('Mcircle').
polygon_based_shape('Mdiamond').
polygon_based_shape('Msquare').
polygon_based_shape(none).
polygon_based_shape(note).
polygon_based_shape(noverhang).
polygon_based_shape(octagon).
polygon_based_shape(oval).
polygon_based_shape(parallelogram).
polygon_based_shape(pentagon).
polygon_based_shape(plaintext).
polygon_based_shape(point).
polygon_based_shape(polygon).
polygon_based_shape(primersite).
polygon_based_shape(promoter).
polygon_based_shape(proteasesite).
polygon_based_shape(proteinstab).
polygon_based_shape(rarrow).
polygon_based_shape(rect).
polygon_based_shape(rectangle).
polygon_based_shape(restrictionsite).
polygon_based_shape(ribosite).
polygon_based_shape(rnastab).
polygon_based_shape(rpromoter).
polygon_based_shape(septagon).
polygon_based_shape(signature).
polygon_based_shape(square).
polygon_based_shape(tab).
polygon_based_shape(terminator).
polygon_based_shape(threepoverhang).
polygon_based_shape(trapezium).
polygon_based_shape(triangle).
polygon_based_shape(tripleoctagon).
polygon_based_shape(utr).


smoothType(V) -->
  {smoothType(V)},
  atom(V).

smoothType(avg_dist).
smoothType(graph_dist).
smoothType(none).
smoothType(power_dist).
smoothType(rng).
smoothType(spring).
smoothType(triangle).


% @tbd splineType


% @tbd startType


%! style(?Context:oneof([cluster,edge,node]), ?Style:atom) is nondet.

style(Context, Style) -->
  {style(Context, Style)},
  atom(Style).

style(cluster, bold).
style(cluster, dashed).
style(cluster, dotted).
style(cluster, filled).
style(cluster, rounded).
style(cluster, solid).
style(cluster, striped).
style(edge, bold).
style(edge, dashed).
style(edge, dotted).
style(edge, solid).
style(node, bold).
style(node, dashed).
style(node, diagonals).
style(node, dotted).
style(node, filled).
style(node, rounded).
style(node, solid).
style(node, striped).
style(node, wedged).


% @tbd viewPort

