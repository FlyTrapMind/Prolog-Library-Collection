:- module(
  gv_attrs,
  [
    gv_attr/3 % +Context:oneof([cluster,edge,graph,node,subgraph])
              % +Attr1:nvpair
              % +Attr2:nvpair
  ]
).

/** <module> GraphViz attributes v2

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(xpath)).

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- use_module(plHtml(html)).
:- use_module(plHtml(html_table)).

:- use_module(gv(gv_attr_type)). % DCGs implementing the GraphViz attribute types.

%! gv_attr(
%!   ?Name:atom,
%!   ?UsedBy:list(oneof([cluster,edge,graph,node,subgraph])),
%!   ?Types:list(atom),
%!   ?Default,
%!   ?Minimum,
%!   ?Notes:atom
%! ) is nondet.

:- dynamic(gv_attr/6).

:- initialization(gv_attrs_download).



gv_attr(Context, N=V, N=V):-
  var(V), !,
  gv_attr(N, UsedBy, _, V, _, _),
  memberchk(Context, UsedBy).
gv_attr(Context, N=V1, N=V2):-
  gv_attr(N, UsedBy, Types, _, Minimum, _),
  memberchk(Context, UsedBy),
  member(Type, Types),
  (
    Type == style
  ->
    Dcg =.. [Type,Context,V1]
  ;
    Dcg =.. [Type,V1]
  ),
  dcg_phrase(Dcg, V2),
  check_minimum(V1, Minimum).

check_minimum(_, ''):- !.
check_minimum(V, Min1):-
  atom_number(Min1, Min2),
  Min2 =< V.



% Download attributes from graphviz.org

gv_attrs_download:-
  gv_attrs_url(Url),
  download_html(Url, Dom, [html_dialect(html4)]),

  xpath(Dom, //table(@align=center), TableDom),
  % @tbd This does not work, since in `record_name(Element, Name)`,
  %      `Element` is a signleton list whereas a compound term is expected.
  %%%%xpath(Dom, /html/body/table, TableDom),

  html_to_table(TableDom, _, Rows),
  maplist(assert_gv_attr, Rows).

gv_attrs_url('http://www.graphviz.org/doc/info/attrs.html').

assert_gv_attr([Name,UsedBy1,Types1,Default1,Minimum,Notes]):-
  dcg_phrase(translate_usedby(UsedBy2), UsedBy1),
  once(dcg_phrase(translate_type(Types2), Types1)),
  sort(UsedBy2, UsedBy3),
  translate_default(Default1, Default2),
  assert(gv_attr(Name, UsedBy3, Types2, Default2, Minimum, Notes)).

% The empty string is represented by the empty atom.
translate_default('""', ''):- !.
% The absence of a default value is represented by an uninstantiated variable.
translate_default('<none>', _):- !.
translate_default(Default, Default).

translate_type([H|T]) -->
  {gv_attr_type(H)},
  atom(H),
  whites,
  translate_type(T).
translate_type([]) --> [].

translate_usedby([cluster|T]) --> `C`, !, translate_usedby(T).
translate_usedby([edge|T]) --> `E`, !, translate_usedby(T).
translate_usedby([graph|T]) --> `G`, !, translate_usedby(T).
translate_usedby([node|T]) --> `N`, !, translate_usedby(T).
translate_usedby([subgraph|T]) --> `S`, !, translate_usedby(T).
translate_usedby([]) --> [].

