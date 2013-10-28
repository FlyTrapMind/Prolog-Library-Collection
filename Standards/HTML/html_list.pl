:- module(
  html_list,
  [
    html_list//3, % +ListOptions:list(nvpair)
                  % +ListItemOptions:list(nvpair)
                  % +Items:list(list(dsl))
    html_link_list//2 % +ListOptions:list(nvpair)
                      % +Parent:atom
  ]
).

/** <module> HTML list

Support for generating HTML lists.

@author Wouter Beek
@version 2013/10
*/

:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).



html_list(L_O1, LI_O1, Items) -->
  {select_option(ordered(Ordered), L_O1, L_O2, false)},
  {
    findall(
      li(LI_O1, Entry),
      member(Entry, Items),
      ListItems
    )
  },
  (
    {Ordered == false}
  ->
    html(ul(L_O2, ListItems))
  ;
    {Ordered == true}
  ->
    html(ol(L_O2, ListItems))
  ).

html_link_list(O1, Parent) -->
  {
    findall(
      li(a(href=Link3,Name)),
      (
        http:location(Name, ParentSpec1, _Priority),
        ParentSpec1 =.. [Parent,_],
        ParentSpec2 =.. [Parent,'.'],
        http_absolute_location(ParentSpec2, Link1, []),
        atomic_list_concat([Link1,Name], Link2),
        file_name_extension(Link2, html, Link3)
      ),
      ListItems
    )
  },
  {select_option(ordered(Ordered), O1, O2, false)},
  (
    {Ordered == false}
  ->
    html(ul(O2, ListItems))
  ;
    {Ordered == true}
  ->
    html(ol(O2, ListItems))
  ).

