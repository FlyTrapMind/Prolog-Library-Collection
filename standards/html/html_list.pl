:- module(
  html_list,
  [
    html_list//1, % +Elements:list(ground)
    html_list//2, % +Elements:list(ground)
                  % +Options:list(nvpair)
    html_list//3 % +Elements:list(ground)
                 % :ItemWriter
                 % +Options:list(nvpair)
  ]
).

/** <module> HTML list

Support for generating HTML lists.

@author Wouter Beek
@version 2013/10-2013/11, 2014/01, 2014/03, 2014/05
*/

:- use_module(library(http/html_write)).
:- use_module(library(option)).

:- use_module(dcg(dcg_meta)).

:- use_module(pl_web(html_pl_term)).

:- meta_predicate(html_list(+,3,+,?,?)).
:- meta_predicate(html_list_items(//,+,?,?)).



%! html_list(+Elements:list(ground))// is det.
% @see Wrapper for html_list//3 using html_pl_term//1 as the item writer
%      and no list attributes.

html_list(Elements) -->
  html_list(Elements, html_pl_term, []).

%! html_list(+Elements:list(ground), +ListAttributes:list(nvpair))// is det.
% @see Wrapper for html_list//3 using html_pl_term//1 as the item writer.

html_list(Elements, ListAttrs) -->
  html_list(Elements, html_pl_term, ListAttrs).

%! html_list(
%!   +Elements:list(ground),
%!   :ItemWriter,
%!   +ListAttributes:list(nvpair)
%! )// is det.
% Generates an HTML list containing the given list items.
%
% The following list options are supported:
%   * =|ordered(Ordered:boolean)|=
%     Whether an order =ol= or unordered =ul= HTML list is used.
%
% @param Elements A list of Prolog terms that will be written as the items]
%        of the HTML list using the given item writer.
%        If an element is a pair, i.e. `Attributes:list(nvpair)-term`,
%        then the former element part will be used as the HTML attributes
%        of the list item.
% @param ItemWriter A uary DCG rule, writing Prolog terms as HTML.
% @param ListAttributes A list of HTML attributes for lists.

html_list(Elements, ItemWriter, ListAttrs1) -->
  {select_option(ordered(Ordered), ListAttrs1, ListAttrs2, false)},
  (
    {Ordered == false}
  ->
    html(ul(ListAttrs2, \html_list_items(ItemWriter, Elements)))
  ;
    {Ordered == true}
  ->
    html(ol(ListAttrs2, \html_list_items(ItemWriter, Elements)))
  ).


html_list_items(_, []) --> !, [].
% First try to call the `ItemWriter` DCG on the `H` argument.
% `H` may be a pair of the form `X-Y`.
html_list_items(ItemWriter, [H|T]) -->
  html(li(\dcg_call(ItemWriter, H))), !,
  html_list_items(ItemWriter, T).
% Since generating the cell for `H` failed, we interpret
% the first element in the pair as an options list (i.e. HTML attributes)
% to the list item tag.
html_list_items(ItemWriter, [ItemAttrs-H|T]) -->
  html(li(ItemAttrs, \dcg_call(ItemWriter, H))),
  html_list_items(ItemWriter, T).

