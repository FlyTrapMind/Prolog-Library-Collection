:- module(
  rdf_list,
  [
    rdf_is_list/1, % +RDF_List:uri
    rdf_assert_list/3, % +List:list
                       % -RDF_List:uri
                       % +Graph:atom
    rdf_list/2, % +RDF_List:uri
                % -List:list
    rdf_list/3, % +Options:list(nvpair)
                % +RDF_List:uri
                % -List:list
    rdf_list_first/2, % +List:uri
                      % -FirstElement:uri
    rdf_list_last/2, % +List:uri
                     % -LastElement:uri
    rdf_list_length/2, % +List:uri
                       % -Length:number
    rdf_list_next/2, % ?Element:uri
                     % ?NextElement:uri
    rdf_list_occurs_after/2, % +After:uri
                             % +Before:uri
    rdf_list_occurs_before/2, % +Before:uri
                              % +After:uri
    rdf_list_previous/2, % ?Element:uri
                         % ?PreviousElement:uri
    rdf_list_member/2, % ?Element
                       % ?RDF_List:uri
% DEBUG
    rdf_list_name//2 % +Options:list(nvpair)
                     % +RDF_List:or([bnode,iri])
  ]
).

/** <module> RDF_LIST

Support for RDF lists.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/05, 2013/07-2013/09,
         2014/01
*/

:- use_module(dcg(dcg_collection)).
:- use_module(generics(print_ext)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_bnode_map)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_name)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_is_list(r)).
:- rdf_meta(rdf_assert_list(t,r,+)).
:- rdf_meta(rdf_list(r,-)).
:- rdf_meta(rdf_list(+,r,-)).
:- rdf_meta(rdf_list_first(r,r)).
:- rdf_meta(rdf_list_last(r,r)).
:- rdf_meta(rdf_list_length(r,-)).
:- rdf_meta(rdf_list_next(r,r)).
:- rdf_meta(rdf_list_occurs_after(r,r)).
:- rdf_meta(rdf_list_occurs_before(r,r)).
:- rdf_meta(rdf_list_previous(r,r)).
:- rdf_meta(rdf_list_member(r,r)).
:- rdf_meta(rdf_list_name(+,r,?,?)).



%! rdf_is_list(?RDF_List:rdf_list) is semidet.
% Succeeds if the given term is an RDF list.
%
% ## Tricky stuff
%
% For a triple [1] simple entailment can deduce [2].
% We do *not* want to represent `bnode2` as an RDF list,
% since `bnode2` maps to `bnode1` in the blank node map.
%
% ~~~
% [1] <bnode1,rdf:type,rdf:List>
% [2] <bnode2,rdf:type,rdf:List>
% ~~~

rdf_is_list(RDF_List1):-
  \+ rdf_is_literal(RDF_List1),
  rdf_global_id(rdf:'List', C),
  % WATCH OUT! THIS IS VERY TRICKY!
  (b2r(_, RDF_List1, RDF_List2), ! ; RDF_List2 = RDF_List1),
  rdfs_individual(m(t,f,f), RDF_List2, C, _).

%! rdf_assert_list(+List:list, -RDF_List:uri, +Graph:atom) is det.
% Asserts the given, possibly nested list into RDF.
%
% @arg List The, possibly nested, Prolog list.
% @arg RDF_List The URI of the node at which the RDF list starts.
% @arg Graph The atomic name of a graph or unbound.
%
% @author Wouter Beek, elaborating on Sanders original, allowing the graph
%         to be optional and returning the root of the asserted list.
% @author Sander Latour, who wrote the original version, dealing with
%         nested lists.

rdf_assert_list(List, RDF_List, G):-
  add_blank_list_individual(RDF_List, G),
  rdf_assert_list0(List, RDF_List, G).

rdf_assert_list0([], rdf:nil, _Graph).
rdf_assert_list0([H|T], RDF_List, G):-
  (
    is_list(H)
  ->
    rdf_assert_list0(H, H1, G)
  ;
    H1 = H
  ),
  rdf_assert(RDF_List, rdf:first, H1, G),
  (
    T == []
  ->
    rdf_global_id(rdf:nil, TList)
  ;
    add_blank_list_individual(TList, G),
    rdf_assert_list0(T, TList, G)
  ),
  rdf_assert(RDF_List, rdf:rest, TList, G).

add_blank_list_individual(Blank, G):-
  rdf_bnode(Blank),
  rdf_assert_individual(Blank, rdf:'List', G).

%! rdf_list(+RDF_List:rdf_list, -List:list) is det.
% @see Wrapper around rdf_list/3.

rdf_list(RDF_List, List):-
  rdf_list([], RDF_List, List).

%! rdf_list(+Options:list(nvpair), +RDFList:uri, -List:list) is det
% Returns the list that starts at the given node.
%
% @arg Options The following options are supported:
% @arg StartNode The URI of a node that starts the RDF list.
% @arg List A prolog list.
%
% @author Wouter Beek
% @author Sander Latour
%
% The following options are supported:
%   * =|recursive(+RecursivelyApplied:boolean)|=
%     The default value is `true`.

rdf_list(_, RDFList, []):-
  rdf_global_id(rdf:nil, RDFList), !.
rdf_list(O1, RDFList, [H1 | T]):-
  rdf_has(RDFList, rdf:first, H),
  (
    option(recursive(true), O1, true),
    rdf_is_list(H)
  ->
    rdf_list(O1, H, H1)
  ;
    H1 = H
  ),
  rdf_has(RDFList, rdf:rest, RDFTail), !,
  rdf_list(O1, RDFTail, T).

%! rdf_list_first(?List:uri, ?First:uri) is nondet.
% Pairs of lists and their first element.
%
% @arg List an RDF list.
% @arg First The first element of an RDF list.

rdf_list_first(List, First):-
  rdf_has(List, rdf:first, First).

%! rdf_list_first(?List:uri, ?Last:uri) is nondet.
% Pairs of lists and their last element.
%
% @arg List an RDF list.
% @arg Last The last element of an RDF list.

rdf_list_last(List, Last):-
  rdf_has(List, rdf:rest, rdf:nil), !,
  rdf_has(List, rdf:first, Last).
rdf_list_last(List, Last):-
  rdf_has(List, rdf:rest, NextList),
  rdf_list_last(NextList, Last).

%! rdf_list_length(+List:uri, -Length:integer) is det.
% Returns the number of elements in the given list.
%
% @arg List An RDF list.
% @arg Length An integer.

rdf_list_length(List, Length):-
  rdf_list_length(List, 0, Length).

rdf_list_length(List, Length, Length):-
  rdf_has(List, rdf:rest, rdf:nil), !.
rdf_list_length(List, Length, Length):-
  rdf_has(List, rdf:rest, PartialList),
  rdf_list_length(PartialList, PartialLength, Length),
  succ(PartialLength, Length).

%! rdf_list_next(Element, NextElement) is nondet.
% Returns pairs of consecutive elements in a list.
%
% @arg Element A resource that is an element in an RDF list.
% @arg NextElement A resource that is an element in an RDF list.

rdf_list_next(Element, NextElement):-
  rdf_has(List, rdf:first, Element),
  rdf_has(List, rdf:rest, NextList),
  \+ rdf_global_id(rdf:nil, NextList),
  rdf_has(NextList, rdf:first, NextElement).

rdf_list_occurs_after(After, Before):-
  After \== Before,
  rdf_list_occurs_after0(After, Before).
rdf_list_occurs_after0(X, X).
rdf_list_occurs_after0(After1, Before):-
  rdf_list_previous(After1, After2),
  rdf_list_occurs_after0(After2, Before).

rdf_list_occurs_before(Before, After):-
  Before \== After,
  rdf_list_occurs_before0(Before, After).
rdf_list_occurs_before0(X, X).
rdf_list_occurs_before0(Before1, After):-
  rdf_list_next(Before1, Before2),
  rdf_list_occurs_before0(Before2, After).

%! rdf_list_previous(Element, PreviousElement) is nondet.
% Returns pairs of inverted consecutive elements in a list.
%
% @arg Element A resource that is an element in an RDF list.
% @arg PreviousElement A resource that is an element in an RDF list.

rdf_list_previous(Element, PreviousElement):-
  rdf_list_next(PreviousElement, Element).

%! rdf_list_member(?Element, ?RDF_List:rdf_list) is nondet.
% @see Variant of member/2 for RDF lists.

rdf_list_member(Element, RDF_List):-
  rdf_list_first(RDF_List, FirstElement),
  rdf_list_member_(Element, FirstElement).
rdf_list_member_(Element, Element).
rdf_list_member_(Element, TempElement1):-
  rdf_list_next(TempElement1, TempElement2),
  rdf_list_member_(Element, TempElement2).



% DEBUG %

rdf_list_name(O1, RDF_List) -->
  % Recursively retrieve the contents of the RDF list.
  % This has to be done non-recursively, since the nested
  % Prolog list `[a,[b,c]]` would bring rdf_term_name//1 into
  % trouble when it comes accross `[b,c]`
  % (which fails the check for RDF list).
  {rdf_list([recursive(false)], RDF_List, RDF_Terms)},
  
  list(rdf_term_name(O1), RDF_Terms).

