:- module(
  rdf_list,
  [
    is_rdf_list/1, % +RDF_List:uri
    rdf_list/2, % +RDF_List:uri
                % -List:list
    rdf_list/3, % +O:list(nvpair)
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
    rdf_list_member/2 % ?Element
                      % ?RDF_List:uri
  ]
).

/** <module> RDF_LIST

Support for RDF lists.

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(is_rdf_list(r)).
:- rdf_meta(rdf_list(+,r,-)).
:- rdf_meta(rdf_list_first(r,r)).
:- rdf_meta(rdf_list_last(r,r)).
:- rdf_meta(rdf_list_length(r,-)).
:- rdf_meta(rdf_list_next(r,r)).
:- rdf_meta(rdf_list_occurs_after(r,r)).
:- rdf_meta(rdf_list_occurs_before(r,r)).
:- rdf_meta(rdf_list_previous(r,r)).
:- rdf_meta(rdf_list_member(r,r)).



%! is_rdf_list(?RDF_List:rdf_list) is semidet.
% Succeeds if the given term is an RDF list.

is_rdf_list(RDF_List):-
  rdfs_individual_of(RDF_List, rdf:'List').

%! rdf_list(+RDF_List:rdf_list, -List:list) is det.
% @see Wrapper around rdf_list/3.

rdf_list(RDF_List, List):-
  rdf_list([], RDF_List, List).

%! rdf_list(+O:list(nvpair), +RDFList:uri, -List:list) is det
% Returns the list that starts at the given node.
%
% @arg Options The following options are supported:
%      1. =|recursive(+RecursivelyApplied:boolean)|=
%         The default value is `true`.
% @arg StartNode The URI of a node that starts the RDF list.
% @arg List A prolog list.
%
% @author Wouter Beek
% @author Sander Latour

rdf_list(_O, RDFList, []):-
  rdf_global_id(rdf:nil, RDFList), !.
rdf_list(O, RDFList, [H1 | T]):-
  rdf(RDFList, rdf:first, H),
  (
    option(recursive(true), O, true),
    is_rdf_list(H)
  ->
    rdf_list(O, H, H1)
  ;
    H1 = H
  ),
  rdf_has(RDFList, rdf:rest, RDFTail), !,
  rdf_list(O, RDFTail, T).

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
