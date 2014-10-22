:- module(
  sort_ext,
  [
    icompare/3, % ?InvertedOrder
                % @Term1
                % @Term2
    predsort_with_duplicates/3, % :Goal:atom
                                % +List:list
                                % -SortedList:list
    sort/3 % +Unsorted:list
           % -Sorted:list
           % +Options:list(nvpair)
  ]
).

/** <module> Sort extensions

Extensions for sorting lists.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03-2013/04, 2013/09-2013/10, 2013/12,
         2014/07-2014/08
*/

:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate(predmerge_with_duplicates(2,+,+,-)).
:- meta_predicate(predmerge_with_duplicates(2,+,+,+,+,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-,-,-)).

:- predicate_options(sort/3, 3, [
     duplicates(+boolean),
     inverted(+boolean)
  ]).



%! sort(+List:list, -Sorted:list, +Options:list(nvpair)) is det.
% The following options are supported:
%   * =|duplicates(+boolean)|= Whether duplicate elements are retained
%     in the sorted list.
%   * =|inverted(+boolean)|= Whether the sorted list goes from lowest to
%     highest (standard) or from highest to lowest.

sort(Unsorted, Sorted, Options):-
  option(inverted(Inverted), Options, false),
  option(duplicates(Duplicates), Options, false),
  sort(Duplicates, Inverted, Unsorted, Sorted).

%! sort(
%!   +Duplicates:boolean,
%!   +Inverted:boolean,
%!   +Unsorted:list,
%!   -Sorted:list
%! ) is det.
% @tbd Optimize the case for without duplicates & inverted.

% With duplicates & inverted.
sort(true, true, Unsorted, Sorted):- !,
  msort(Unsorted, Sorted0),
  reverse(Sorted0, Sorted).
% With duplicates & uninverted.
sort(true, false, Unsorted, Sorted):- !,
  msort(Unsorted, Sorted).
% Without duplicates & inverted
sort(false, true, Unsorted, Sorted):- !,
  predsort(icompare, Unsorted, Sorted).
% Without duplicates & uninverted.
sort(false, false, Unsorted, Sorted):- !,
  sort(Unsorted, Sorted).


%! predsort_with_duplicates(
%!    +Predicate:atom,
%!    +UnsortedList:list,
%!    -SortedList:list
%! ) is det.
% Variation of the standard predicate predsort/3 that does keeps any
% duplicates (instead of removing them).
%
% @arg Predicate An atomic predicate name of a tertiary predicate.
% @arg UnsortedList ...
% @arg SortedList ...
% @see Slight alteration of predsort/3.

predsort_with_duplicates(Predicate, UnsortedList, SortedList):-
  length(UnsortedList, Length),
  predsort_with_duplicates(
    Predicate,
    Length,
    UnsortedList,
    _,
    SortedList
  ).

%! predsort_with_duplicates(
%!   +Predicate:atom,
%!   +Length:integer,
%!   -SortedListHalf:list(term),
%!   -UnsortedListHalf:list(term),
%!   -SortedList:ordset(term)
%! ) is det.
% The division between =SortedListHalf1= and =UnsortedListHalf2= is defined
% by =Length=, which is the approximate length of both lists.
% The =SortedListHalf= is sorted in this predicate. The
% =UnsortedListHalf= will be sorted in the next iteration.
%
% @arg Predicate The atomic name of a binary semideterministic predicate.
% @arg Length An integer.
% @arg SortedListHalf A list of terms that are already sorted.
% @arg UnsortedListHalf A list of terms that are not yet sorted.
% @arg SortedList An ordered set of terms.

% There are 2 more unsorted terms.
predsort_with_duplicates(
  Predicate,
  2,
  [H1,H2|TailUnsortedList],
  TailUnsortedList,
  SortedList
):- !,
  % We perform one last call to finalize the sorting.
  call(Predicate, Delta, H1, H2),
  sort_with_duplicates(Delta, H1, H2, SortedList).
% There is 1 more unsorted term.
predsort_with_duplicates(
  _Predicate,
  1,
  [H|UnsortedList],
  UnsortedList,
  [H]
):- !.
% There are no more unsorted terms.
predsort_with_duplicates(_Predicate, 0, UnsortedList, UnsortedList, []):- !.
% The recursive case.
predsort_with_duplicates(Predicate, Length, L1, L3, SortedList):-
  % Rounded division of the given length.
  HalfLength1 is Length // 2,
  plus(HalfLength1, HalfLength2, Length),
  predsort_with_duplicates(Predicate, HalfLength1, L1, L2, Result1),
  predsort_with_duplicates(Predicate, HalfLength2, L2, L3, Result2),

  % The two results are themselves ordered, but when put together they may
  % be not sorted anymore. This is what the merge does.
  predmerge_with_duplicates(Predicate, Result1, Result2, SortedList).



% Helpers.

%! i(?Order1, ?Order2) is nondet.
% Inverter of order relations.
% This is used for sorting with the =inverted= option set to =true=.
%
% @arg Order1 An order relation, i.e. <, > or =.
% @arg Order1 An order relation, i.e. <, > or =.

i(<, >).
i(>, <).
i(=, =).

%! icompare(?InvertedOrder, @Term1, @Term2) is det.
% Determine or test the order between two terms in the inversion of the
% standard order of terms.
% This allows inverted sorting, useful for some algorithms that can
% operate (more) quickly on inversely sorted operations, without the
% cost of reverse/2.
% This is used for sorting with the =inverted= option set to =true=.
%
% @arg InvertedOrder One of <, > or =.
% @arg Term1
% @arg Term2
% @see compare/3 using uninverted order predicates.

icompare(InvertedOrder, Term1, Term2):-
  compare(Order, Term1, Term2),
  i(Order, InvertedOrder).


%! predmerge_with_duplicates(+Predicate, +List1, +List2, -Solution)
% Merges the given lists based on the given sort predicate.
% @precondition It is assumed that both lists are themselves sorted.
% @arg Predicate The sort predicate. It should be tertiary, of the form
% <{ <, =, > }, Element1, Element2>.
%
% @arg List1 An ordered list.
% @arg List2 An ordered list.
% @arg Solution An ordered list.

predmerge_with_duplicates(_Predicate, [], MergeResult, MergeResult):- !.
predmerge_with_duplicates(_Predicate, MergeResult, [], MergeResult):- !.
predmerge_with_duplicates(Predicate, [H1 | T1], [H2 | T2], Result):-
  call(Predicate, Delta, H1, H2),
  predmerge_with_duplicates(Delta, Predicate, H1, H2, T1, T2, Result).

%! predmerge_with_duplicates(
%!   +Delta,
%!   +Predicate,
%!   +ElementHalf1,
%!   +ElementHalf2,
%!   +SortedListHalf1,
%!   +SortedListHalf2,
%!   -SortedList
%! ) is det.

% H1 > H2, so place H2 in front of the result, and run again with H1.
predmerge_with_duplicates(>, Predicate, H1, H2, T1, T2, [H2 | Result]):-
  predmerge_with_duplicates(Predicate, [H1 | T1], T2, Result).
% H1 = H2, so place both H1 and H2 in the result (the order does not matter).
predmerge_with_duplicates(=, Predicate, H1, H2, T1, T2, [H1, H2 | Result]):-
  predmerge_with_duplicates(Predicate, T1, T2, Result).
% H1 < H2, so place H1 in front of the result, and run again with H2.

predmerge_with_duplicates(<, Predicate, H1, H2, T1, T2, [H1 | Result]):-
  predmerge_with_duplicates(Predicate, T1, [H2 | T2], Result).


%! sort_with_duplicates(+Delta, +Element1, +Element2, -SortedList:list)
% Returns the sorted list of the two given elements according to Delta.

sort_with_duplicates(<, H1, H2, [H1,H2]).
sort_with_duplicates(=, H1, H2, [H1,H2]).
sort_with_duplicates(>, H1, H2, [H2,H1]).

