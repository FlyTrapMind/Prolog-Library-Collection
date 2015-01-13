:- module(
  sort_ext,
  [
    gnu_sort/2, % +File:atom
                % +Options:list(nvpair)
    icompare/3, % ?InvertedOrder
                % @Term1
                % @Term2
    predsort_with_duplicates/3, % :Goal:atom
                                % +Unsorted:list
                                % -Sorted:list
    sort/3 % +Unsorted:list
           % -Sorted:list
           % +Options:list(nvpair)
  ]
).

/** <module> Sort extensions

Extensions for sorting lists.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03-2013/04, 2013/09-2013/10, 2013/12,
         2014/07-2014/08, 2014/11, 2015/01
*/

:- use_module(library(error)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).
:- use_module(library(process)).

:- use_module(os(run_ext)).

:- meta_predicate(merge_with_duplicates(3,+,+,-)).
:- meta_predicate(merge_with_duplicates(+,3,+,+,+,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-)).
:- meta_predicate(predsort_with_duplicates(3,+,-,-,-)).

:- predicate_options(gnu_sort/2, 2, [
     duplicates(+boolean),
     output(+atom)
   ]).

:- predicate_options(sort/3, 3, [
     duplicates(+boolean),
     inverted(+boolean)
  ]).





%! gnu_sort(+File:atom, +Options:list(nvpair)) is det.

gnu_sort(File, _):-
  var(File), !,
  instantiation_error(File).
gnu_sort(File, _):-
  \+ exists_file(File), !,
  existence_error(file, File).
gnu_sort(File, _):-
  \+ access_file(File, read), !,
  permission_error(read, file, File).
gnu_sort(File, Options):-
  gnu_sort_args(Options, Args),
  handle_process(sort, [file(File)|Args], [program('GNU sort')]).

gnu_sort_args([], []).
gnu_sort_args([duplicates(false)|T1], ['-u'|T2]):- !,
  gnu_sort_args(T1, T2).
gnu_sort_args([output(File)|T1], ['-o',file(File)|T2]):- !,
  gnu_sort_args(T1, T2).
gnu_sort_args([_|T1], L2):-
  gnu_sort_args(T1, L2).



%! sort(+Unsorted:list, -Sorted:list, +Options:list(nvpair)) is det.
% The following options are supported:
%   * `duplicates(+boolean)` Whether duplicate elements are retained
%     in the sorted list.
%   * `inverted(+boolean)` Whether the sorted list goes from lowest to
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



%! predsort_with_duplicates(:Goal, +Unsorted:list, -Sorted:list) is det.
% Variation of the standard predicate predsort/3 that does keeps any
% duplicates (instead of removing them).
%
% @see predsort/3.

predsort_with_duplicates(Goal, Unsorted, Sorted):-
  length(Unsorted, Length),
  predsort_with_duplicates(
    Goal,
    Length,
    Unsorted,
    _,
    Sorted
  ).

%! predsort_with_duplicates(
%!   :Goal,
%!   +Length:integer,
%!   -SortedHalf:list,
%!   -UnsortedHalf:list,
%!   -Sorted:ordset
%! ) is det.
% The division between =Sorted1= and =Unsorted2= is defined
% by =Length=, which is the approximate length of both lists.
% The =SortedHalf= is sorted in this predicate. The
% =UnsortedHalf= will be sorted in the next iteration.
%
% @arg Goal The atomic name of a binary semideterministic predicate.
% @arg Length An integer.
% @arg SortedHalf A list of terms that are already sorted.
% @arg UnsortedHalf A list of terms that are not yet sorted.
% @arg Sorted An ordered set of terms.

% There are 2 more unsorted terms.
predsort_with_duplicates(
  Goal,
  2,
  [H1,H2|TailUnsorted],
  TailUnsorted,
  Sorted
):- !,
  % We perform one last call to finalize the sorting.
  call(Goal, Delta, H1, H2),
  sort_with_duplicates(Delta, H1, H2, Sorted).
% There is 1 more unsorted term.
predsort_with_duplicates(_, 1, [H|Unsorted], Unsorted, [H]):- !.
% There are no more unsorted terms.
predsort_with_duplicates(_, 0, Unsorted, Unsorted, []):- !.
% The recursive case.
predsort_with_duplicates(Goal, Length, L1, L3, Sorted):-
  % Rounded division of the given length.
  HalfLength1 is Length // 2,
  plus(HalfLength1, HalfLength2, Length),
  predsort_with_duplicates(Goal, HalfLength1, L1, L2, Sorted1),
  predsort_with_duplicates(Goal, HalfLength2, L2, L3, Sorted2),

  % The two results are themselves ordered, but when put together they may
  % be not sorted anymore. This is what the merge does.
  merge_with_duplicates(Goal, Sorted1, Sorted2, Sorted).



% HELPERS

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



%! merge_with_duplicates(
%!   :Goal,
%!   +Sorted1:list,
%!   +Sorted2:list,
%!   -Sorted:list
%! ) is det.
% Merges the given two sorted lists into a single sorted list,
% according to the given sorting Goal.
%
% Notice that the sort predicate is tertiary, i.e., of the following form:
% ```
% Goal({<|`>},Element1,Element2)
% ```

merge_with_duplicates(_, [], Sorted, Sorted):- !.
merge_with_duplicates(_, Sorted, [], Sorted):- !.
merge_with_duplicates(Goal, [H1|T1], [H2|T2], Sorted):-
  call(Goal, Delta, H1, H2),
  merge_with_duplicates(Delta, Goal, H1, H2, T1, T2, Sorted).

%! merge_with_duplicates(
%!   +Delta:oneof([<,=,>]),
%!   :Goal,
%!   +Element1,
%!   +Element2,
%!   +Sorted1:list,
%!   +Sorted2:list,
%!   -Sorted:list
%! ) is det.

% H1 > H2, so place H2 in front of the result, and run again with H1.
merge_with_duplicates(>, Goal, H1, H2, T1, T2, [H2|Sorted]):-
  merge_with_duplicates(Goal, [H1|T1], T2, Sorted).
% H1 = H2, so place both H1 and H2 in the result (the order does not matter).
merge_with_duplicates(=, Goal, H1, H2, T1, T2, [H1,H2|Sorted]):-
  merge_with_duplicates(Goal, T1, T2, Sorted).
% H1 < H2, so place H1 in front of the result, and run again with H2.
merge_with_duplicates(<, Goal, H1, H2, T1, T2, [H1|Sorted]):-
  merge_with_duplicates(Goal, T1, [H2|T2], Sorted).



%! sort_with_duplicates(
%!   +Delta:oneof([<,=,>]),
%!   +Element1,
%!   +Element2,
%!   -Sorted:list
%! ) is det.
% Returns the sorted list of the two given elements according to Delta.

sort_with_duplicates(<, H1, H2, [H1,H2]).
sort_with_duplicates(=, H1, H2, [H1,H2]).
sort_with_duplicates(>, H1, H2, [H2,H1]).
