:- module(
  ordset_ext,
  [
    pairs_to_ord_sets/2 % +Pairs:list(pair(iri))
                        % -Sets:list(ordset(iri))
  ]
).

/** <module> Ordered set extensions

Extensions for SWI-Prolog library `ordsets`.

@author Wouter Beek
@version 2013/09-2013/10
*/

:- use_module(library(lists)).
:- use_module(library(ordsets)).



%! pairs_to_ord_set(
%!   +OldPairs:list(pair),
%!   -Set:ordset,
%!   -NewPairs:list(pair)
%! ) is det.
% Returns the equivalence closure of the first of the given pairs w.r.t.
% the other pair.
% The pairs that were not used to calculate the closure are returned.

pairs_to_ord_set(Pairs1, Set2, Pairs3):-
  % Take the first alignment pair.
  selectchk(X-Y, Pairs1, Pairs2),
  
  % Turn the pair into a sorted list.
  list_to_ord_set([X,Y], Set1),
  
  % Add the members from all the other alignment pairs
  % that reach into this alignment set.
  pairs_to_ord_set(Set1, Pairs2, Set2, Pairs3).

%! pairs_to_ord_set(
%!   +OldSet:ordset,
%!   +OldPairs:list(pair),
%!   -OldSet:ordset,
%!   -OldPairs:list(pair)
%! ) is det.

pairs_to_ord_set(Set1, Pairs1, Set3, Pairs3):-
  % The next alignments we add must relate to a resource
  % that is already in the alignment set.
  member(X, Set1),

  % Look up a related resource in the alginment pairs.
  % The pair can be found in either direction.
  (
    selectchk(X-Y, Pairs1, Pairs2)
  ;
    selectchk(Y-X, Pairs1, Pairs2)
  ),
  % We can safely add a cut here, since we will come back for any
  % `Y-X` option that appears after a `X-Y` option.
  !,

  % The added resource must not already appear in the alignment set.
  % Maybe some alignments contain double occurrences of the same pair.
  % The alignment set stays the same in that case.
  ord_add_element(Set1, Y, Set2),

  % Recurse.
  pairs_to_ord_set(Set2, Pairs2, Set3, Pairs3).
pairs_to_ord_set(Set, Pairs, Set, Pairs).

%! pairs_to_ord_sets(
%!   +Pairs:list(pair(iri)),
%!   -Sets:ordset(ordset(iri))
%! ) is det.
% For instance, the following pairs:
% ~~~
% <a,b>
% <a,c>
% <d,e>
% ~~~
% are converted to the following set:
% ~~~
% {{a,b,c},{d,e}}
% ~~~

pairs_to_ord_sets(Pairs, Sets):-
  pairs_to_ord_sets(Pairs, [], Sets).

pairs_to_ord_sets([], Sets, Sets):- !.
pairs_to_ord_sets(Pairs1, Sets1, Sets3):-
  pairs_to_ord_set(Pairs1, Set, Pairs2),
  ord_add_element(Sets1, Set, Sets2),
  pairs_to_ord_sets(Pairs2, Sets2, Sets3).
