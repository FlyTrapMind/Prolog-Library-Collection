:- module(
  db_ext,
  [
    db_add/1, % +New
    db_add_novel/1, % +New
    db_replace/2, % +New
                  % +Pattern:list(oneof([e,r]))
    db_replace/2, % +Old
                  % -New
    db_replace_novel/2, % +New
                        % +Pattern:list(oneof([e,r]))
    db_replace_novel/2 % +Old
                       % -New
  ]
).

/** <module> DB_EXT

Database extensions.

Replacement predicates should specify which parameters
should be replaced and which should stay the same.

Example: =|rdf_namespace_color(rdf, red)|= should replace
=|rdf_namespace_color(rdf, blue)|=, but not
=|rdf_namespace_color(rdfs, blue)|=.

@author Wouter Beek
@version 2013/04-2013/05, 2013/08
*/

:- use_module(library(apply)).

:- meta_predicate(db_add(:)).
:- meta_predicate(db_add_novel(:)).
:- meta_predicate(db_replace(:,:)).
:- meta_predicate(db_replace_novel(:,:)).



db_add(New):-
  assert(New).

%! db_add_novel(+New) is det.
% Asserts the given fact only if it does not already exist.

db_add_novel(New):-
  call(New), !.
db_add_novel(New):-
  assert(New).

%! db_replace(:Old, +Pattern:list(oneof([e,r]))) is det.
%! db_replace(:Old, :New) is det.
% Replaces at most one asserted fact (if present) with another one.

db_replace(New, _Mod:Pattern):-
  is_list(Pattern), !,
  new_to_old(New, Pattern, Old),
  db_replace(Old, New).
% There is some fact that will be overwritten.
db_replace(Old, New):-
  retract(Old), !,
  assert(New).
% There is nothing to overwrite.
db_replace(_Old, New):-
  assert(New).

%! db_replace_novel(:Old, +Pattern:list(oneof([e,r]))) is det.
%! db_replace_novel(:Old, :New) is det.

db_replace_novel(New, _Mod:Pattern):-
  is_list(Pattern), !,
  new_to_old(New, Pattern, Old),
  db_replace_novel(Old, New).
db_replace_novel(Old, New):-
  call(New),
  \+ call(Old), !.
db_replace_novel(Old, New):-
  db_replace(Old, New).

match_argument(X, e, X):- !.
match_argument(_, _, _):- !.

%! new_to_old(:NewFact, +Pattern:list(oneof([e,r])), :OldFact) is nondet.
% Returns an old fact that matches the given new fact description,
% using the given pattern specification.
% In the pattern:
%   * =e= stands for arguments that should be the same
%     as in the given (new) fact.
%   * =r= stands for arguments that should be different
%     in a fact in order to count as an old fact.

new_to_old(New, Pattern, Module:Old):-
  strip_module(New, Module, Plain),
  Plain =.. [Predicate | NewArguments],
  maplist(match_argument, NewArguments, Pattern, OldArguments),
  Old =.. [Predicate | OldArguments].

