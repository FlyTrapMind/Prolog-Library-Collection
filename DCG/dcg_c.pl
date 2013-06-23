:- module(dcg_c, [c_convert//0]).

c_convert -->
  dcg_replace(c_from(X), c_to(X)).

c_from(X) -->
  {c_trans(X,_Y)},
  X.

c_to(X) -->
  {c_trans(X,Y)},
  Y.

dcg_replace(_From, _To, [], []):- !.
dcg_replace(From, To), To -->
  From, !,
  dcg_replace(From, To).
dcg_replace(From, To), [X] -->
  [X],
  dcg_replace(From, To).

% Replace the bell character with '\b'.
c_trans([92, 98], [7]).
% Replace the line feed character with '\n'.
c_trans([92, 110], [10]).
% Replace the horizontal tab character with '\t'.
c_trans([92, 116], [9]).
