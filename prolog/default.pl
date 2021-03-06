:- module(
  default,
  [
    defgoal/2, % :DefGoal_1, ?Value
    defval/2   % +Def, ?Value
  ]
).

/** <module> Default

@author Wouter Beek
@version 2015/07, 2015/11, 2016/05
*/

:- meta_predicate
    defgoal(1, ?).





%! defgoal(:DefGoal_1, ?Value) is det.
% Runs the given goal, whenever the given value is uninstantiated.
% The given goal is assumed to be unary and deterministic, always
% returning an instantiation for `Value`.
%
% ### Example
%
% The following code allows a specific start node to be given for
% traveral, but also allows the start node to be uninstantiated,
% instantiating it to a randomly chosen start node.
%
% ```prolog
% graph_traversal(StartNode) :-
%   default_goal(random_start_node, StartNode),
%   ...
% ```

defgoal(_, X) :-
  ground(X), !.
defgoal(Goal_1, X) :-
  once(call(Goal_1, X)).



%! defval(+Def, ?Value) is det.
% Returns either the given value or the default value, in case there
% is no value given.
%
% ### Example
%
% `Ordering` is a meta-argument that allows a list of elements to be
% arbitrarily ordered.  The use of defval/2 here allows the original
% ordering of elements to be retained in case the `Ordering` argument
% is not instantiated.
%
% ```prolog
% defval(=, Ordering),
% once(call(Ordering, L1, L2))
% ```

defval(_, X):-
  nonvar(X), !.
defval(X, X).
