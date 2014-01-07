:- module(
  sparql_build,
  [
    formulate_sparql/6 % +Graph:compound
                       % +Prefixes:list(atom)
                       % +Select:compound
                       % +Where:atom
                       % +Extra:compound
                       % -SPARQL_Query:atom
  ]
).

/** <module> SPARQL build

Predicates that support the construction of SPARQL queries.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2013/12
*/

:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(sparql(sparql_db)).



%! formulate_extra(+Extra:compound, -Statement:atom) is det.
% Formulates either a "limit" or an "order by" statement.
%
% @arg Extra A compound term.
%        Either =|limit(+Options:list(nvpair),+Limit:positive_integer)|=
%        or =|sort_by(+Options:list(nvpair),+VarNames:list(atom))|=.

formulate_extra(Extra, ExtraStmt):-
  Extra =.. [ExtraF,O1,Arg],
  formulate_extra(ExtraF, O1, Arg, ExtraStmt).

formulate_extra(limit, _O1, Max, LimitStmt):- !,
  formulate_limit(Max, LimitStmt).
formulate_extra(order_by, O1, VarNames, OrderByStmt):-
  formulate_vars(VarNames, Vars1),
  atomic_list_concat(Vars1, ' ', Vars2),
  option(order(Order), O1, asc),
  format(atom(OrderByStmt), 'ORDER BY ~w(~w)', [Order,Vars2]).

%! formulate_graph(+Option:nvpair, -Statement:atom) is det.
% Formulates a statement specifying a default graph.
%
% @arg Option A name-value pair of the form
%        =|default_graph(+DefaultGraph:url)|=.
% @arg Statement

formulate_graph(default_graph(G), GStmt):-
  format(atom(GStmt), '# Default graph (located at ~w)', [G]).

%! formulate_limit(+Limit:nonneg, -Statement:atom) is det.
% Formulates a SPARQL limit statement.
%
% @arg Limit The maximum number of returned search results.
% @arg Statement

formulate_limit(Limit, LimitStmt):-
  nonneg(Limit),
  format(atom(LimitStmt), 'LIMIT ~w', [Limit]).

%! formulate_mode(+Mode:oneof([select]), -Statement:atom) is det.
% Formulates a SPARQL mode statement.
% Currently only the "select" mode is supported.
%
% @arg Mode A compound term. The functor is a mode,
%        the argument is a list of SPARQL variable names (for `SELECT`).
% @arg Statement

formulate_mode(Mode, ModeStmt):-
  Mode =.. [ModeF,O1,VarNames],
  formulate_mode(ModeF, O1, VarNames, ModeStmt).

formulate_mode(select, O1, VarNames, SelectStmt):- !,
  formulate_vars(VarNames, Vars),
  (
    option(distinct(true), O1, false)
  ->
    L = ['SELECT','DISTINCT'|Vars]
  ;
    L = ['SELECT'|Vars]
  ),
  atomic_list_concat(L, ' ', SelectStmt).

%! formulate_prefix(+Prefix:atom, PrefixStatement:atom) is det.
% Formulates a SPARQL prefix statement.
%
% @arg Prefix The atomic name of a prefix that is registered
%        in [sparql_db].
% @arg Statement
%
% @throws existence_error in case the given SPARQL prefix is not registered.

formulate_prefix(Prefix, PrefixStmt):-
  sparql_current_prefix(Prefix, IRI), !,
  format(atom(PrefixStmt), 'PREFIX ~w: <~w>', [Prefix,IRI]).
formulate_prefix(Prefix, _PrefixStmt):-
  existence_error('SPARQL prefix', Prefix).

formulate_prefixes(Prefixes, PrefixStmts2):-
  maplist(formulate_prefix, Prefixes, PrefixStmts1),
  atomic_list_concat(PrefixStmts1, '\n\c', PrefixStmts2).

%! formulate_sparql(
%!   ?Graph:compound,
%!   +Prefixes:list(atom),
%!   +Mode:compound,
%!   +Where:atom,
%!   ?Extra:compound,
%!   -Query:atom
%! ) is det.
% Formulate a SPARQL query, build out of the given components.
%
% The following modes are supported:
%    * `select`
%
% @arg Graph A compound term of the form =|default_graph(+Graph:url)|=.
% @arg Prefixes A list of atomic prefix names, registered as prefix/4.
% @arg Mode A compound term. The functor is a mode,
%        the argument is a list of SPARQL variable names (for `SELECT`).
% @arg Where An atomic WHERE-statement.
% @arg Extra A compound term.
%        Either =|limit(+Options:list(nvpair),+Limit:positive_integer)|=
%        or =|sort_by(+Options:list(nvpair),+VarNames:list(atom))|=.
% @arg Query An atomic SPARQL query.

formulate_sparql(G, Prefixes, Mode, Where, Extra, Query):-
  (
    var(G)
  ->
    Stmts1 = []
  ;
    formulate_graph(G, GStmt),
    Stmts1 = [GStmt]
  ),
  formulate_prefixes(Prefixes, PrefixStmts),
  formulate_mode(Mode, ModeStmt),
  formulate_where(Where, WhereStmt),
  Stmts2 = [PrefixStmts,ModeStmt,WhereStmt],
  (
    var(Extra)
  ->
    Stmts3 = []
  ;
    formulate_extra(Extra, ExtraStmt),
    Stmts3 = [ExtraStmt]
  ),
  append([Stmts1,Stmts2,Stmts3], Stmts),
  atomic_list_concat(Stmts, '\n', Query).

formulate_var(VarName, Var):-
  format(atom(Var), '?~w', [VarName]).

formulate_vars('*', '*'):- !.
formulate_vars(VarNames, Vars):-
  is_list(VarNames),
  maplist(formulate_var, VarNames, Vars).

formulate_where(Stmts, WhereStmt):-
  atomic_list_concat(Stmts, '\n  ', Stmts1),
  format(atom(WhereStmt), 'WHERE {\n  ~w\n}', [Stmts1]).
