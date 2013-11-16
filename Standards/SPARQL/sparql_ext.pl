:- module(
  sparql_ext,
  [
% QUERY FORMULATION
    formulate_sparql/6, % +Graph:compound
                        % +Prefixes:list(atom)
                        % +Select:compound
                        % +Where:atom
                        % +Extra:compound
                        % -Query:atom

% QUERY PART REGISTRATION
    register_sparql_prefix/1, % +Prefix:atom
    register_sparql_prefix/2, % +Prefix:atom
                              % +URI:uri
    register_sparql_remote/4, % +Remote:atom
                              % +Server:atom
                              % +Port:oneof(default,integer)
                              % +Path:atom
    sparql_remote/4, % ?Remote:atom
                     % ?Server:atom
                     % ?Port:oneof(default,integer)
                     % ?Path:atom

% QUERY RESULTS
    sparql_rows_to_lists/2, % +SPARQL_Rows:list(compound)
                            % -Lists:list(list)

% QUERYING
    describe_resource/3, % +Remote:atom
                         % +Resource:uri
                         % -Rows:list(row)
    enqueue_sparql/4, % +Remote:atom
                      % +Query:atom
                      % -VarNames:list
                      % -Results:list
    query_sparql/4 % +Remote:atom
                   % +Query:atom
                   % -VarNames:list
                   % -Results:list
  ]
).

/** <module> SPARQL

Predicates for formulating and executing SPARQL queries.

## Sample query

~~~{.sparql}
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT *
WHERE { ?s rdf:type rdfs:Class }
LIMIT 10
~~~

# Warnings

When the results from a SPARQL endpoint are in XML/RDF without
proper end tags, then the following warnings will be given by
the XML parser:

~~~{.txt}
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "uri"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "binding"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "result"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "results"
Warning: [Thread t03] SGML2PL(xmlns): []:216: Inserted omitted end-tag for "sparql"
~~~

@author Wouter Beek
@see SPARQL 1.1 Recommendation 2013/03
     http://www.w3.org/TR/2013/REC-sparql11-overview-20130321/
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(library(semweb/sparql_client)).
:- use_module(rdf(rdf_name)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(describe_resource(+,r,-)).

:- dynamic(sparql_prefix/2).
:- dynamic(sparql_remote/4).

:- nodebug(sparql).



% QUERY FORMULATION %

formulate_extra(Extra, ExtraStmt):-
  Extra =.. [ExtraF,O1,Arg],
  formulate_extra(ExtraF, O1, Arg, ExtraStmt).

formulate_extra(limit, _O1, Max, LimitStmt):- !,
  positive_integer(Max),
  format(atom(LimitStmt), 'LIMIT ~w', [Max]).
formulate_extra(order_by, O1, VarNames, OrderByStmt):-
  formulate_vars(VarNames, Vars1),
  atomic_list_concat(Vars1, ' ', Vars2),
  option(order(Order), O1, asc),
  format(atom(OrderByStmt), 'ORDER BY ~w(~w)', [Order,Vars2]).

formulate_graph(default_graph(G), GStmt):-
  format(atom(GStmt), '# Default graph (located at ~w)', [G]).

formulate_limit(Limit, LimitStmt):-
  positive_integer(Limit),
  format(atom(LimitStmt), 'LIMIT ~w', [Limit]).

formulate_mode(Mode, ModeStmt):-
  Mode =.. [ModeF,O1,VarNames],
  formulate_mode(ModeF, O1, VarNames, ModeStmt).

formulate_mode(select, O1, VarNames, SelectStmt):-
  formulate_vars(VarNames, Vars),
  (
    option(distinct(true), O1, false)
  ->
    L = ['SELECT','DISTINCT'|Vars]
  ;
    L = ['SELECT'|Vars]
  ),
  atomic_list_concat(L, ' ', SelectStmt).

%! formulate_prefix(+Prefix:atom, +URL:atom, -SPARQL_Prefix:atom) is det.
% Returns the SPARQL prefix statement for the assigning the given URL
% to the given prefix shorthand.

formulate_prefix(Prefix, URL, SPARQL_Prefix):-
  format(atom(SPARQL_Prefix), 'PREFIX ~w: <~w>', [Prefix,URL]).

formulate_prefixes(Prefixes, PrefixStmts):-
  % Make sure are prefix names are registered with a URL.
  is_list(Prefixes),
  maplist(sparql_prefix, Prefixes, _URLs),

  setoff(
    PrefixStmt,
    (
      member(Prefix, Prefixes),
      sparql_prefix(Prefix, URL),
      formulate_prefix(Prefix, URL, PrefixStmt)
    ),
    PrefixStmts0
  ),
  atomic_list_concat(PrefixStmts0, '\n\c', PrefixStmts).

formulate_select(SelectStmt, SelectStmt).

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
% @param Graph A compound term of the form =|default_graph(+Graph:url)|=.
% @param Prefixes A list of atomic prefix names, registered as prefix/4.
% @param Mode A compound term. The functor is a mode,
%        the argument is a list of SPARQL variable names (for `SELECT`).
% @param Where An atomic WHERE-statement.
% @param Extra A compound term.
%        Either =|limit(+Options:list(nvpair),+Limit:positive_integer)|=
%        or =|sort_by(+Options:list(nvpair),+VarNames:list(atom))|=.
% @param Query An atomic SPARQL query.

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



% QUERY PART REGISTRATION %

%! register_sparql_prefix(+Prefix:atom) is det.
% @see register_sparql_prefix/2

register_sparql_prefix(Prefix):-
  once(xml_current_namespace(Prefix, URI)),
  register_sparql_prefix(Prefix, URI).

%! register_sparql_prefix(+Prefix:atom, +IRI:iri) is det.
% XML namespace registrations and SPARQL predix registrations are separated,
% because they may not be the same thing.
%
% @tbd Check the SPARQL 1.1 standard whether they are indeed different.

register_sparql_prefix(Prefix, URI):-
  sparql_prefix(Prefix, URI), !.
register_sparql_prefix(Prefix, URI):-
  assert(sparql_prefix(Prefix, URI)).

register_sparql_remote(Remote, Server, Port, Path):-
  sparql_remote(Remote, Server, Port, Path), !.
register_sparql_remote(Remote, Server, Port, Path):-
  assert(sparql_remote(Remote, Server, Port, Path)).
:- register_sparql_remote(localhost, localhost, 5000, '/sparql/').



% QUERY RESULTS %

sparql_rows_to_lists([], []):- !.
sparql_rows_to_lists([H1|T1], [H2|T2]):-
  H1 =.. [row|H2],
  sparql_rows_to_lists(T1, T2).



% QUERYING %

%! describe_resource(
%!   +Remote:atom,
%!   +Resource:iri,
%!   -Rows:list(compound)
%! ) is det.
% Returns a depth-1 description of the given resource
% in terms of predicate-object rows.
%
% @tbd Make the depth of the description a parameter.

describe_resource(Remote, Resource, Rows):-
  format(atom(Where), '  <~w> ?p ?o .', [Resource]),
  formulate_sparql(
    _Graph,
    [],
    select([distinct(true)],[p,o]),
    [Where],
    _Extra,
    Query
  ),
  enqueue_sparql(Remote, Query, _VarNames, Rows),

  % DEB
  ( Rows \== [], !
  ; debug(sparql_ext, 'Empty results for DESCRIBE ~w.', [Resource])).

%! enqueue_sparql(
%!   +Remote:atom,
%!   +Query:atom,
%!   -VarNames:list,
%!   -Results:list
%! ) is det.
% @error =|existence_error(url,URL)|= with context
%        =|context(_, status(509, 'Bandwidth Limit Exceeded'))|=

enqueue_sparql(Remote, Query, VarNames, Results):-
  catch(
    query_sparql(Remote, Query, VarNames, Results),
    Exception,
    (
      debug(sparql, 'EXCEPTION', [Exception]),
      sleep(10),
      enqueue_sparql(Remote, Query, VarNames, Results)
    )
  ).

%! query_sparql(
%!   +Remote:atom,
%!   +Query:atom,
%!   -VarNames:list,
%!   -Results:list
%! ) is det.

query_sparql(Remote, Query, VarNames, Results):-
  once(sparql_remote(Remote, Host, Port, Path)),
  (
    Port == default
  ->
    PortOption = []
  ;
    PortOption = [port(Port)]
  ),
  findall(
    Result,
    sparql_query(
      Query,
      Result,
      [host(Host), path(Path), variable_names(VarNames) | PortOption]
    ),
    Results
  ).

