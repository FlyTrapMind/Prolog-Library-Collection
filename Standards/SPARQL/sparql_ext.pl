:- module(
  sparql_ext,
  [
% QUERY FORMULATION
    formulate_sparql/5, % +Prefixes:list(atom)
                        % +Select:atom
                        % +Where:atom
                        % +Limit:integer
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
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09
*/

:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(library(semweb/sparql_client)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(describe_resource(+,r,-)).

:- dynamic(sparql_prefix/2).
:- dynamic(sparql_remote/4).

:- nodebug(sparql).



% QUERY FORMULATION %

formulate_limit(Limit, LimitStatement):-
  integer(Limit), !,
  format(atom(LimitStatement), 'LIMIT ~w', [Limit]).

%! formulate_prefix(+Prefix:atom, +URL:atom, -SPARQL_Prefix:atom) is det.
% Returns the SPARQL prefix statement for the assigning the given URL
% to the given prefix shorthand.

formulate_prefix(Prefix, URL, SPARQL_Prefix):-
  format(
    atom(SPARQL_Prefix),
    'PREFIX ~w: <~w>',
    [Prefix, URL]
  ).

formulate_prefixes(Prefixes, PrefixStatements):-
  setoff(
    PrefixStatement,
    (
      member(Prefix, Prefixes),
      sparql_prefix(Prefix, URL),
      formulate_prefix(Prefix, URL, PrefixStatement)
    ),
    PrefixStatements0
  ),
  atomic_list_concat(PrefixStatements0, '\n\c', PrefixStatements).

formulate_select(SelectStatement, SelectStatement).

%! formulate_sparql(
%!   +Prefixes:list(atom),
%!   +Select:atom,
%!   +Where:atom,
%!   +Limit:integer,
%!   -Query:atom
%! ) is det.
% Formulate a SPARQL query, build out of the given components.
%
% @param Prefixes A list of atomic prefix names, registered as prefix/4.
% @param Select An atomic SELECT-statements.
% @param Where An atomic WHERE-statement.
% @param Limit An integer representing the maximum number of results.
% @param Query An atomic SPARQL query.

formulate_sparql(Prefixes, Select, Where, Limit, Query):-
  formulate_prefixes(Prefixes, PrefixStatements),
  formulate_select(Select, SelectStatement),
  formulate_where(Where, WhereStatement),
  (
    Limit == 0
  ->
    Statements = [PrefixStatements, SelectStatement, WhereStatement]
  ;
    formulate_limit(Limit, LimitStatement),
    Statements =
        [PrefixStatements,SelectStatement,WhereStatement,LimitStatement]
  ),
  atomic_list_concat(Statements, '\n', Query).

formulate_where(Statements, WhereStatement):-
  atomic_list_concat(Statements, '\n', Statements1),
  format(atom(WhereStatement), 'WHERE {\n~w\n}', [Statements1]).



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
    [],
    'SELECT DISTINCT ?p ?o',
    [Where],
    0,
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

