:- module(
  sparql_ext,
  [
    describe_resource/3, % +Remote:atom
                         % +Resource:uri
                         % -SPARQL_Results:list(list)
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

/** <module> SPARQL extensions

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
proper end tags, the following warnings will be given by
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
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2013/12
*/

:- use_module(generics(row_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(library(semweb/sparql_client)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).

:- rdf_meta(describe_resource(+,r,-)).

:- sparql_add_remote(localhost, localhost, 5000, '/sparql/').

:- debug(sparql_ext).



%! describe_resource(
%!   +Remote:atom,
%!   +Resource:iri,
%!   -SPARQL_Results:list(list)
%! ) is det.
% Returns a depth-1 description of the given resource
% in terms of predicate-object rows.
%
% @tbd Make the depth of the description a parameter.

describe_resource(Remote, Resource, Results):-
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
  rows_to_lists(Rows, Results),

  % DEB
  (
    Results \== [], !
  ;
    debug(sparql_ext, 'Empty results for describing resource ~w.', [Resource])
  ).

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
      debug(sparql_ext, 'EXCEPTION', [Exception]),
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
  once(sparql_current_remote(Remote, Host, Port, Path)),
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

