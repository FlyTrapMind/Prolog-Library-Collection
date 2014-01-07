:- module(
  sparql_ext,
  [
    'SPARQL_assert'/4, % +Options:list(nvpair)
                       % +Remote:atom
                       % +Resource:iri
                       % +Graph:atom
    'SPARQL_describe'/4, % +Options:list(nvpair)
                         % +Remote:atom
                         % +Resource:uri
                         % -PO_Pairs:list(list(or([bnode,iri,literal])))
    'SPARQL_enqueue'/4, % +Remote:atom
                        % +Query:atom
                        % -VarNames:list
                        % -Results:list
    'SPARQL_find'/3, % +Remote:atom
                     % +SearchTerm:or([atom,iri])
                     % -Resource:iri
    'SPARQL_query'/4, % +Remote:atom
                      % +Query:atom
                      % -VarNames:list
                      % -Results:list
    'SPARQL_query_sameAs'/3 % +Remote:atom
                            % +Resource:iri
                            % -IdenticalResources:ordset
  ]
).

/** <module> SPARQL extensions

Predicates for formulating and executing SPARQL queries.

# SPARQL 1.1 Query Language

`true`, `false` are `xsd:boolean`.
Numbers with `e` are `xsd:double`.
Numbers with `.` are `xsd:decimal`.
Integers are `xsd:integer`.

Blank nodes are denotated by `_:LABEL`.

Predicate-object lists `;`

Object-lists `,`

`()` is `rdf:nil`.
`a` is `rdf:type`.

Alternatives `UNION`.

~~~{.sparql}
FILTER NOT EXISTS { pattern }
FILTER EXISTS { pattern }
{ pattern } MINUS { pattern }
~~~

~~~{.sparql}
PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
PREFIX  ns:  <http://example.org/ns#>
SELECT  ?title ?price
WHERE   { ?x ns:price ?price .
          FILTER (?price < 30.5)
          ?x dc:title ?title . }
~~~

Count the number of employees in each department:
~~~{.sparql}
select distinct ?dept (count(?emp) as ?count) where {
  ?dept a f:dept.
  ?emp f:Dept ?dept.
} group by ?dept
~~~

# Sample query

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

:- use_module(generics(list_ext)).
:- use_module(generics(row_ext)).
:- use_module(generics(typecheck)).
:- use_module(graph_theory(graph_closure)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/sparql_client)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(xml(xml_namespace)).

% owl
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- sparql_add_prefix(owl).

:- rdf_meta('SPARQL_describe'(+,r,-)).
:- rdf_meta('SPARQL_query_sameAs'(+,r,-)).

% @tbd Only in debug mode.
% @tbd How do we know this is the port?
:- sparql_add_remote(localhost, localhost, 5000, '/sparql/').

:- debug('SPARQL_ext').



%! 'SPARQL_assert'(
%!   +Options:list(nvpair),
%!   +Remote:atom,
%!   +Resource:iri,
%!   +Graph:atom
%! ) is det.

'SPARQL_assert'(O1, Remote, Resource, G):-
  'SPARQL_describe'(O1, Remote, Resource, PO_Pairs),
  forall(
    member([P,O], PO_Pairs),
    rdf_assert(Resource, P, O, G)
  ).



%! 'SPARQL_describe'(
%!   +Options:list(nvpair),
%!   +Remote:atom,
%!   +Resource:iri,
%!   -PO_Pairs:list(list(or([bnode,iri,literal])))
%! ) is det.
% Returns a depth-1 description of the given resource
% in terms of predicate-object rows.
%
% @tbd Make the depth of the description a parameter.

'SPARQL_describe'(O1, Remote, Resource, PO_Pairs):-
  option(closed_under_identity(true), O1, true), !,
  'SPARQL_query_sameAs'(Remote, Resource, Resources1),
  maplist('_SPARQL_describe'(Remote), Resources1, Resources2),
  ord_union(Resources2, PO_Pairs).
'SPARQL_describe'(_O1, Remote, Resource, PO_Pairs):-
  '_SPARQL_describe'(Remote, Resource, PO_Pairs).

'_SPARQL_describe'(Remote, Resource, PO_Pairs):-
  format(atom(Where), '  <~w> ?p ?o .', [Resource]),
  formulate_sparql(
    _Graph,
    [],
    select([distinct(true)],[p,o]),
    [Where],
    _Extra,
    Query
  ),
  'SPARQL_enqueue'(Remote, Query, _VarNames, Rows),
  rows_to_lists(Rows, PO_Pairs),

  % DEB
  (
    PO_Pairs \== [], !
  ;
    debug('SPARQL_ext', 'Empty results for describing resource ~w.', [Resource])
  ).



%! 'SPARQL_enqueue'(
%!   +Remote:atom,
%!   +Query:atom,
%!   -VarNames:list,
%!   -Results:list
%! ) is det.
% @error =|existence_error(url,URL)|= with context
%        =|context(_, status(509, 'Bandwidth Limit Exceeded'))|=

'SPARQL_enqueue'(Remote, Query, VarNames, Results):-
  catch(
    'SPARQL_query'(Remote, Query, VarNames, Results),
    Exception,
    (
      debug('SPARQL_ext', 'EXCEPTION', [Exception]),
      sleep(10),
      'SPARQL_enqueue'(Remote, Query, VarNames, Results)
    )
  ).



%! 'SPARQL_find'(
%!   +Remote:atom,
%!   +SearchTerm:or([atom,iri]),
%!   -Resource:iri
%! ) is det.
% Returns the resource that best fits the given search term.
%
% If the search term is itself a concept, then this is returned.
% Otherwise, the remote is searched for a resource that is labeled with
%  the given search term.
%
% @arg Remote
% @arg SearchTerm
% @arg Resource

'SPARQL_find'(Remote, Resource, Resource):-
  is_uri(Resource),
  % @tbd This can be done more efficiently by just looking for
  %      the first triple.
  'SPARQL_describe'([closed_under_identity(false)], Remote, Resource, PO_Pairs),
  PO_Pairs \== [], !.
'SPARQL_find'(Remote, SearchTerm, Resource):-
  Where1 = '?resource rdfs:label ?label .',
  format(atom(Where2), 'FILTER regex(?label, "^~w", "i")', [SearchTerm]),
  Where = [Where1,Where2],
  formulate_sparql(
    _Graph,
    [rdfs],
    select([distinct(true)],[resource]),
    Where,
    _Extra,
    Query
  ),
  'SPARQL_enqueue'(Remote, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug('SPARQL_ext', 'Could not find a resource for \'~w\'.', [SearchTerm]),
    fail
  ;
    first(Resources, row(Resource))
  ).



%! 'SPARQL_query'(
%!   +Remote:atom,
%!   +Query:atom,
%!   -VarNames:list,
%!   -Results:list
%! ) is det.
% Simply performs a SPARQL query (no additional options, closures).

'SPARQL_query'(Remote, Query, VarNames, Results):-
  once(sparql_current_remote(Remote, Host, Port, Path)),
  O1 = [host(Host),path(Path),variable_names(VarNames)],
  (
    Port == default
  ->
    O2 = O1
  ;
    merge_options([port(Port)], O1, O2)
  ),
  findall(
    Result,
    sparql_query(Query, Result, O2),
    Results
  ).



%! 'SPARQL_query_sameAs'(
%!   +Remote:atom,
%!   +Resource:uri,
%!   -IdenticalResources:ordset
%! ) is det.
% @arg Remote The atomic name of a registered SPARQL remote.
% @arg Resource The URI of a resource.
% @arg IdenticalResources An ordered set of identical resources.

'SPARQL_query_sameAs'(Remote, Resource, Resources2):-
  graph_closure([Resource], '_SPARQL_query_sameAs'(Remote), Resources1),
  ord_add_element(Resources1, Resource, Resources2).

'_SPARQL_query_sameAs'(Remote, Resource, Resources2):-
  format(atom(Where), '  <~w> owl:sameAs ?x .', [Resource]),
  formulate_sparql(
    _Graph,
    [owl],
    select([distinct(true)],[x]),
    [Where],
    _Extra,
    Query
  ),
  'SPARQL_enqueue'(Remote, Query, _VarNames, Resources1),
  rows_to_ord_set(Resources1, Resources2).

