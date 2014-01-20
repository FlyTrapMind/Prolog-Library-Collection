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
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(typecheck)).
:- use_module(graph_theory(graph_closure)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/sparql_client)).
:- use_module(sparql(row_ext)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(xml(xml_namespace)).

% OWL
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- sparql_add_prefix(owl).

:- rdf_meta('SPARQL_describe'(+,r,-)).
:- rdf_meta('SPARQL_query_sameAs'(+,r,-)).

:- if(predicate_property(user:debug_project, visible)).
  :-
    once(http_server_property(Port, _)),
    sparql_add_remote(localhost, localhost, Port, '/sparql/').
:- endif.



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
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [],
      select,
      true,
      [p,o],
      [rdf(iri(Resource), var(p), var(o))],
      inf,
      _
    ),
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
  must_be(iri, Resource),
  % @tbd This can be done more efficiently by just looking for
  %      the first triple.
  'SPARQL_describe'([closed_under_identity(false)], Remote, Resource, PO_Pairs),
  PO_Pairs \== [], !.
'SPARQL_find'(Remote, SearchTerm, Resource):-
  phrase(
    'SPARQL_formulate'(
      _,
      _,
      [rdfs],
      select,
      true,
      [resource],
      [
        rdf(var(resource), rdfs:label, var(label)),
        filter(regex(var(label), at_start(SearchTerm), [case_insensitive]))
      ],
      inf,
      _
    ),
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

'SPARQL_query'(Remote, Query1, VarNames, Results):-
  to_atom(Query1, Query2),
  debug(sparl_ext, '~w', [Query2]),
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
    sparql_query(Query2, Result, O2),
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

'SPARQL_query_sameAs'(Remote, Resource, Resources3):-
  phrase(
    'SPARQL_formulate'(
      owl,
      _,
      [owl],
      select,
      true,
      [x],
      [rdf(iri(Resource), owl:sameAs, var(x))],
      inf,
      _
    ),
    Query
  ),
  'SPARQL_enqueue'(Remote, Query, _VarNames, Resources1),
  rows_to_ord_set(Resources1, Resources2),
  ord_add_element(Resources2, Resource, Resources3).

