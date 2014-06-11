:- module(
  sparql_api,
  [
    sparql_ask/5, % +Endpoint:atom
                  % ?Regime:oneof([owl])
                  % +Prefixes:list(atom)
                  % +Bbps:or([compound,list(compound)])
                  % +Options:list(nvpair)
    sparql_select/11, % +Endpoint:atom
                      % ?Regime:oneof([owl])
                      % +Prefixes:list(atom)
                      % +Distinct:boolean
                      % +Variables:list(atom)
                      % +BGPs:or([compound,list(compound)])
                      % ?Limit:or([nonneg,oneof([inf])])
                      % ?Offset:nonneg
                      % ?Order:pair(oneof([asc]),list(atom))
                      % -Result:list(list)
                      % +Options:list(nvpair)
    sparql_update/3 % +Endpoint:atom
                    % +Triples:list(list(or([bnode,iri,literal])))
                    % +Options:list(nvpair)
  ]
).

/** <module> SPARQL API

High-level API for making SPARQL queries.

@author Wouter Beek
@see SPARQL 1.1 Recommendation 2013/03
     http://www.w3.org/TR/2013/REC-sparql11-overview-20130321/
@version 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/sparql_client)).

:- use_module(generics(row_ext)).
:- use_module(generics(uri_search)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).

:- use_module(plRdf_ser(rdf_ntriples_write)).



%! sparql_ask(
%!   +Endpoint:atom,
%!   ?Regime:oneof([owl]),
%!   +Prefixes:list(atom),
%!   +Bbps:or([compound,list(compound)]),
%!   +Options:list(nvpair)
%! ) is semidet.

sparql_ask(Endpoint, Regime, Prefixes, Bgps, Options):-
  % Construct the query.
  phrase(sparql_formulate_ask(Regime, _, Prefixes, Bgps), Query1),

  % Debug message.
  atom_codes(Query2, Query1),
  debug(sparql_api, '~w', [Query2]),

  % Execute the ASK query.
  sparql_query(Endpoint, Query2, true, Options).


%! sparql_select(
%!   +Endpoint:atom,
%!   ?Regime:oneof([owl]),
%!   +Prefixes:list(atom),
%!   +Distinct:boolean,
%!   +Variables:list(atom),
%!   +BGPs:or([compound,list(compound)]),
%!   ?Limit:or([nonneg,oneof([inf])]),
%!   ?Offset:nonneg,
%!   ?Order:pair(oneof([asc]),list(atom)),
%!   -Result:list(list),
%!   +Options:list(nvpair)
%! ) is det.

sparql_select(
  Endpoint,
  Regime,
  Prefixes,
  Distinct,
  Variables,
  Bgps,
  Limit,
  Offset,
  Order,
  Result,
  Options
):-
  phrase(
    sparql_formulate(
      Regime,
      _,
      Prefixes,
      select,
      Distinct,
      Variables,
      Bgps,
      Limit,
      Offset,
      Order
    ),
    Query1
  ),

  % Debug message.
  atom_codes(Query2, Query1),
  debug(sparql_api, '~w', Query2),

  % Execute the SELECT query.
  findall(
    Row,
    sparql_query(Endpoint, Query2, Row, Options),
    Rows
  ),
  Rows = Result.


%! sparql_update(
%!   +Endpoint:atom,
%!   +Triples:list(list(or([bnode,iri,literal]))),
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   - =|update_method(+Method:oneof([direct,url_encoded]))|=
%     Default: `direct`.
%   - Other options are passed on to http_post/4.

sparql_update(Endpoint, Triples, Options):-
  rdf_transaction(
    sparql_update0(Endpoint, Triples, Options),
    _,
    [snapshot(true)]
  ).

sparql_update0(Endpoint, Triples, Options1):-
  % Construct the contents of the request message.
  maplist(assert_triple, Triples),
  with_output_to(codes(Content1), sparql_insert_data([])),

  % Debug message.
  atom_codes(Content2, Content1),
  debug(sparql_api, '~w', [Content2]),

  % Set options.
  once(sparql_endpoint(Endpoint, update, Url)),
  merge_options(Options1, [request_header('Accept'='*/*')], Options2),

  % The actual SPARQL Update request.
  select_option(update_method(Method), Options2, Options3, direct),
  sparql_update_post(Url, Content1, Method, Options3).

% Method: URL encoded.
sparql_update_post(Url, Query, url_encoded, Options1):- !,
  graph_search_parameters(Options1, GraphParams, Options2),
  uri_query_components(Search1, [query=Query|GraphParams]),
  atom_codes(Search1, Search2),
  http_post(
    Url,
    codes('application/x-www-form-urlencoded', Search2),
    Reply,
    Options2
  ),
  debug(sparql_api, '~w', [Reply]).
% Method: direct (default).
sparql_update_post(Url1, Content, url_encoded, Options1):-
  graph_search_parameters(Options1, Search, Options2),
  uri_search_add(Url1, Search, Url2),
  http_post(
    Url2,
    codes('application/sparql-update', Content),
    Reply,
    Options2
  ),
  debug(sparql_api, '~w', [Reply]).



% Helpers

%! assert_triple(+List:list(or([bnode,iri,literal]))) is det.

assert_triple([S,P,O]):-
  rdf_assert(S, P, O).


%! clean_content_type(+DirtyContentType:atom, -CleanContentType:atom) is det.

clean_content_type(Dirty, Clean):-
  sub_atom(Dirty, B, _, _, (;)), !,
  sub_string(Dirty, 0, B, _, Main),
  normalize_space(atom(Clean), Main).
clean_content_type(Clean, Clean).


%! graph_search_parameters(
%!   +Options:list(nvpair),
%!   -SearchParameters:list(nvpair),
%!   -RestOptions:list(nvpair)
%! ) is det.

graph_search_parameters(Options1, Params3, Options3):-
  % Default graph.
  Params1 = [],
  (
    select_option(default_graph(DefaultGraph), Options1, Options2)
  ->
    append(Params1, ['using-graph-uri'=DefaultGraph], Params2)
  ;
    Options2 = Options1,
    Params2 = Params1
  ),

  % Named graphs.
  select_option(named_graphs(NamedGraphs), Options2, Options3, []),
  findall(
    'using-named-graph-uri'=NamedGraph,
    member(NamedGraph, NamedGraphs),
    NamedGraphPairs
  ),
  append(Params2, NamedGraphPairs, Params3).


%! sparql_insert_data(+Options:list(nvpair)) is det.
% Intended to be run from within an rdf_transaction/3 with `snapshot(true)`.

sparql_insert_data(Options):-
  writeln('INSERT DATA {'),
  rdf_ntriples_write(Options),
  writeln('}').


%! sparql_query(
%!   +Endpoint:atom,
%!   +Query:atom,
%!   -Row:compound,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|query_method(+Method:oneof([direct,get,url_encoded]))|=
%     Default: `direct`.
%   * =|variable_names(-VariableNames:list(atom))|=
%   * Other options are passed on to http_open/3.

sparql_query(Endpoint, Query, Row, Options1):-
  once(sparql_endpoint(Endpoint, query, Url1)),
  merge_options(
    [header(content_type,ContentType),variable_names(VariableNames)],
    Options1,
    Options2
  ),
  uri_search_add(Url1, query, Query, Url2),
  http_open(Url2, Read, Options2),
  clean_content_type(ContentType, CleanType),
  sparql_read_reply(CleanType, Read, VariableNames, Row).


%! sparql_read_reply(
%!   +ContentType:atom,
%!   +Read:blob,
%!   -VariableNames:list(atom),
%!   -Result:compound
%! ) is det.

sparql_read_reply('application/rdf+xml', Read, _, Result):- !,
  call_cleanup(
    load_rdf(stream(Read), RDF),
    close(Read)
  ),
  member(Result, RDF).
sparql_read_reply('text/rdf+n3', Read, _, Result):- !,
  call_cleanup(
    rdf_read_turtle(stream(Read), RDF, []),
    close(Read)
  ),
  member(Result, RDF).
% SPARQL 1.1 Query Results XML Format
% @see http://www.w3.org/TR/2013/REC-rdf-sparql-XMLres-20130321/
sparql_read_reply('application/sparql-results+xml', Read, VarNames, Result2):- !,
  call_cleanup(
    sparql_read_xml_result(stream(Read), Result),
    close(Read)
  ),
  varnames(Result1, VarNames),
  xml_result(Result1, Result2).
% SPARQL 1.1 Query Results JSON Format.
% @see http://www.w3.org/TR/sparql11-results-json/
sparql_read_reply('application/sparql-results+json', Read, VarNames, Result):- !,
  call_cleanup(
    sparql_read_json_result(stream(Read), Result),
    close(Read)
  ),
  varnames(Result, VarNames).
sparql_read_reply(Type, Read, _, _):-
  close(Read),
  domain_error(sparql_result_document, Type).


varnames(ask(_), _).
varnames(select(VarTerm, _Rows), VarNames):-
  VarTerm =.. [_|VarNames].


%! xml_result(+Result:compound, -Result:or([boolean,list])) is nondet.

xml_result(ask(Result1), Result2):- !,
  Result1 = Result2.
xml_result(select(_, Rows), Result):-
  % NONDET
  member(Row, Rows),
  row_to_list(Row, Result).

