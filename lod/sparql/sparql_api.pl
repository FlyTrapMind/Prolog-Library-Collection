:- module(
  sparql_api,
  [
    sparql_ask/4, % +Endpoint:atom
                  % ?Regime:oneof([owl])
                  % +Prefixes:list(atom)
                  % +Bbps:or([compound,list(compound)])
    sparql_select/10, % +Endpoint:atom
                      % ?Regime:oneof([owl])
                      % +Prefixes:list(atom)
                      % +Distinct:boolean
                      % +Variables:list(atom)
                      % +BGPs:or([compound,list(compound)])
                      % ?Limit:or([nonneg,oneof([inf])])
                      % ?Offset:nonneg
                      % ?Order:pair(oneof([asc]),list(atom))
                      % -Result:list(list)
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
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/sparql_client)).

:- use_module(generics(row_ext)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).

:- use_module(plRdf_ser(rdf_ntriples_write)).



%! sparql_ask(
%!   +Endpoint:atom,
%!   ?Regime:oneof([owl]),
%!   +Prefixes:list(atom),
%!   +Bbps:or([compound,list(compound)])
%! ) is semidet.

sparql_ask(Endpoint, Regime, Prefixes, Bgps):-
  % Construct the query.
  phrase(sparql_formulate_ask(Regime, _, Prefixes, Bgps), Query1),

  % Debug message.
  atom_codes(Query2, Query1),
  debug(sparql_api, '~w', [Query2]),

  % Execute the ASK query.
  sparql_query_options(Endpoint, UrlOptions, HttpOptions),
  sparql_query2(Query2, true, UrlOptions, HttpOptions).



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
%!   -Rows:list(list)
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
  Result1
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
  sparql_query_options(Endpoint, UrlOptions, HttpOptions),
  findall(
    Row,
    sparql_query2(Query2, Row, UrlOptions, HttpOptions),
    Rows
  ),
  maplist(row_to_list, Rows, Result2),
  Result1 = Result2.


%! sparql_update(
%!   +Endpoint:atom,
%!   +Triples:list(list(or([bnode,iri,literal]))),
%!   +Options:list(nvpair)
%! ) is det.

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
  sparql_endpoint(Endpoint, update, Location),
  merge_options(
    Options1,
    [request_header('Accept'='application/sparql-results+json')],
    Options2
  ),

  % The actual SPARQL Update request.
  http_post(
    Location,
    codes('application/sparql-update', Content1),
    Reply,
    Options2
  ),

  % Debug message showing the HTTP POST reply.
  debug(sparql_api, '~w', [Reply]).



% Helpers

%! assert_triple(+List:list(or([bnode,iri,literal]))) is det.

assert_triple([S,P,O]):-
  rdf_assert(S, P, O).


%! sparql_insert_data(+Options:list(nvpair)) is det.
% Intended to be run from within an rdf_transaction/3 with `snapshot(true)`.

sparql_insert_data(Options):-
  writeln('INSERT DATA {'),
  rdf_ntriples_write(Options),
  writeln('}').


sparql_query2(Query, Row, UrlOptions1, HttpOptions1):-
  sparql_client:sparql_param(host(Host), UrlOptions1, UrlOptions2),
  sparql_client:sparql_param(port(Port), UrlOptions2, UrlOptions3),
  sparql_client:sparql_param(path(Path), UrlOptions3, UrlOptions4),
  select_option(search(Extra), UrlOptions4, UrlOptions5, []),
  select_option(variable_names(VarNames), UrlOptions5, UrlOptions6, _),
  merge_options(
    [header(content_type, ContentType)],
    HttpOptions1,
    HttpOptions2
  ),
  http_open(
    [protocol(http),
     host(Host),
     port(Port),
     path(Path),
     search([query=Query|Extra])
    |UrlOptions6],
    In,
    HttpOptions2
  ),
  sparql_client:plain_content_type(ContentType, CleanType),
  sparql_client:read_reply(CleanType, In, VarNames, Row).


%! sparql_query_options(
%!   +Endpoint:atom,
%!   -UrlOptions:list(nvpair),
%!   -HttpOptions:list(nvpair)
%! ) is det.

sparql_query_options(Endpoint, UrlOptions2, HttpOptions):-
  % Options are based on the given endpoint registration.
  once(sparql_endpoint(Endpoint, query, Location)),
  uri_components(Location, uri_components(_,Authority,Path,_,_)),
  uri_authority_components(Authority, uri_authority(_,_,Host,Port)),
  UrlOptions1 = [host(Host),timeout(1),path(Path)],
  (
    nonvar(Port)
  ->
    merge_options([port(Port)], UrlOptions1, UrlOptions2)
  ;
    UrlOptions2 = UrlOptions1
  ),
  HttpOptions =  [request_header('Accept'='application/sparql-results+xml')].

