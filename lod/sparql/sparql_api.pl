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
                      % -Rows:list(list)
    sparql_update/3 % +Endpoint:atom
                    % +Triples:list(list(or([bnode,iri,literal])))
                    % +Options:list(nvpair)
  ]
).

/** <module> SPARQL API

High-level API for making SPARQL queries.

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/http_client)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).

:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).

:- use_module(plRdf_ser(rdf_ntriples_write)).



%! sparql_ask(
%!   +Endpoint:atom,
%!   ?Regime:oneof([owl]),
%!   +Prefixes:list(atom),
%!   +Bbps:or([compound,list(compound)])
%! ) is semidet.

sparql_ask(Endpoint, Regime, Prefixes, Bgps):-
  % Construct the query.
  phrase(sparql_formulate_ask(Regime, _, Prefixes, Bgps), Query),
  
  % Debug message.
  atom_codes(Atom, Query),
  debug(sparql_api, '~w', [Atom]),
  
  % Execute the query.
  sparql_query(Endpoint, Query, _, true).


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
  Rows
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
    Query
  ),
  
  atom_codes(Atom, Query),
  debug(sparql_api, '~w', Atom),
  
  sparql_query(Endpoint, Query, _, Rows).


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
  with_output_to(codes(Content), sparql_insert_data([])),
  
  % Debug message.
  atom_codes(Atom, Content),
  debug(sparql_api, '~w', [Atom]),
  
  % Set options.
  sparql_endpoint(Endpoint, update, Location),
  merge_options(
    Options1,
    [request_header('Accept'='application/json')],
    Options2
  ),
  
  % The actual SPARQL Update request.
  http_post(
    Location,
    codes('application/sparql-update', Content),
    Reply,
    Options2
  ),
  
  % Debug message showing the HTTP POST reply.
  debug(sparql_api, '~w', [Reply]).

assert_triple([S,P,O]):-
  rdf_assert(S, P, O).

sparql_insert_data(Options):-
  writeln('INSERT DATA {'),
  rdf_ntriples_write(Options),
  writeln('}').

