:- module(
  sparql_ext,
  [
    sparql_query/4, % +Endpoint:atom
                    % +Query:atom
                    % -VarNames:list
                    % -Result:or([boolean,list(list)])
    sparql_query/5, % +Endpoint:atom
                    % +Query:atom
                    % -VarNames:list
                    % -Result:or([boolean,list(list)])
                    % +Attempts:or([oneof([inf]),positive_integer])
    sparql_query_sameas/3 % +Endpoint:atom
                          % +Resource:iri
                          % -IdenticalResources:ordset
  ]
).

/** <module> SPARQL extensions

Predicates for executing SPARQL queries.

@author Wouter Beek
@see SPARQL 1.1 Recommendation 2013/03
     http://www.w3.org/TR/2013/REC-sparql11-overview-20130321/
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01
         2014/04, 2014/06
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(uri)).

:- use_module(generics(codes_ext)).
:- use_module(generics(row_ext)).
:- use_module(generics(typecheck)).
:- use_module(http(http_goal)).
:- use_module(math(math_ext)).
:- use_module(sparql(sparql_api)).
:- use_module(sparql(sparql_db)).
:- use_module(xml(xml_namespace)).

% OWL
:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').



%! sparql_query(
%!   +Endpoint:atom,
%!   +Query:atom,
%!   -VarNames:list,
%!   -Result:or([boolean,list(list)])
%! ) is det.

sparql_query(Endpoint, Query, VarNames, Result):-
  sparql_query(Endpoint, Query, VarNames, Result, 1).

%! sparql_query(
%!   +Endpoint:atom,
%!   +Query:atom,
%!   -VarNames:list,
%!   -Result:or([boolean,list(list)]),
%!   +Attempts:or([oneof([inf]),positive_integer])
%! ) is det.
%
% @error =|existence_error(url,URL)|= with context
%        =|context(_, status(509, 'Bandwidth Limit Exceeded'))|=

sparql_query(_, _, [], [], 0):- !.
sparql_query(Endpoint, Query, VarNames, Result, Attempts):-
  catch(
    sparql_query_no_catch(Endpoint, Query, VarNames, Result),
    E,
    http_catcher(E, Endpoint, Query, VarNames, Result, Attempts)
  ).

http_catcher(exit, _, _, _, _, _).
http_catcher(E, _, _, _, _, 1):- !,
  throw(E).
http_catcher(_, Endpoint, Query, VarNames, Results, Attempts1):-
  count_down(Attempts1, Attempts2),
  sparql_query(Endpoint, Query, VarNames, Results, Attempts2).


sparql_query_no_catch(Endpoint, Query1, VarNames, Result2):-
  % Debug message.
  atomic_codes(Query2, Query1),
  debug(sparql_ext, '~w', [Query2]),

  % Options are based on the given endpoint registration.
  once(sparql_endpoint(Endpoint, query, Location)),
  uri_components(Location, uri_components(_,Authority,Path,_,_)),
  uri_authority_components(Authority, uri_authority(_,_,Host,Port)),
  Options1 = [host(Host),timeout(1),path(Path),variable_names(VarNames)],
  (
    nonvar(Port)
  ->
    merge_options([port(Port)], Options1, Options2)
  ;
    Options2 = Options1
  ),

  % The actual SPARQL query.
  sparql_query(Query2, Result1, Options2),

  % Result post-processing.
  (
    is_of_type(boolean, Result1)
  ->
    % For queries of kind ASK.
    Result2 = Result1
  ;
    % For queries of kind SELECT.
    maplist(row_to_list, Result1, Result2)
  ).


%! sparql_query_sameas(
%!   +Endpoint:atom,
%!   +Resource:uri,
%!   -IdenticalResources:ordset
%! ) is det.
% @arg Remote The atomic name of a registered SPARQL remote.
% @arg Resource The URI of a resource.
% @arg IdenticalResources An ordered set of identical resources.

sparql_query_sameas(Endpoint, Resource, Resources2):-
  sparql_select(Endpoint, owl, [owl], true, [x],
      [rdf(iri(Resource),owl:sameAs,var(x))], inf, _, _, Rows),
  rows_to_resources(Rows, Resources1),
  ord_add_element(Resources1, Resource, Resources2).

