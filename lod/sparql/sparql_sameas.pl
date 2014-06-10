:- module(
  sparql_sameas,
  [
    sparql_query_sameas/3 % +Endpoint:atom
                          % +Resource:iri
                          % -IdenticalResources:ordset
  ]
).

/** <module> SPARQL sameas

Close SPARQL queries under identity.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01
         2014/04, 2014/06
*/

:- use_module(library(ordsets)).

:- use_module(generics(row_ext)).
:- use_module(sparql(sparql_api)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').



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

