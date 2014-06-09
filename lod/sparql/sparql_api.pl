:- module(
  sparql_api,
  [
    sparql_ask/4, % +Endpoint:atom
                  % ?Regime:oneof([owl])
                  % +Prefixes:list(atom)
                  % +Bbps:or([compound,list(compound)])
    sparql_select/10 % +Endpoint:atom
                     % ?Regime:oneof([owl])
                     % +Prefixes:list(atom)
                     % +Distinct:boolean
                     % +Variables:list(atom)
                     % +BGPs:or([compound,list(compound)])
                     % ?Limit:or([nonneg,oneof([inf])])
                     % ?Offset:nonneg
                     % ?Order:pair(oneof([asc]),list(atom))
                     % -Rows:list(list)
  ]
).

/** <module> SPARQL API

High-level API for making SPARQL queries.

@author Wouter Beek
@version 2014/06
*/

:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_ext)).



sparql_ask(Endpoint, Regime, Prefixes, Bgps):-
  phrase(sparql_formulate_ask(Regime, _, Prefixes, Bgps), Query),
  sparql_query(Endpoint, Query, _, true).


sparql_select(Endpoint, Regime, Prefixes, Distinct, Variables, Bgps, Limit,
    Offset, Order, Rows
):-
  phrase(
    sparql_formulate(Regime, _, Prefixes, select, Distinct, Variables, Bgps,
        Limit, Offset, Order
    ),
    Query
  ),
  sparql_query(Endpoint, Query, _, Rows).

