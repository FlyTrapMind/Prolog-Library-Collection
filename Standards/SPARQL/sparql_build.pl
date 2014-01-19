:- module(
  sparql_build,
  [
    'SPARQL_formulate'//8 % +DefaultGraph:iri
                          % +Prefixes:list(atom)
                          % +Mode:oneof([select])
                          % +Distinct:boolean
                          % +Variables:list(atom)
                          % +BGPs:list(compound)
                          % +Limit:or([nonneg,oneof([inf])])
                          % +Order:pair(oneof([asc]),list(atom))
  ]
).

/** <module> SPARQL build

DCG for the construction of SPARQL queries.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2014/01
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(xml(xml_namespace)).



bgp([]) --> [].
bgp([filter(Filter)|T]) -->
  "FILTER ",
  filter(Filter),
  bgp(T).
bgp([optional(Optional)|T]) -->
  "  OPTIONAL {\n",
  bgp(Optional),
  "  }\n",
  bgp(T).
bgp([rdf(S,P,O)|T]) -->
  "  ",
  term(S),
  " ",
  term(P),
  " ",
  term(O),
  " .\n",
  bgp(T).

default_graph(DefaultGraph) -->
  "# Default graph (located at ",
  atom(DefaultGraph),
  ")\n".
default_graph(_) --> [].

distinct(true) -->
  " DISTINCT".
distinct(false) --> [].

filter(regex(Arg1,Arg2)) -->
  filter(regex(Arg1,Arg2,[])).
filter(regex(Arg1,Arg2,Flags)) -->
  "REGEX(",
  term(Arg1),
  ",",
  term(Arg2),
  regex_flags(Flags),
  ")".
filter(strends(Arg1,Arg2)) -->
  "STRENDS(",
  term(Arg1),
  ",",
  term(Arg2),
  ")".

limit(inf) --> [].
limit(Limit) -->
  "LIMIT ",
  integer(Limit),
  "\n".
limit(_) --> [].

mode(select) -->
  "SELECT".

order(Criterion-Variables) -->
  "ORDER BY ",
  order_criterion(Criterion),
  bracketed(variables(Variables)),
  "\n".
order(_) --> [].

order_criterion(ascending) -->
  "ASC".

prefix(Prefix) -->
  {xml_current_namespace(Prefix, IRI)},
  "PREFIX ",
  atom(Prefix),
  ": <",
  atom(IRI),
  ">\n".

prefixes([]) --> [].
prefixes([H|T]) -->
  prefix(H),
  prefixes(T).

regex_flags([]) --> [].
regex_flags(Flags) -->
  ",",
  quoted(regex_flags1(Flags)).

regex_flags1([]) --> [].
regex_flags1([case_insensitive|T]) -->
  "i",
  regex_flags1(T).

%! 'SPARQL_formulate'(
%!   +DefaultGraph:iri,
%!   +Prefixes:list(atom),
%!   +Mode:oneof([select]),
%!   +Distinct:boolean,
%!   +Variables:list(atom),
%!   +BGP:list(compound),
%!   +Limit:or([nonneg,oneof([inf])]),
%!   +Order:pair(oneof([asc]),list(atom))
%! ) is det.
%
% # Example
%
% ~~~{.pl}
% 'SPARQL(_, [rdfs], select, true, [class], Where, inf, asc-class)
% ~~~
% With the corresponding SPARQL query:
% ~~~{.sparql}
% PREFIX rdf: <...>
% SELECT DISTINCT ?class
% WHERE {
%   dbpedia:Monkey rdf:type ?x .
%   ?x rdfs:subClassOf* ?class .
% }
% ORDER BY ASC(?class)
% ~~~
%
% @arg DefaultGraph An IRI denoting the default graph to query from.
% @arg Prefixes A list of registered atomic XML prefixes.
% @arg Mode The mode of the SPARQL query.
%      Currently only `select` is supported.
% @arg Distinct Whether the returned results should be distinct or not.
% @arg Variables A list of atomic variable names.
% @arg BGP A list denoting a basic graph pattern.
% @arg Limit Either a positive integer indicating the maximum number of
%      retrieved results, or `inf`.
% @arg Order A pair consisting of the ordering criterion and the variables
%      relative to which ordering takes place.
%      Currently the only supported ordering criterion is `asc` for
%      ascending lexicographically.

'SPARQL_formulate'(
  DefaultGraph,
  Prefixes,
  Mode,
  Distinct,
  Variables,
  BGP,
  Limit,
  Order
) -->
  default_graph(DefaultGraph),
  prefixes(Prefixes),
  mode(Mode),
  distinct(Distinct),
  " ",
  variables(Variables),
  "\n",
  where(BGP),
  limit(Limit),
  order(Order).

term(a) --> !,
  "a".
term(at_start(String)) -->
  double_quote,
  "^",
  atom(String),
  double_quote.
term(string(String)) --> !,
  quoted(atom(String)).
term(var(Variable)) --> !,
  atom(Variable).
term(Prefix:Postfix) --> !,
  atom(Prefix),
  ":",
  atom(Postfix).
term(IRI) -->
  atom(IRI).

variable(Variable) -->
  "?",
  atom(Variable).

variables('*') -->
  "*".
variables([H|T]) -->
  variable(H),
  variables1(T).

variables1([]) --> [].
variables1([H|T]) -->
  " ",
  variable(H),
  variables1(T).

where(BGP) -->
  "WHERE {\n",
  bgp(BGP),
  "}\n".

