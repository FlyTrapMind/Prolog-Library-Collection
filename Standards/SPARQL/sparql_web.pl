:- module(
  sparql_web,
  [
    sparql_input_web/1, % -Markup:list
    sparql_input_web/2, % +Remote:atom
                        % -Markup:list
    sparql_output_web/2, % +Query:atom
                         % -Markup:list
    sparql_output_web/3 % +Remote:atom
                        % +Query:atom
                        % -Markup:list
  ]
).

/** <module> SPARQL Web

Web front-end for SPARQL queries.

@author Wouter Beek
@version 2012/12, 2013/03-2013/04, 2013/12
*/

:- use_module(html(html_form)).
:- use_module(html(html_table)).
:- use_module(library(http/http_dispatch)).
:- use_module(server(web_console)).
:- use_module(sparql(sparql_ext)).

:- http_handler(root(sparql), sparql, []).

:- dynamic(default_remote0/1).

:- debug(sparql_web).



% @tbd
sparql_input_web(Markup):-
  input_ui(Markup).

% @tbd
sparql_input_web(_Remote, Markup):-
  input_ui(Markup).

% @tbd
sparql(Request, Remote):-
  Request -> Query
  sparql_output_web(Remote, Query, Markup).

%! sparql_output_web(+Remote:atom, +Query:atom, -Markup:list) is det.
% Returns markup for the given SPARQL query.

% @tbd
sparql_output_web(Remote, Query, Markup):-
  statistics(cputime, CPU_Before),
  'SPARQL_query'(Remote, Query, VarNames, Results),
  statistics(cputime, CPU_After),
  CPU is CPU_After - CPU_Before,
  write_table(Results, [cputime(CPU),variables(VarNames)], Markup).

%! write_statistics(+Options:list(nvpairs), -Markup:dom) is det.
% Returns the markup element representing the statistics of a SPARQL query.

write_statistics(O1, element(p,[],[Message])):-
  option(count(Count), O1),
  option(cputime(CPU), O1),
  format(
    atom(Message),
    'Query completed in ~3f seconds, returning ~D rows.',
    [CPU,Count]
  ).

%! write_table(+Lists:list(list), +Options:list(nvpair), -Markup:dom) is det.
% Returns the markup table for the given SPARQL results.

write_table(Lists, Options, [Statistics,Table]):-
  is_list(Lists), !,
  length(Lists, Count),
  add_option(O1, count, Count, O2),
  write_statistics(O2, Stats),
  option(variables(VarNames), O2),
  html_table([header(true)], [VarNames|Lists], Table).
write_table(Row, O1, Markup):-
  write_table([Row], O1, Markup).

