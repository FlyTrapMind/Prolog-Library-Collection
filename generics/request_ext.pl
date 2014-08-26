:- module(
  request_ext,
  [
    request_query_nvpair/3, % +Request:list(nvpair)
                            % +Name:atom
                            % -Value
    request_query_nvpair/4, % +Request:list(nvpair)
                            % +Name:atom
                            % -Value
                            % +Default
    request_query_pl_nvpair/3 % +Request:list(nvpair)
                              % +Name:atom
                              % -Value
  ]
).

/** <module> Request extensions

Additional support for requests.

@author Wouter Beek
@version 2014/08
*/



%! request_query_nvpair(+Request:list(nvpair), +Name:atom, -Value) is det.

request_query_nvpair(Request, Name, Value):-
  memberchk(search(SearchPairs), Request),
  memberchk(Name=Value, SearchPairs), !.


%! request_query_nvpair(
%!   +Request:list(nvpair),
%!   +Name:atom,
%!   -Value,
%!   +Default
%! ) is det.

request_query_nvpair(Request, Name, Value, _):-
  request_query_nvpair(Request, Name, Value), !.
request_query_nvpair(_, _, Default, Default).


%! request_query_pl_nvpair(
%!   +Request:list(nvpair),
%!   +Name:atom,
%!   -Value:term
%! ) is det.

request_query_pl_nvpair(Request, Name, Value2):-
  request_query_nvpair(Request, Name, Value1),
  read_term_from_atom(Value1, Value2, []).

