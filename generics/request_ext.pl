:- module(
  request_ext,
  [
    request_nvpair/3, % +Request:list(nvpair)
                      % +Name:atom
                      % -Value
    request_nvpair/4, % +Request:list(nvpair)
                      % +Name:atom
                      % -Value
                      % +Default
    request_pl_term/3 % +Request:list(nvpair)
                      % +Name:atom
                      % -Value
  ]
).

/** <module> Request extensions

Additional support for requests.

@author Wouter Beek
@version 2014/08
*/



%! request_nvpair(+Request:list(nvpair), +Name:atom, -Value) is det.

request_nvpair(Request, Name, Value):-
  memberchk(search(SearchPairs), Request),
  memberchk(Name=Value, SearchPairs), !.

%! request_nvpair(
%!   +Request:list(nvpair),
%!   +Name:atom,
%!   -Value,
%!   +Default
%! ) is det.

request_nvpair(Request, Name, Value, _):-
  request_nvpair(Request, Name, Value), !.
request_nvpair(_, _, Default, Default).


%! request_pl_term(
%!   +Request:list(nvpair),
%!   +Name:atom,
%!   -Value:term
%! ) is det.

request_pl_term(Request, Name, Value2):-
  request_nvpair(Request, Name, Value1),
  read_term_from_atom(Value1, Value2, []).
