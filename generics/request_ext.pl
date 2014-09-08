:- module(
  request_ext,
  [
    request_filter/4, % +Request:list(nvpair)
                      % +Method:oneof([delete,get,head,option,post,put])
                      % +Accept:pair(atom)
                      % -Location:url
    request_filter/5, % +Request:list(nvpair)
                      % +Method:oneof([delete,get,head,option,post,put])
                      % +Accept:atom
                      % -Location:url
                      % -LocationComponents:pair(url,atom)
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
@version 2014/08-2014/09
*/

:- use_module(library(http/http_cors)).



request_filter(Request, Method, Accept, Location):-
gtrace,
  cors_enable,
  writeln(Request),
  Method = Request,
  Accept = Request,
  Location = Request.


request_filter(Request, Method, Accept, Location, LocationComponents):-
  request_filter(Request, Method, Accept, Location),
  split_location(Request, Location, LocationComponents).

split_location(_Request, Location, Base-Path):-
  Base = Location,
  Path = Location.


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

