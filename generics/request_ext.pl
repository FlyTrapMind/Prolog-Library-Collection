:- module(
  request_ext,
  [
    request_filter/4, % +Request:list(nvpair)
                      % +Method:oneof([delete,get,head,option,post,put])
                      % +Accept:pair(atom)
                      % -Location:url
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

:- use_module(library(http/http_path)).



request_filter(Request, Method, Accept, Location):-
  memberchk(method(Method), Request),
  request_filter_accept(Accept, Request),
  memberchk(path(Path), Request),
  http_absolute_uri(Path, Location).

request_filter_accept(X/Y, Request):-
  memberchk(accept(Accept), Request),
  once((
    % For some entries in `Request`
    % the media type may be uninstantiated.
    member(media(X0/Y0,_,_,_), Accept),
    ground(X0/Y0),
    X/Y = X0/Y0
  )).


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

