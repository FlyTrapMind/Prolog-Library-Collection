:- module(
  dispatch,
  [
    dispatch/1, % +Request:list
    dispatch_method/3, % +Module:atom
                       % +Method:oneof([delete,get,send])
                       % +Request:list
    http_method/2, % +Request:list
                   % -Method:oneof([delete,get,post])
    return_error/1 % +Error:compound
  ]
).

/** <module> SWAPP dispatch

Dispatching non-login methods goes through this module,
which checks whether the user has authorization.

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/11
*/

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(server(authorization)).

:- meta_predicate(dispatch(:)).



%! dispatch(:Request:list)
% Dispatching a request means:
%   1. Extracting the HTTP method of the request.
%   2. Checking whether the current user is authorized to perform the request.
%   3. Dispatching the request-method pair using the given module.

dispatch(Module:Request):-
  http_method(Request, Method),
  authorized(Method, Request),
  dispatch_method(Module, Method, Request).

%! dispatch_method(
%!   +Module:atom,
%!   +Method:oneof([delete,get,send]),
%!   +Request:list
%! ) is det.

dispatch_method(Module, Method, Request):-
  catch(
    Module:dispatch_method(Method, Request),
    Error,
    return_error(Error)
  ), !.
% The dispatcher for the given method is undefined.
dispatch_method(Module, Method, _Request):-
  PlainError = 'Undefined HTTP method.',
  format(
    atom(Msg),
    'Method ~w is not defined by module ~w.',
    [Method,Module]
  ),
  reply_json(json([error=PlainError,message=Msg]), [width(0)]).

%! http_method(+Request:list, -Method:oneof([delete,get,post])) is det.
% Returns the HTTP method used in the given request.

http_method(Request, Method):-
  memberchk(method(Method_), Request),
  % TODO What is `http_method`?
  http_parameters(Request, [http_method(Method,[default(Method_)])]).

%! return_error(+Error:compound) is det.
% Repies with the given error in JSON format.

return_error(Error):-
  % Do not include the outer `error` functor in JSON communication.
  extract_error(Error, PlainError),
  message_to_string(Error, Msg),
  reply_json(json([error=PlainError,message=Msg]), [width(0)]).

%! extract_error(+Error:compound, -PlainError:compound) is det.
% Make sure the error terms are of the same form,
% removing the outer functor 'error` when present.

extract_error(error(Type,_), Error):- !,
  functor(Type, Error, _).
extract_error(Error, Error).

