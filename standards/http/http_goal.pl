:- module(
  http_goal,
  [
    http_goal/3, % +Url:atom
                 % +Options:list(nvpair)
                 % :Goal
    http_goal/4 % +Url:atom
                % +Options:list(nvpair)
                % :Goal
                % +Attempts:or([integer,oneof([inf])])
  ]
).

/** <module> HTTP goal

Execute HTTP goals while catching any exceptions and retrying
until the goal succeeds or fails.

## http_open/3

It would be nice to have an exhaustive list of exceptions
thrown by this predicate, since the calling code may have to act upon
different exceptions in different ways:

  * `error(existence_error(url,Url),Context)`
  * `error(io_error(Mode:oneof([read,write]),Stream),Context)`
  * `error(permission_error(redirect,http,Url),Context)`
  * `error(socket_error(Reason),Context)`
  * `error(timeout_error(Mode:oneof([read,write]),Stream),Context)`

Another way to detect failure to establish a successful connection over HTTP
is to look at the value of option `status_code`,
whenever it does not instantiate to an integer in the range =2xx=.

@author Wouter Beek
@version 2013/11, 2014/01, 2014/04
*/

%%%%:- use_module(http(rfc2616_status_line)).
:- use_module(library(debug)).
:- use_module(library(http/http_cookie)). % HTTP redirection requires cookies sometimes.
:- use_module(library(http/http_header)). % For option `post(...)`.
:- use_module(library(http/http_json)). % For option `post(json([...]))`.
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(option)).
:- use_module(math(math_ext)).

:- meta_predicate(http_goal(+,+,1)).
:- meta_predicate(http_goal(+,+,1,+)).
:- meta_predicate(http_catcher(+,+,+,1,+)).
:- meta_predicate(http_process(+,+,1)).



cert_verify(_, _, _, _, _):- !.


%! http_goal(+Url:atom, +Options:list(nvpair), :Goal) is det.
%! http_goal(
%!   +Url:atom,
%!   +Options:list(nvpair),
%!   :Goal,
%!   +Attempts:or([integer,oneof([inf])])
%! ) is det.
% Executes the given goal on the content found at the given URL.
% Always succeeds.
%
% The number of attempts is the number of HTTP-related exceptions
%  that can be encountered before the predicate gives up.
%
% The arguments of `Goal` are appended with the argument `Stream`.
%
% The following options are supported:
%   * =|attempts(+NumberOfAttempts:or([positive_integer,oneof([inf])]))|=
%     The number of attempts we make,
%     i.e. the maximum number of times we send an HTTP request
%     and receive an error.
%     Default: 1.
%   * Other options are given to http_open/3.

http_goal(Url, O1, Goal):-
  % The default number of attempts is 1.
  select_option(attempts(Attempts), O1, O2, 1),
  merge_options(
    [cert_verify_hook(cert_verify),timeout(100)],
    O2,
    O3
  ),
  http_goal(Url, O3, Goal, Attempts).

http_goal(Url, O1, Goal, Attempts):-
  merge_options([status_code(Status)], O1, O2),
  catch(
    setup_call_cleanup(
      http_open(Url, Stream, O2),
      http_process(Status, Stream, Goal),
      close(Stream)
    ),
    E,
    http_catcher(E, Url, O2, Goal, Attempts)
  ).


% Succeed.
http_catcher(exit, _, _, _, _).
% That was the last attempt.
% Re-throw the last thrown exception as the final one.
http_catcher(E, Url, _, _, 1):- !,
  debug(http_goal, 'Permanent failure to execute goal on URL ~w.', [Url]),
  throw(E).
% No quite right yet, but we have some attempts left.
http_catcher(_, Url, O1, Goal, Attempts1):-
  count_down(Attempts1, Attempts2),
  http_goal(Url, O1, Goal, Attempts2).


% HTTP status codes in the range 2xx represent a successful connection.
http_process(Status, Stream, Goal):-
  between(200, 299, Status), !,
  call(Goal, Stream).
% Http status codes not in the range 2xx (mostly 4xx and 5xx) represent
% a non-successful connection).
http_process(Status, _, _):-
  % The catcher decides whether a new attempts is made,
  % or whether this exception is thrown to the calling context.
  throw(error(http_status(Status),_)).



% Messages

/*:- multifile(prolog:message//1).

prolog:message(error(http_status(Status))) -->
  {'Status-Code'(Status, Reason)},
  ['[HTTP ',Status,'] ',Reason].*/

