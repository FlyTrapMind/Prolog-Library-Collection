:- module(
  http,
  [
    http_open_wrapper/3, % +URI:uri
                         % -Stream:stream
                         % +Options:list(nvpair)
    http_parameters_fail/2, % +Request
                            % ?Parameters:list
    serve_nothing/1, % +Request
    serve_xml/1, % +XML
    serve_xml/2, % +DTD_Name:atom
                 % +DOM:dom
    serve_xml/3 % +DTD_Name:atom
                % +Style_Name:atom
                % +DOM:dom
  ]
).

/** <module> HTTP

Predicates for sending out HTTP requests.

@author Wouter Beek
@version 2012/10, 2013/02, 2013/11
*/

:- use_module(library(debug)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_parameters)).
:- use_module(xml(xml_dom)).

:- meta_predicate(http_process_exception(+,0)).

:- debug(http).



%! http_open_wrapper(+URI:uri, -Stream:stream, +Options:list(nvpair)) is det.
% Retries the given URI a specific number of times.
% This helps when working with unreliable connections.

http_open_wrapper(URI, Stream, Options):-
  http_open_wrapper(URI, Stream, Options, 0).

% The maximum number of HTTP attempts has been made.
http_open_wrapper(URI, _Stream, _Options, 5):- !,
  debug(
    http,
    'The maximum number of HTTP attempts was reached for <~w>.',
    [URI]
  ).
http_open_wrapper(URI, Stream, Options, Attempts):-
  catch(
    (
      http_open(URI, Stream, Options),
      set_stream(Stream, encoding(utf8))
    ),
    Exception,
    (
      NewAttempts is Attempts + 1,
      http_process_exception(
        Exception,
       http_open_wrapper(URI, Stream, Options, NewAttempts)
      )
    )
  ).

%! http_parameters_fail(Request, Parameters) is semidet.
% Like http_parameters/2, but fails when a given parameter is not found
% in the request.
%
% @see http_parameters/2

http_parameters_fail(Request, Parameters):-
  catch(
    http_parameters(Request, Parameters),
    error(existence_error(_Type, _Term), _Context),
    fail
  ).

% Retry after a while upon existence error.
% Thrown by http_open/3
http_process_exception(error(existence_error(url, URI), Context), Goal):- !,
  debug(http, 'Resource <~w> does not seem to exist.', [URI]),
  debug(http, '[~w]', [Context]),
  sleep(10),
  call(Goal).
% Retry upon socket error.
% Thrown by http_open/3.
http_process_exception(error(socket_error('Try Again'), _Context), Goal):- !,
  debug(http, '[SOCKET ERROR] Try again!', []),
  call(Goal).
% Retry upon I/O error.
%http_process_exception(
%  error(io_error(read, _Stream), context(_Predicate, Reason)),
%  Goal
%):- !,
%  debug(http, '[IO-EXCEPTION] ~w', [Reason]),
%  call(Goal).
http_process_exception(Exception, _Goal):-
  debug(http, '!UNRECOGNIZED EXCEPTION! ~w', [Exception]).

serve_nothing(Request):-
  memberchk(pool(client(_, _ , _In, Out)), Request),
  http_reply_header(Out, status(no_content), []).

serve_xml(XML):-
  % The User Agent needs to know the content type and encoding.
  % If the UTF-8 encoding is not given here explicitly,
  % Prolog throws an IO exception on `format(XML)`.
  format('Content-type: application/xml; charset=utf-8~n~n'),
  format(XML).

serve_xml(DTD_Name, DOM):-
  dom_to_xml(DTD_Name, DOM, XML),
  serve_xml(XML).

serve_xml(DTD_Name, Style_Name, DOM):- !,
  dom_to_xml(DTD_Name, Style_Name, DOM, XML),
  serve_xml(XML).

