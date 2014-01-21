:- module(
  http,
  [
    http_dateTime/1, % -DateTime:term
    http_goal/3, % +URL:atom
                 % +Options:list(nvpair)
                 % :Goal
    http_goal/4, % +URL:atom
                 % +Options:list(nvpair)
                 % :Goal
                 % +Attempts:or([integer,oneof([inf])])
    serve_nothing/1, % +Request:list
    xml_serve_atom/1, % +XML:atom
    xml_serve_dom/2 % +Options:list(nvpair)
                    % +DOM:list
  ]
).

/** <module> HTTP

Predicates for sending out HTTP requests.

@author Wouter Beek
@version 2012/10, 2013/02, 2013/11, 2014/01
*/

:- use_module(generics(atom_ext)).
:- use_module(http(rfc2616_status_line)).
:- use_module(library(debug)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(math(math_ext)).
:- use_module(xml(xml_dom)).

:- meta_predicate(http_goal(+,+,1)).
:- meta_predicate(http_goal(+,+,1,+)).
:- meta_predicate(http_catcher(+,+,+,1,+)).
:- meta_predicate(http_process(+,+,1)).



%! http_dateTime(-DateTime:atom) is det.
% Returns a term describing the current date and time.
%
% @compat RFC 1123


http_dateTime(DateTime):-
  get_time(TimeStamp),
  http_timestamp(TimeStamp, DateTime).


%! http_goal(+URL:atom, +Options:list(nvpair), :Goal) is det.
%! http_goal(
%!   +URL:atom,
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
%   * =|never_give_up(+NeverGiveUp:boolean)|=
%     Never give up upon receiving an HTTP 5xx status code.
%     Default: `false`.
%   * =|nocatch(+DoNotCatchExceptions:boolean)|=
%     When set to `true` (default `false`) exceptions are not caught
%     and no automated retrying occurs.

http_goal(URL, O1, Goal):-
  option(nocatch(true), O1, false), !,
  merge_options(
    [cert_verify_hook(cert_verify),status_code(Status),timeout(1)],
    O1,
    O2
  ),
  setup_call_cleanup(
    http_open(URL, Stream, O2),
    http_process(Status, Stream, Goal),
    close(Stream)
  ).
http_goal(URL, O1, Goal):-
  http_goal(URL, O1, Goal, 10).
http_goal(URL, O1, Goal, Attempts):-
  merge_options(
    [cert_verify_hook(cert_verify),status_code(Status),timeout(1)],
    O1,
    O2
  ),
  catch(
    setup_call_cleanup(
      http_open(URL, Stream, O2),
      http_process(Status, Stream, Goal),
      close(Stream)
    ),
    E,
    http_catcher(E, URL, O2, Goal, Attempts)
  ).

cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
  debug(user_error, 'Accepting certificate', []).

% Succeed.
http_catcher(exit, URL, _, Goal, _):- !,
  term_to_atom(Goal, Atom1),
  atom_truncate(Atom1, 120, Atom2),
  debug(http_low, 'Successfully performed goal ~w on URL ~w.', [Atom2,URL]).
% Permanently fail to receive resource over HTTP.
http_catcher(E, URL, O1, Goal, 0):- !,
  http_exception(E),
  (
    E = error(http_status(Status),_),
    between(500, 599, Status),
    option(never_give_up(true), O1, false)
  ->
    sleep(1),
    http_goal(URL, O1, Goal)
  ;
    fail
  ).
% Incidental fail: retry.
http_catcher(_, URL, O1, Goal, Attempts1):-
  count_down(Attempts1, Attempts2),
  http_goal(URL, O1, Goal, Attempts2).

%! http_exception(+Exception:compound) is det.
% Handle exceptions thrown by http_open/3.

% Retry after a while upon existence error.
http_exception(error(existence_error(url, URL),Context)):- !,
  debug(http, 'URL ~w does not exist (context: ~w).', [URL,Context]).
% HTTP status code.
http_exception(error(http_status(Status),_Context)):- !,
  'Status-Code'(Status, Reason),
  debug(http, '[HTTP-STATUS] ~d ~w', [Status,Reason]).
% Retry upon I/O error.
http_exception(error(io_error(read,_Stream),context(_Predicate,Reason))):- !,
  debug(http, '[IO-ERROR] ~w', [Reason]).
http_exception(error(permission_error(redirect,http,URL),context(_,Reason))):- !,
  debug(http, '[PERMISSION-ERROR] ~w (reason: ~w)', [URL,Reason]).
% Retry upon socket error.
% Thrown by http_open/3.
http_exception(error(socket_error(Reason),_)):- !,
  debug(http, '[SOCKET-ERROR] ~w', [Reason]).
% `Mode` is either `read` or `write`.
http_exception(error(timeout_error(Mode,_Stream),context(PredSignature,_))):- !,
  debug(http, '[TIMEOUT-ERROR] While ~wing ~w.', [Mode,PredSignature]).
% DEB
http_exception(E):-
  debug(http, '[UNRECOGNIZED-EXCEPTION] ~w', [E]).

% Success codes.
http_process(Status, Stream, Goal):-
  between(200, 299, Status), !,
  call(Goal, Stream).
% Non-success codes.
http_process(Status, _, _):-
  % The catcher has to make a new attempt (if there are any attempts left).
  throw(error(http_status(Status),_Context)).


serve_nothing(Request):-
  memberchk(pool(client(_, _ , _In, Out)), Request),
  http_reply_header(Out, status(no_content), []).


%! xml_serve_atom(+XML:atom) is det.
% Serves the given XML-formatted atom.

xml_serve_atom(XML):-
  % The User Agent needs to know the content type and encoding.
  % If the UTF-8 encoding is not given here explicitly,
  % Prolog throws an IO exception on `format(XML)`.
  format('Content-type: application/xml; charset=utf-8~n~n'),
  format(XML).


%! xml_serve_dom(+Options:list(nvpair), +DOM:list) is det.
% Serves the given XML DOM.
%
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%   * =|style(+StyleName:atom)|=
%     The atomic name of a style file on the =css= search path.

xml_serve_dom(O1, DOM):-
  xml_dom_to_atom(O1, DOM, XML),
  xml_serve_atom(XML).

