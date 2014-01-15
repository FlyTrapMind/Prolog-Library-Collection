:- module(
  http,
  [
    http_dateTime/1, % -DateTime:term
    http_goal/3, % +URL:atom
                 % +Options:list(nvpair)
                 % :Goal,
    http_goal/4, % +URL:atom
                 % +Options:list(nvpair)
                 % :Goal,
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
:- use_module(library(option)).
:- use_module(math(math_ext)).
:- use_module(xml(xml_dom)).

:- meta_predicate(http_goal(+,+,1)).
:- meta_predicate(http_goal(+,+,1,+)).
:- meta_predicate(http_open_catcher(+,+,+,1,+)).
:- meta_predicate(http_open_exception(+,0)).
:- meta_predicate(http_open_process(+,+,1)).



%! http_dateTime(-DateTime:atom) is det.
% Returns a term describing the current date and time.
%
% @compat RFC 1123


http_dateTime(DateTime):-
  get_time(TimeStamp),
  http_timestamp(TimeStamp, DateTime).


%! http_goal(+URL:atom, +Options:list(nvpair), :Goal) is semidet.
%! http_goal(
%!   +URL:atom,
%!   +Options:list(nvpair),
%!   :Goal,
%!   +Attempts:or([integer,oneof([inf])])
%! ) is semidet.
% Executes the given goal on the content found at the given URL.
%
% The number of attempts is the number of HTTP-related exceptions
%  that can be encountered before the predicate gives up.
%
% The arguments of `Goal` are appended with the argument `Stream`.

http_goal(URL, Options, Goal):-
  http_goal(URL, Options, Goal, 5).

http_goal(URL, _, Goal, 0):- !,
  debug(
    http,
    'Could not execute goal ~w on URL ~w (no attempts left).',
    [Goal,URL]
  ).
http_goal(URL, O1, Goal, Attempts):-
  merge_options([status_code(Status)], O1, O2),
  setup_call_catcher_cleanup(
    http_open(URL, Stream, O2),
    http_open_process(Status, Stream, Goal),
    Catcher,
    (
      close(Stream),
      http_open_catcher(Catcher, URL, O2, Goal, Attempts)
    )
  ).

http_open_catcher(exit, URL, _, Goal, _):- !,
  term_to_atom(Goal, Atom1),
  truncate(Atom1, 120, Atom2),
  debug(http_low, 'Successfully performed goal ~w on URL ~w.', [Atom2,URL]).
http_open_catcher(Catcher, URL, Options, Goal, Attempts1):-
  debug(
    http,
    'Encountered exception ~w while executing goal ~w on URL ~w.',
    [Catcher,Goal,URL]
  ),
  count_down(Attempts1, Attempts2),
  http_open_exception(Catcher),
  http_goal(URL, Options, Goal, Attempts2).

%! http_open_exception(+Exception:compound) is det.
% Handle exceptions thrown by http_open/3.

% Retry after a while upon existence error.
http_open_exception(error(existence_error(url, URL),Context)):- !,
  debug(http, 'URL ~w does not exist.', [URL]),
  debug(http, '[~w]', [Context]),
  sleep(10).
% Retry upon I/O error.
http_open_exception(
  error(io_error(read,_Stream),context(_Predicate,Reason))
):- !,
  debug(http, '[IO-EXCEPTION] ~w', [Reason]).
% Retry upon socket error.
% Thrown by http_open/3.
http_open_exception(error(socket_error('Try Again'),_Context)):- !,
  debug(http, '[SOCKET ERROR] Try again!', []),
  sleep(1).
http_open_exception(exception(error(http_status(Status),_Context))):- !,
  'Status-Code'(Status, Reason),
  debug(http, '[HTTP STATUS CODE] ~d ~a', [Status,Reason]),
  sleep(60).
http_open_exception(Exception):-
  gtrace, %DEB
  debug(http, '!UNRECOGNIZED EXCEPTION! ~w', [Exception]).

% Success codes.
http_open_process(Status, Stream, Goal):-
  between(200, 299, Status), !,
  call(Goal, Stream).
% Non-success codes.
http_open_process(Status, _, _):-
  debug(http, '[HTTP STATUS CODE] ~d', [Status]),
  sleep(10),
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

