:- module(http_dcg_test, []).

:- use_module(http(http_dcg)).
:- use_module(library(apply)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(readutil)).
:- use_module(library(socket)).

:- multifile(thread_httpd:open_client_hook/5).


/*
thread_httpd:open_client_hook(
  tcp_client(ClientSocket, Goal, _Peer),
  Goal,
  In,
  Out,
  []
):-
  tcp_open_socket(ClientSocket, In, Out),
  read_header_data(In, Codes),
  phrase(
    request(Tree, Method, URI, Version, MessageHeaders, MessageBody),
    Codes,
    _Rest
  ),
  maplist(
    to_terminal,
    ['Tree','Method','URI','Version','MessageHeaders','MessageBody'],
    [Tree,Method,URI,Version,MessageHeaders,MessageBody]
  ),
  flush_output(user_output).
*/

to_terminal(Name, Value):-
  format(user_output, '~w\t~w\n', [Name,Value]).

read_header_data(In, Text) :-
  read_line_to_codes(In, Text, Tail),
  read_header_data(Text, In, Tail).

read_header_data("\r\n", _, _):- !.
read_header_data(_, In, Tail):-
  read_line_to_codes(In, Tail, NewTail),
  read_header_data(Tail, In, NewTail).
