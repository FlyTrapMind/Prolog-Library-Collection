:- module(
  json_ext,
  [
    http_read_json2/2, % +Request:list
                       % -JSON:compound
    json_rows/2 % +JSON:list
                % -Rows:list
  ]
).
:- reexport(
  library(http/http_json),
  [
    http_read_json/2,
    http_read_json/3,
    reply_json/1,
    reply_json/2
  ]
).

/** <module> JSON_EXT

@author Wouter Beek
@version 2013/07, 2013/11
*/

:- use_module(library(error)).
:- use_module(library(http/http_client)).
:- use_module(library(option)).



%! http_read_json2(+Request:list(nvpair), -JSON:compound) is det.
% @see Like http_read_json/2, but is not restricted to the HTTP POST method.

http_read_json2(Request, JSON2):-
  memberchk(content_type(Type), Request),
  (
    http_json:is_json_type(Type)
  ->
    http_read_data(Request, JSON1, [])
  ;
    domain_error(mimetype, Type)
  ),
  JSON1 = json(JSON2).

json_header_row([json(L1)|_], L2):-
  maplist(json_name, L1, L2).

json_name(N=_, N).

json_row(json(L1), L2):-
  maplist(json_value, L1, L2).

%! json_rows(+JSON:list, -Rows:list) is det.
% Converts a list of JSON objects to (HTML) table rows.

json_rows(JSON, [HeaderRow|DataRows]):-
  json_header_row(JSON, HeaderRow),
  maplist(json_row, JSON, DataRows).

json_value(_=V, V).

