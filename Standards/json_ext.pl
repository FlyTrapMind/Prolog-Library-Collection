:- module(
  json_ext,
  [
    http_read_json2/2 % +Request:list(nvpair)
                      % -JSON:compound
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
@version 2013/07
*/

:- use_module(generics(meta_ext)).
:- use_module(library(http/http_client)).



%! http_read_json2(+Request:list(nvpair), -JSON:compound) is det.
% @see Like http_read_json/2, but is not restricted to the HTTP POST method.

http_read_json2(Request, JSON):-
  memberchk(content_type(Type), Request),
  unless(
    http_json:is_json_type(Type),
    domain_error(mimetype, Type)
  ),
  http_read_data(Request, JSON, []).

