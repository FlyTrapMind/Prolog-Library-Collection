:- module(
  json_ext,
  [
    http_parameters2/2, % +Request:list
                        % ?Params:list
    http_parameters2/3, % +Request:list
                        % ?Params:list
                        % :Options:list
    http_read_json2/2 % +Request:list
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

:- use_module(library(error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(option)).

:- meta_predicate(http_parameters2(+,?,:)).



%! http_parameters2(+Request, ?Params)
% @see Like http_parameters/2, but works for JSON as well and
%      is not restricted to the POST method.

http_parameters2(Request, Params) :-
  http_parameters2(Request, Params, []).

%! http_parameters2(+Request, ?Params, :Options)
% @see Like http_parameters/2, but works for JSON as well and
%      is not restricted to the POST method.

http_parameters2(Request, Params, Options) :-
	must_be(list, Params),
	meta_options(http_parameters:is_meta, Options, QOptions),
	option(attribute_declarations(DeclGoal), QOptions, -),
	http_parms2(Request, Params, DeclGoal, Form),
	(   memberchk(form_data(RForm), QOptions)
	->  RForm = Form
	;   true
	).

http_parms2(Request, Params, DeclGoal, Data2) :-
  memberchk(content_type(Type), Request),
  http_json:is_json_type(Type), !,
  http_read_data(Request, Data1, []),
  Data1 = json(Data2),
  http_parameters:fill_parameters(Params, Data2, DeclGoal).

%! http_read_json2(+Request:list(nvpair), -JSON:compound) is det.
% @see Like http_read_json/2, but is not restricted to the HTTP POST method.

http_read_json2(Request, JSON):-
  memberchk(content_type(Type), Request),
  (
    http_json:is_json_type(Type)
  ->
    http_read_data(Request, JSON, [])
  ;
    domain_error(mimetype, Type)
  ).

