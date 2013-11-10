:- module(
  ckan,
  [
    act/5, % +Options:list(nvpair)
           % +Action:atom
           % +Query:list(nvpair)
           % +In:compound
           % -Out:compound
    act_web/5, % +Options:list(nvpair)
               % +Action:atom
               % +Query:list(nvpair)
               % +In:compound
               % -DOM:list
    group_list/3, % +Options:list(nvpair)
                  % +Parameters:list(nvpair)
                  % -JSON:compound
    package_list/3, % +Options:list(nvpair)
                    % +Params:list(nvpair)
                    % -Out:compound
    site_read/1 % +Options:list(nvpair)
  ]
).

/** <module> CKAN

Querying the CKAN API.

@author Wouter Beek
@version 2013/11
*/

:- use_module(generics(uri_ext)).
:- use_module(html(html_table)).
:- use_module(library(apply)).
:- use_module(library(http/http_client)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(server(web_modules)).
:- use_module(standards(json_ext)).

:- initialization(web_module_add('CKAN', ckan, ckan)).

%! act(
%!   +Options:list(nvpair),
%!   +Action:atom,
%!   +Query:list(nvpair),
%!   -In:compound,
%!   -Out:compound
%! ) is det.
% The following options are supported:
%   * =|api_key(+Key:atom)|=
%     A atomic API key.
%   * =|api_version(+Version:positive_integer)|=
%     Default is uninstantiated (use server-side default).
%   * =|authority(+Authority:atom)|=
%     Default: =|datahub.io|=
%   * =|scheme(+Scheme:oneof([http,https]))|=
%     Default: =http=.
%
% The following actions are supported:
%   * =dashboard_activity_list=
%     Returns a list of activities from the user dashboard (API key required).
%   * =package_create=
%     Create a new dataset (API key required).
%   * =package_list=
%     Returns a list of the CKAN datasets.
%   * =package_search=
%     Search for CKAN datasets by name.
%     Requires the search option `q` to be set to the search term.
%     Optionally set the maximum number of returned search requests
%     with search option `rows` (with a positive integer value).
%   * =site_read=
%     Whether the CKAN site gives the user read access?
%
% @param Options A list of name-value pairs.
% @param Action The atomic name of a CKAN action.
% @param Query A list of name-value pairs representing query terms.
% @param In JSON-formatted input.
% @param Out JSON-formatted output.
%
% @see http://docs.ckan.org/en/latest/api.html

act(O1, Action, Query, In, Out2):-
  % URL
  option(scheme(Scheme), O1, http),
  option(authority(Authority), O1, 'datahub.io'),
  option(api_version(API_Version), O1, _VAR),
  uri_path([api,API_Version,action,Action], Path),
  uri_query_components(Search, Query),
  uri_components(
    URL,
    uri_components(Scheme, Authority, Path, Search, _Fragment)
  ),

  % API key
  (
    option(api_key(API_Key), O1)
  ->
    O0 = [request_header('Authorization'=API_Key)]
  ;
    O0 = []
  ),

  (
    In == []
  ->
    % HTTP GET
    http_get(URL, json(Out1), O0)
  ;
    % HTTP POST
    http_post(URL, json(In), json(Out1), O0)
  ),
  memberchk(result=Out2, Out1).

act_web(O1, Action, Query, In, [DOM]):-
  act(O1, Action, Query, In, Out1),
  json_rows(Out1, Out2),
  format(atom(Caption), 'The outcome of CKAN action ~w.', [Action]),
  html_table([caption(Caption),header(true),indexed(true)], Out2, DOM).

%! group_list(
%!   +Options:list(nvpair),
%!   +Parameters:list(nvpair),
%!   -JSON:compound
%! ) is det.

group_list(O1, Params, Out):-
  act(O1, group_list, [], Params, Out).

%! package_list(
%!   +Options:list(nvpair),
%!   +Parameters:list(nvpair),
%!   -JSON:compound
%! ) is det.
% The following options are supported:
%   * =|api_key(+Key:atom)|=
%     A atomic API key.
%   * =|api_version(+Version:positive_integer)|=
%     Default is uninstantiated (use server-side default).
%   * =|authority(+Authority:atom)|=
%     Default: =|datahub.io|=
%   * =|scheme(+Scheme:oneof([http,https]))|=
%     Default: =http=.
%
% The following parameters are supported:
%   * =|limit(+Limit:positive_integer)|=
%     If given, the list of datasets will be broken into pages
%     of at most =limit= datasets per page and only one page
%     will be returned at a time (optional).
%   * =|offset(+Offset:nonneg)|=
%     When =limit= is given, the offset to start returning packages from.
%
% @param Options A list of name-value pairs.
% @param Query A list of name-value pairs representing query terms.
% @param Out A list of the atomic names of CKAN packages.
%
% @tbd Query terms =limit= and =offset= do not change the output
%      on =datahub.io=.

package_list(O1, Params, Out):-
  act(O1, package_list, [], Params, Out).

%! site_read(+Options:list(nvpair)) is semidet.
% The following options are supported:
%   * =|api_key(+Key:atom)|=
%     A atomic API key.
%   * =|api_version(+Version:positive_integer)|=
%     Default is uninstantiated (use server-side default).
%   * =|authority(+Authority:atom)|=
%     Default: =|datahub.io|=
%   * =|scheme(+Scheme:oneof([http,https]))|=
%     Default: =http=.

site_read(O1):-
  act(O1, site_read, [], [], Out),
  Out == @true.

