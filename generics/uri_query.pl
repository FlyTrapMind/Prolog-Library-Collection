:- module(
  uri_query,
  [
    uri_query_add_name/3, % +FromUri:or([compound,url])
                          % +Name:atom
                          % -ToUri:url
    uri_query_add_nvpair/4, % +FromUri:or([compound,url])
                            % +Name:atom
                            % +Value:atom
                            % -ToUri:url
    uri_query_add_nvpairs/3, % +FromUri:or([compound,url])
                             % +NVPairs:list(nvpair)
                             % -ToUri:url
    uri_query_add_pl_nvpair/4, % +FromUri:or([compound,uri])
                               % +Name:atom
                               % +Value:term
                               % -ToUri:uri
    uri_query_nvpair/3, % +Uri:or([compound,uri])
                        % +Name:atom
                        % -Value:atom
    uri_query_pl_nvpair/3 % +Uri:or([compound,uri])
                          % +Name:atom
                          % -Value:term
  ]
).

/** <module> URI Query String

Support for the query string part of URIs.

@author Wouter Beek
@version 2014/03, 2014/05-2014/06, 2014/08
*/

:- use_module(library(lambda)).
:- use_module(library(uri)).

:- use_module(generics(meta_ext)).
:- use_module(generics(option_ext)).
:- use_module(pl(pl_log)).

:- meta_predicate(uri_change_query_nvpairs(+,2,-)).
:- meta_predicate(uri_change_query_nvpairs0(+,2,-)).
:- meta_predicate(uri_change_query_string(+,2,-)).



%! uri_query_add_nvpairs(
%!   +FromUri:or([compound,url]),
%!   +NewNVPairs:list(nvpair),
%!   -ToUri:url
%! ) is det.

uri_query_add_nvpairs(Uri1, NewNVPairs, Uri2):-
  uri_change_query_nvpairs(
    Uri1,
    \NVPairs1^NVPairs2^merge_options(NewNVPairs, NVPairs1, NVPairs2),
    Uri2
  ).


%! uri_query_add_name(+FromUri:or([compound,url]), +Name:atom, -ToUri:url) is det.

uri_query_add_name(Uri1, Name, Uri2):-
  uri_change_query_string(
    Uri1,
    \Query1^Query2^atomic_list_concat([Query1,Name], '&', Query2),
    Uri2
  ).


%! uri_query_add_nvpair(
%!   +FromUri:or([compound,url]),
%!   +Name:atom,
%!   +Value:atom,
%!   -ToUri:url
%! ) is det.
% Inserts the given name-value pair as a query component into the given URI.

uri_query_add_nvpair(Uri1, Name, Value, Uri2):-
  uri_change_query_nvpairs(
    Uri1,
    \NVPairs1^NVPairs2^add_option(NVPairs1, Name, Value, NVPairs2),
    Uri2
  ).


%! uri_query_add_pl_nvpair(
%!   +FromUri:or([compound,uri]),
%!   +Name:atom,
%!   +Value:term,
%!   -ToUri:uri
%! ) is det.

uri_query_add_pl_nvpair(Uri1, Name, Value1, Uri2):-
  canonical_blobs_atom(Value1, Value2),
  uri_query_add_nvpair(Uri1, Name, Value2, Uri2).


%! uri_query_nvpair(
%!   +Uri:or([compound,uri]),
%!   +Name:atom,
%!   -Value:atom
%! ) is semidet.
% Returns the value for the query item with the given name, if present.
%
% @tbd Can the same query name occur multiple times?

uri_query_nvpair(Uri, Name, Value):-
  uri_components0(Uri, UriComponents),
  uri_data(search, UriComponents, QueryString),
  uri_query_components(QueryString, QueryPairs),
  memberchk(Name=Value, QueryPairs).


%! uri_query_pl_nvpair(
%!   +Uri:or([compound,uri]),
%!   +Name:atom,
%!   -Value:term
%! ) is semidet.

uri_query_pl_nvpair(Uri, Name, Value2):-
  uri_query_nvpair(Uri, Name, Value1),
  read_term_from_atom(Value1, Value2, []).



% Helpers.

%! uri_components0(+Input:or([compound,url]), -UriComponents:compound) is det.
% Slight optimization that allows predicate to be called with URL components
% i.o. URLs. This helps in cases where the URL would have to be build
% prior to calling, and would have to be decomposed into components
% inside the call again.

uri_components0(
  uri_components(Scheme,Authority,Path,Search,Fragment),
  uri_components(Scheme,Authority,Path,Search,Fragment)
):- !.
uri_components0(Uri, UriComponents):-
  uri_components(Uri, UriComponents).


%! uri_change_query_nvpairs(+FromUri:url, :Goal, -ToUri:url) is det.

uri_change_query_nvpairs(Uri1, Goal, Uri2):-
  uri_change_query_string(Uri1, uri_change_query_nvpairs0(Goal), Uri2).

uri_change_query_nvpairs0(Goal, Query1, Query2):-
  uri_query_components(Query1, NVPairs1),
  call(Goal, NVPairs1, NVPairs2),
  uri_query_components(Query2, NVPairs2).


%! uri_change_query_string(+FromUri:url, :Goal, -ToUri:url) is det.
% Deterministic if `Goal` is deterministic.
%
% Meta-wrapper that is used by every predicate that operates
% on a URL's query string.
%
% The additional arguments of `Goal` are the list of query parameters
% before and after calling.

uri_change_query_string(Uri1, Goal, Uri2):-
  % Disasseble the old URI.
  uri_components0(
    Uri1,
    uri_components(Scheme,Authority,Path,Query1,Fragment)
  ),
  % BEWARE: If a URL has no query string,
  % then uri_components/2 does not give back the empty atom.
  % Instead, it leaves `Query1` uninstantiated.
  default('', Query1),
  
  call(Goal, Query1, Query2),
  
  % Back to URL form.
  uri_components(
    Uri2,
    uri_components(Scheme,Authority,Path,Query2,Fragment)
  ).

