:- module(
  uri_search,
  [
    request_search_read/3, % +Request:list(nvpair)
                           % +Name:atom
                           % -Value
    request_search_read/4, % +Request:list(nvpair)
                           % +Name:atom
                           % -Value
                           % +Default
    request_search_read_pl_term/3, % +Request:list(nvpair)
                                   % +Name:atom
                                   % -Value
    uri_search_add/3, % +FromUri:or([compound,uri])
                      % +Parameters:list(nvpair)
                      % -ToUri:uri
    uri_search_add/4, % +FromUri:or([compound,uri])
                      % +Name:atom
                      % +Value:atom
                      % -ToUri:uri
    uri_search_add_pl_term/4, % +FromUri:or([compound,uri])
                              % +Name:atom
                              % +Value:term
                              % -ToUri:uri
    uri_search_read/3, % +Uri:or([compound,uri])
                       % +Name:atom
                       % -Value:atom
    uri_search_read_pl_term/3 % +Uri:or([compound,uri])
                              % +Name:atom
                              % -Value:term
  ]
).

/** <module> URI search

Support for the search string part of URIs.

@author Wouter Beek
@version 2014/03, 2014/05-2014/06, 2014/08
*/

:- use_module(library(uri)).

:- use_module(generics(option_ext)).
:- use_module(pl(pl_log)).



%! request_search_read(+Request:list(nvpair), +Name:atom, -Value) is det.

request_search_read(Request, Name, Value):-
  memberchk(search(SearchPairs), Request),
  memberchk(Name=Value, SearchPairs), !.

%! request_search_read(
%!   +Request:list(nvpair),
%!   +Name:atom,
%!   -Value,
%!   +Default
%! ) is det.

request_search_read(Request, Name, Value, _):-
  request_search_read(Request, Name, Value), !.
request_search_read(_, _, Default, Default).


%! request_search_read_pl_term(
%!   +Request:list(nvpair),
%!   +Name:atom,
%!   -Value:term
%! ) is det.

request_search_read_pl_term(Request, Name, Value2):-
  request_search_read(Request, Name, Value1),
  read_term_from_atom(Value1, Value2, []).


%! uri_search_add(
%!   +FromUri:or([compound,uri]),
%!   +Parameters:list(nvpair),
%!   -ToUri:uri
%! ) is det.

uri_search_add(Uri1, Params0, Uri2):-
  uri_components0(Uri1, uri_components(Scheme,Auth,Path,Search1,Frag)),
  (
    var(Search1)
  ->
    Params1 = []
  ;
    uri_query_components(Search1, Params1)
  ),
  merge_options(Params0, Params1, Params2),
  uri_query_components(Search2, Params2),
  uri_components(Uri2, uri_components(Scheme,Auth,Path,Search2,Frag)).


%! uri_search_add(
%!   +FromUri:or([compound,uri]),
%!   +Name:atom,
%!   +Value:atom,
%!   -ToUri:uri
%! ) is det.
% Inserts the given name-value pair as a query component into the given URI.

uri_search_add(Uri1, Name, Value, Uri2):-
  % Disasseble the old URI.
  uri_components0(
    Uri1,
    uri_components(Scheme,Authority,Path,SearchString1,Fragment)
  ),

  % When an URI has no search parameters,
  % its search string is uninstantiated.
  catch(
    uri_query_components(SearchString1, SearchOptions1),
    error(instantiation_error,_),
    SearchOptions1 = []
  ),

  % Search parameters are represented as option lists.
  add_option(SearchOptions1, Name, Value, SearchOptions2),

  % Construct the new URI.
  uri_query_components(SearchString2, SearchOptions2),
  uri_components(
    Uri2,
    uri_components(Scheme,Authority,Path,SearchString2,Fragment)
  ).


%! uri_search_add_pl_term(
%!   +FromUri:or([compound,uri]),
%!   +Name:atom,
%!   +Value:term,
%!   -ToUri:uri
%! ) is det.

uri_search_add_pl_term(Uri1, Name, Value1, Uri2):-
  canonical_blobs_atom(Value1, Value2),
  uri_search_add(Uri1, Name, Value2, Uri2).


%! uri_search_read(
%!   +Uri:or([compound,uri]),
%!   +Name:atom,
%!   -Value:atom
%! ) is semidet.
% Returns the value for the query item with the given name, if present.
%
% @tbd Can the same query name occur multiple times?

uri_search_read(Uri, Name, Value):-
  uri_components0(Uri, UriComponents),
  uri_data(search, UriComponents, QueryString),
  uri_query_components(QueryString, QueryPairs),
  memberchk(Name=Value, QueryPairs).


%! uri_search_read_pl_term(
%!   +Uri:or([compound,uri]),
%!   +Name:atom,
%!   -Value:term
%! ) is semidet.

uri_search_read_pl_term(Uri, Name, Value2):-
  uri_search_read(Uri, Name, Value1),
  read_term_from_atom(Value1, Value2, []).



% Helpers

uri_components0(
  uri_components(Scheme,Authority,Path,Search,Fragment),
  uri_components(Scheme,Authority,Path,Search,Fragment)
):- !.
uri_components0(Uri, UriComponents):-
  uri_components(Uri, UriComponents).

