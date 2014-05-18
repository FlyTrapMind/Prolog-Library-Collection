:- module(
  uri_query,
  [
    request_query_read/3, % +Request:list(nvpair)
                          % +QueryName:atom
                          % -PlTerm:term
    uri_query_add/4, % +FromUri:uri
                     % +QueryName:atom
                     % +Atom:atom
                     % -ToUri:uri
    uri_query_add_pl_term/4, % +FromUri:uri
                             % +Name:atom
                             % @Term
                             % -ToUri:uri
    uri_query_read/3, % +Uri:uri
                      % +Name:atom
                      % -Atom:atom
    uri_query_read_pl_term/3 % +Uri:uri
                             % +Name:atom
                             % -Term:term
  ]
).

/** <module> URI query

Support for the query string part of URIs.

@author Wouter Beek
@version 2014/03, 2014/05
*/

:- use_module(library(uri)).

:- use_module(generics(option_ext)).
:- use_module(pl(pl_log)).



%! request_query_read(+Request:list(nvpair), +Name:atom, -Term:term) is det.

request_query_read(Request, Name, Term):-
  memberchk(search(SearchPairs), Request),
  memberchk(Name=Atom, SearchPairs), !,
  read_term_from_atom(Atom, Term, []).


%! uri_query_add(+FromUri:uri, +Name:atom, +Atom:atom, -ToUri:uri) is det.
% Inserts the given name-value pair as a query component into the given URI.

uri_query_add(Uri1, Name, Atom, Uri2):-
  % Disasseble the old URI.
  uri_components(
    Uri1,
    uri_components(Scheme, Authority, Path, SearchString1, Fragment)
  ),

  % When an URI has no search parameters,
  % its search string is uninstantiated.
  catch(
    uri_query_components(SearchString1, SearchOptions1),
    error(instantiation_error,_),
    SearchOptions1 = []
  ),

  % Search parameters are represented as option lists.
  add_option(SearchOptions1, Name, Atom, SearchOptions2),

  % Construct the new URI.
  uri_query_components(SearchString2, SearchOptions2),
  uri_components(
    Uri2,
    uri_components(Scheme, Authority, Path, SearchString2, Fragment)
  ).


%! uri_query_add_pl_term(+FromUri:uri, +Name:atom, +Term:term, -ToUri:uri) is det.

uri_query_add_pl_term(Uri1, Name, Term, Uri2):-
  canonical_blobs_atom(Term, Atom),
  uri_query_add(Uri1, Name, Atom, Uri2).


%! uri_query_read(+Uri:uri, +QueryName:atom, -Atom:atom) is semidet.
% Returns the value for the query item with the given name, if present.
%
% @tbd Can the same query name occur multiple times?

uri_query_read(Uri, Name, Atom):-
  uri_components(Uri, UriComponents),
  uri_data(search, UriComponents, QueryString),
  uri_query_components(QueryString, QueryPairs),
  memberchk(Name=Atom, QueryPairs).


%! uri_query_read_pl_term(+Uri:uri, +QueryName:atom, -Term:term) is semidet.

uri_query_read_pl_term(Uri, Name, Term):-
  uri_query_read(Uri, Name, Atom),
  read_term_from_atom(Atom, Term, []).

