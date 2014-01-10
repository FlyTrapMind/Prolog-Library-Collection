:- module(
  sparql_db,
  [
    sparql_add_prefix/1, % +Prefix:atom
    sparql_add_prefix/2, % +Prefix:atom
                         % +IRI:iri
    sparql_add_remote/4, % +Remote:atom
                         % +Server:atom
                         % +Port:or([oneof([default]),nonneg])
                         % +Path:atom
    sparql_current_prefix/2, % ?Prefix:atom
                             % ?IRI:iri
    sparql_current_remote/4, % ?Remote:atom
                             % ?Server:atom
                             % ?Port:or([oneof([default]),nonneg])
                             % ?Path:atom
    sparql_remove_prefix/1, % +Prefix:atom
    sparql_remove_remote/1 % +Remote:atom
  ]
).

/** <module> SPARQL database

Persistency store for SPARQL-related information.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/07, 2013/09, 2013/11-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(persistency)).
:- use_module(os(file_ext)).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:prolog_file_type(db, database)).

:- persistent(sparql_prefix(prefix:atom,iri:atom)).
:- persistent(sparql_remote(remote:atom,server:atom,port:atomic,path:atom)).

:- initialization(init_sparql_db).



init_sparql_db:-
  absolute_file_name(
    project(sparql),
    File,
    [access(write),file_type(database)]
  ),
  create_file(File),
  db_attach(File, []).

%! sparql_add_prefix(+Prefix:atom) is det.
% Adds the given SPARQL prefix,
% based on an existing XML prefix with the same name.
%
% @arg Prefix The atomic name of an existing XML prefix.

sparql_add_prefix(Prefix):-
  xml_current_namespace(Prefix, URI), !,
  sparql_add_prefix(Prefix, URI).
sparql_add_prefix(Prefix):-
  existence_error('XML namespace', Prefix).

%! sparql_add_prefix(+Prefix:atom, +IRI:iri) is det.
% XML namespace registrations and SPARQL prefix registrations are separated,
% because they may not be the same thing.
%
% @tbd Check the SPARQL 1.1 standard whether they are indeed different.

sparql_add_prefix(Prefix, IRI):-
  sparql_current_prefix(Prefix, IRI), !,
  debug(sparql_db, 'SPARQL prefix ~w is already set. No changes.', [Prefix]).
sparql_add_prefix(Prefix, _IRI1):-
  sparql_current_prefix(Prefix, _IRI2), !,
  debug(
    sparql_db,
    'SPARQL prefix ~w is already set DIFFERENTLY. First remove.',
    [Prefix]
  ).
sparql_add_prefix(Prefix, IRI):-
  with_mutex(sparql_db, assert_sparql_prefix(Prefix, IRI)).

sparql_add_remote(Remote, Server, Port, Path):-
  sparql_current_remote(Remote, Server, Port, Path), !,
  debug(sparql_db, 'SPARQL remote ~w is already set. No changes.', [Remote]).
sparql_add_remote(Remote, _Server1, _Port1, _Path1):-
  sparql_current_remote(Remote, _Server2, _Port2, _Path2), !,
  debug(
    sparql_db,
    'SPARQL remote ~w is already set DIFFERENTLY. First remove.',
    [Remote]
  ).
sparql_add_remote(Remote, Server, Port, Path):-
  with_mutex(sparql_db, assert_sparql_remote(Remote, Server, Port, Path)).

%! sparql_current_prefix(?Prefix:atom, ?IRI:iri) is nondet.

sparql_current_prefix(Prefix, IRI):-
  with_mutex(sparql_db, sparql_prefix(Prefix, IRI)).

sparql_current_remote(Remote, Server, Port, Path):-
  with_mutex(sparql_db, sparql_remote(Remote, Server, Port, Path)).

%! sparql_remove_prefix(+Prefix:atom) is det.

sparql_remove_prefix(Prefix):-
  with_mutex(
    sparql_db,
    (
      once(sparql_prefix(Prefix, _IRI1)),
      retractall_sparql_prefix(Prefix, _IRI2)
    )
  ).
sparql_remove_prefix(Prefix):-
  existence_error('SPARQL prefix', Prefix).

sparql_remove_remote(Remote):-
  with_mutex(
    sparql_db,
    (
      once(sparql_remote(Remote, Server, Port, Path)),
      retractall_sparql_remote(Remote, Server, Port, Path)
    )
  ).
sparql_remove_remote(Remote):-
  existence_error('SPARQL remote', Remote).
