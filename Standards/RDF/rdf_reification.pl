:- module(
  rdf_reification,
  [
% DEBUG
    print_statement/2, % +Options:list(nvpair)
                       % +Statement:iri

% READING
    rdf_object/3, % ?Stmt:statement
                  % ?Object:onef([literal,resource])
                  % +Graph:graph
    rdf_predicate/3, % ?Stmt:statement
                     % ?Predicate:resource
                     % +Graph:graph
    rdf_statement/5, % ?Subject:onef([bnode,literal,resource])
                     % ?Predicate:resource
                     % ?Object:onef([literal,resource])
                     % +Graph:graph
                     % ?Stmt:statement
    rdf_subject/3, % ?Stmt:statement
                   % ?Subject:onef([bnode,literal,resource])
                   % +Graph:graph

% WRITING
    rdf_assert_object/3, % +Stmt:statement
                         % +Object:oneof([literal,resource])
                         % +Graph:graph
    rdf_assert_predicate/3, % +Stmt:statement
                            % +Predicate:resource
                            % +Graph:graph
    rdf_assert_statement/5, % +Subject:resource
                            % +Predicate:resource
                            % +Object:resource
                            % +Graph:graph
                            % -Stmt:statement
    rdf_assert_subject/3 % +Stmt:statement
                         % +Subject:resource
                         % +Graph:graph
  ]
).

/** <module> RDF reification

Reification for RDF. Both reading and writing.

@author Wouter Beek
@tbd Assess this module after reading the semantics standard for reification.
@version 2013/02, 2013/07, 2013/09
*/

:- use_module(generics(print_ext)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_name)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(print_statement(+,r)).

:- rdf_meta(rdf_object(r,r,?)).
:- rdf_meta(rdf_predicate(r,r,?)).
:- rdf_meta(rdf_statement(r,r,r,?,r)).
:- rdf_meta(rdf_subject(r,r,?)).

:- rdf_meta(rdf_assert_object(r,r,+)).
:- rdf_meta(rdf_assert_predicate(r,r,+)).
:- rdf_meta(rdf_assert_statement(r,r,r,+,r)).
:- rdf_meta(rdf_assert_subject(r,r,+)).



% DEBUG %

%! print_statement(+Options:list(nvpair), +Statement:iri) is det.
% The following options are defined:
%   * =|mode(+Mode:oneof([natlang,triple]))|=
%     The mode in which the statenent is printed.
%     Either `triple` (default) for an RDF triple representation
%     (according to module RDF_NAME),
%     or `natlang` for a natural language representation.

print_statement(O1, Stmt):-
  select_option(mode(Mode), O1, O2, triple),
  print_statement(Mode, O2, Stmt).

print_statement(natlang, _O1, Stmt):- !,
  rdf_statement(S, P, O, _G, Stmt),
  once(rdfs_label(S, SName)),
  once(rdfs_label(P, PName)),
  once(rdfs_label(O, OName)),
  print_collection([begin(''),end(''),separator(' ')], [SName,PName,OName]).
print_statement(triple, _O1, Stmt):- !,
  rdf_statement(S, P, O, _G, Stmt),
  rdf_triple_name(rdf(S,P,O), TripleName),
  write(TripleName).



% READING %

rdf_object(Stmt, Object, Graph):-
  rdf(Stmt, rdf:object, Object, Graph).

rdf_predicate(Stmt, Predicate, Graph):-
  rdf(Stmt, rdf:predicate, Predicate, Graph).

rdf_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_subject(Stmt, Subject, Graph),
  rdf_predicate(Stmt, Predicate, Graph),
  rdf_object(Stmt, Object, Graph).

rdf_subject(Stmt, Subject, Graph):-
  rdf(Stmt, rdf:subject, Subject, Graph).



% WRITING %

rdf_assert_object(Stmt, Object, Graph):-
  rdf_assert(Stmt, rdf:object, Object, Graph).

rdf_assert_predicate(Stmt, Predicate, Graph):-
  rdf_assert(Stmt, rdf:predicate, Predicate, Graph).

rdf_assert_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_statement(Subject, Predicate, Object, Graph, Stmt), !.
rdf_assert_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_bnode(Stmt),
  rdf_assert_individual(Stmt, rdf:'Statement', Graph),
  rdf_assert_subject(Stmt, Subject, Graph),
  rdf_assert_predicate(Stmt, Predicate, Graph),
  rdf_assert_object(Stmt, Object, Graph),
  rdf_assert(Subject, Predicate, Object, Graph).

rdf_assert_subject(Stmt, Subject, Graph):-
  rdf_assert(Stmt, rdf:subject, Subject, Graph).

