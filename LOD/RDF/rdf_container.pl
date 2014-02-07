:- module(
  rdf_container,
  [
% ALTERNATIVES
    rdf_alt/2, % ?Alt:uri
               % ?Graph:atom
    rdf_alt/3, % ?Alt:uri
               % -Contents:list(uri)
               % ?Graph:atom
% BAG
    rdf_bag/2, % ?Bag:uri
               % ?Graph:atom
    rdf_bag/3, % ?Bag:uri
               % -Contents:list(uri)
               % ?Graph:atom
% COLLECTION
    rdf_assert_collection_member/3, % +Collection:iri
                                    % +Member:iri
                                    % +Graph:atom
    rdf_collection/2, % ?Collection:uri
                      % ?Graph:atom
    rdf_collection/3, % ?Collection:uri
                      % -Contents:list(uri)
                      % ?Graph:atom
    rdf_container_membership_property/1, % ?Predicate:uri

% SEQUENCE
    rdf_seq/2, % ?Seq:uri
               % ?Graph:atom
    rdf_seq/3 % ?Seq:uri
              % -Contents:list(uri)
              % ?Graph:atom
  ]
).

/** <module> RDF_CONTAINER

Support for RDF containers (sequence, bag, and alternatives).

@author Wouter Beek
@tbd Add predicates for building containers.
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09, 2014/02
*/

:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_term)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

% ALTERNATIVES
:- rdf_meta(rdf_alt(r,?)).
:- rdf_meta(rdf_alt(r,-,?)).
% BAG
:- rdf_meta(rdf_bag(r,?)).
:- rdf_meta(rdf_bag(r,-,?)).
% COLLECTION
:- rdf_meta(rdf_collection(r,?)).
:- rdf_meta(rdf_collection(r,-,?)).
:- rdf_meta(rdf_container_membership_property(r)).
% SEQUENCE
:- rdf_meta(rdf_seq(r,?)).
:- rdf_meta(rdf_seq(r,-,?)).



% ALTERNATIVES %

%! rdf_alt(?Alt:uri, ?Graph:atom) is nondet.
% Alternative collections.
% No duplicates and unordered.

rdf_alt(Alt, G):-
  rdfs_individual_of(Alt, rdf:'Alt'),
  rdf_subject(G, Alt).

%! rdf_alt(?Alt:uri, -Contents:list(uri), ?Graph:atom) is nondet.

rdf_alt(Alt, Contents, G):-
  rdf_alt(Alt, G),
  rdf_collection_(Alt, Contents, G).



% BAG %

%! rdf_bag(-Bag:uri, +Graph:atom) is nondet.
% Returns bags in the given graph.
%
% Unordered & duplicated allowed.
%
% @arg Bag An RDF bag resource.
% @arg Graph The atomic name of a graph.

rdf_bag(Bag, G):-
  rdfs_individual_of(Bag, rdf:'Bag'),
  rdf_subject(G, Bag).

%! rdf_bag(+Bag:uri, -Contents:list(uri), +Graph:atom) is nondet.
% Returns bags and their contents in the given graph.
%
% @arg Bag An RDF bag.
% @arg Contents A list of resources.
% @arg Graph The atomic name of a graph.

rdf_bag(Bag, Contents, G):-
  rdf_bag(Bag, G),
  rdf_collection_(Bag, Contents, G).



% COLLECTION %

:- rdf_meta(rdf_assert_collection_member(r,r,+)).
rdf_assert_collection_member(Collection, Member, Graph):-
  rdf_collection(Collection, Contents, Graph),
  length(Contents, Length),
  Index is Length + 1,
  atomic_list_concat(['',Index], '_', LocalName),
  rdf_global_id(rdf:LocalName, MembershipRelation),
  rdf_assert(Collection, MembershipRelation, Member, Graph).

rdf_collection(Collection, G):-
  rdf_alt(Collection, G), !.
rdf_collection(Collection, G):-
  rdf_bag(Collection, G), !.
rdf_collection(Collection, G):-
  rdf_seq(Collection, G), !.

rdf_collection(Collection, Contents, G):-
  rdf_alt(Collection, G), !,
  rdf_collection_(Collection, Contents, G).
rdf_collection(Collection, Contents, G):-
  rdf_bag(Collection, G), !,
  rdf_collection_(Collection, Contents, G).
rdf_collection(Collection, Contents, G):-
  rdf_seq(Collection, G), !,
  rdf_collection_(Collection, Contents, G).

%! rdf_collection_(
%!   ?Collection:uri,
%!   ?Contents:list(uri),
%!   ?Graph:atom
%! ) is nondet.

rdf_collection_(Collection, Contents, G):-
  findall(
    Content,
    (
      rdf(Collection, ContainerMembershipProperty, Content, G),
      rdf_container_membership_property(ContainerMembershipProperty)
    ),
    Contents
  ).

%! rdf_container_membership_property(+Predicate:uri) is semidet.
%! rdf_container_membership_property(-Predicate:uri) is nondet.
% Succeeds if =Predicate= is a container membership property.

rdf_container_membership_property(P):-
  must_be(nonvar, P), !,
  rdf_global_id(rdf:Name, P),
  atom_concat('_', Atom, Name),
  atom_number(Atom, Number),
  must_be(nonneg, Number).
rdf_container_membership_property(P):-
  between(1, inf, Integer),
  format(atom(Name), '_~w', [Integer]),
  rdf_global_id(rdf:Name, P).



% SEQUENCE %

%! rdf_seq(?Sequence:or([bnode,iri]), ?Graph:atom) is nondet.
% Ordered.

rdf_seq(Seq, G):-
  rdfs_individual_of(Seq, rdf:'Seq'),
  rdf_subject(G, Seq).

rdf_seq(Seq, Contents, G):-
  rdf_seq(Seq, G),
  rdf_collection(Seq, Contents, G).

