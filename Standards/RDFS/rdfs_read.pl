:- module(
  rdfs_read,
  [
% ALTS
    rdfs_alt/2, % ?Alt:uri
                % ?Graph:atom
    rdfs_alt/3, % ?Alt:uri
                % -Contents:list(uri)
                % ?Graph:atom

% BAGS
    rdfs_bag/2, % ?Bag:uri
                % ?Graph:atom
    rdfs_bag/3, % ?Bag:uri
                % -Contents:list(uri)
                % ?Graph:atom

% COLLECTIONS
    rdfs_collection/2, % ?Collection:uri
                      % ?Graph:atom
    rdfs_collection/3, % ?Collection:uri
                      % -Contents:list(uri)
                      % ?Graph:atom

% DOMAIN & RANGE
    rdfs_domain/3, % ?Property:uri
                   % ?Domain:uri
                   % ?Graph:atom
    rdfs_range/3, % ?Property:uri
                  % ?Range:uri
                  % ?Graph:atom

% LABELS
    rdfs_preferred_label/3, % ?RDF_Term:oneof([bnode,uri])
                            % ?Languages:list(atom)
                            % ?PreferredLabel:atom
    rdfs_list_label/3, % +List:uri
                       % +Label:atom
                       % -Element:uri

% RDF-HAS
    rdfs/4, % ?Subject:oneof([bnode,uri])
            % ?Predicate:uri
            % ?Object:uri
            % ?Graph:atom

% SEQUENCES
    rdfs_seq/2, % ?Seq:uri
                % ?Graph:atom
    rdfs_seq/3 % ?Seq:uri
               % -Contents:list(uri)
               % ?Graph:atom
  ]
).

/** <module> RDFS read

Predicates for reading/writing RDF lists.

An RDF list is taken to be a resource that occurs in the subject position
on the =|rdf:first|= and of the =|rdf:rest|= predicates.

# rdfs:subClassOf

~~~{.pl}
rdfs_subclass(X, Y, G):-
  rdfs_subclass0(X, Y, G),
  % We add the restriction that both resources are classes.
  once(rdfs_individual(X, rdfs:'Class', G)),
  once(rdfs_individual(Y, rdfs:'Class', G)).

rdfs_subclass0(X, X, _G).
rdfs_subclass0(X, Y, G):-
  rdf(X, rdfs:subClassOf, Z, _SameOrOtherG),
  rdfs_subclass0(Z, Y, G).
~~~

# rdfs:subPropertyOf

~~~{.pl}
rdfs_subproperty(X, Y, G):-
  rdfs_subproperty0(X, Y, G),
  % We add the restriction that both resources are properties.
  rdfs_individual(X, rdf:'Property', G),
  rdfs_individual(Y, rdf:'Property', G).

rdfs_subproperty0(X, X, _G).
rdfs_subproperty0(X, Y, G):-
  rdf(X, rdfs:subPropertyOf, Z, G),
  rdfs_subproperty0(Z, Y, G).
~~~

# rdf:type

~~~{.pl}
%! rdfs_individual(?Individual:uri, ?Class:uri, ?Graph:atom) is nondet.
% Individual and class pairs.
%
% @param Individual An instance resource.
% @param Class A class resource.
% @param Graph The atomic name of a graph.

% We make the memberhip of RDFS class to itself explicit because otherwise
% the class checks in method rdfs_subclass_of/2 cause rdfs_individual/3 to go
% into a loop.

rdfs_individual(rdfs:'Class', rdfs:'Class', _G).
rdfs_individual(X, Y, G):-
  nonvar(X),
  !,
  rdf(X, rdf:type, Z, G),
  rdfs_subclass_of(Z, Y),
  \+ (rdf_global_id(rdfs:'Class', X), rdf_global_id(rdfs:'Class', Y)).
rdfs_individual(X, Y, G):-
  rdfs_subclass_of(Z, Y),
  rdf(X, rdf:type, Z, G),
  \+ (rdf_global_id(rdfs:'Class', X), rdf_global_id(rdfs:'Class', Y)).
~~~

@author Wouter Beek
@author Sander Latour
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07
*/

:- use_module(generics(db_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_term)).

% ALTS
:- rdf_meta(rdfs_alt(r,?)).
:- rdf_meta(rdfs_alt(r,-,?)).
% BAGS
:- rdf_meta(rdfs_bag(r,?)).
:- rdf_meta(rdfs_bag(r,-,?)).
% COLLECTIONS
:- rdf_meta(rdfs_collection(r,?)).
:- rdf_meta(rdfs_collection(r,-,?)).
% DOMAIN & RANGE
:- rdf_meta(rdfs_domain(r,r,?)).
:- rdf_meta(rdfs_range(r,r,?)).
% LABELS
:- rdf_meta(rdfs_preferred_label(r,+,-)).
:- rdf_meta(rdfs_list_label(r,+,-)).
% RDF-HAS
:- rdf_meta(rdfs(r,r,r,?)).
% SEQUENCES
:- rdf_meta(rdfs_seq(r,?)).
:- rdf_meta(rdfs_seq(r,-,?)).

:- db_add_novel(user:prolog_file_type(rdf, rdf)).



% ALTS %

%! rdfs_alt(?Alt:uri, ?Graph:atom) is nondet.
% Alternative collections.
% No duplicates and unordered.

rdfs_alt(Alt, Graph):-
  rdfs_individual_of(Alt, rdf:'Alt'),
  rdf_subject(Graph, Alt).

%! rdfs_alt(?Alt:uri, -Contents:list(uri), ?Graph:atom) is nondet.

rdfs_alt(Alt, Contents, Graph):-
  rdfs_alt(Alt, Graph),
  rdf_collection0(Alt, Contents, Graph).



% BAGS %

%! rdfs_bag(-Bag:uri, +Graph:atom) is nondet.
% Returns bags in the given graph.
%
% @param Bag An RDF bag resource.
% @param Graph The atomic name of a graph.

rdfs_bag(Bag, Graph):-
  rdfs_individual_of(Bag, rdf:'Bag'),
  rdf_subject(Graph, Bag).

%! rdfs_bag(+Bag:uri, -Contents:list(uri), +Graph:atom) is nondet.
% Returns bags and their contents in the given graph.
%
% @param Bag An RDF bag.
% @param Contents A list of resources.
% @param Graph The atomic name of a graph.

rdfs_bag(Bag, Contents, Graph):-
  rdfs_bag(Bag, Graph),
  rdf_collection0(Bag, Contents, Graph).



% COLLECTIONS %

%! container_membership_property(+Predicate:uri) is semidet.
% Succeeds if =Predicate= is a container membership property.

container_membership_property(P):-
  atom_concat('_:', N, P),
  catch(
    atom_number(N, I),
    _Exception,
    fail
  ),
  integer(I),
  I > 0.

rdfs_collection(Collection, Graph):-
  rdfs_alt(Collection, Graph),
  !.
rdfs_collection(Collection, Graph):-
  rdfs_bag(Collection, Graph),
  !.
rdfs_collection(Collection, Graph):-
  rdfs_seq(Collection, Graph),
  !.

rdfs_collection(Collection, Contents, Graph):-
  rdfs_alt(Collection, Graph),
  !,
  rdf_collection0(Collection, Contents, Graph).
rdfs_collection(Collection, Contents, Graph):-
  rdfs_bag(Collection, Graph),
  !,
  rdf_collection0(Collection, Contents, Graph).
rdfs_collection(Collection, Contents, Graph):-
  rdfs_seq(Collection, Graph),
  !,
  rdf_collection0(Collection, Contents, Graph).

%! rdf_collection0(
%!   ?Collection:uri,
%!   ?Contents:list(uri),
%!   ?Graph:atom
%! ) is nondet.

rdf_collection0(Collection, Contents, Graph):-
  findall(
    Content,
    (
      rdf(Collection, ContainerMembershipProperty, Content, Graph),
      container_membership_property(ContainerMembershipProperty)
    ),
    Contents
  ).



% DOMAIN & RANGE

rdfs_domain(Property1, Domain, Graph):-
  rdfs_subproperty_of(Property1, Property2),
  rdf(Property2, rdfs:domain, Domain, Graph).

rdfs_range(Property1, Range, Graph):-
  rdfs_subproperty_of(Property1, Property2),
  rdf(Property2, rdfs:range, Range, Graph).



% LABELS %

%! rdfs_preferred_label(
%!   ?RDF_Term:oneof([bnode,uri]),
%!   ?Language:atom,
%!   ?Label:atom
%! ) is nondet.
% Multiple labels are returned (nondet) in a descending preference order.

% If the preferred language is not available,
% then we look for an arbitrary other language.
rdfs_preferred_label(RDF_Term, [], Label):- !,
  rdfs_label(RDF_Term, _OtherLanguage, Label).
% Look for the preferred languages, in order of occurrence in the list.
rdfs_preferred_label(RDF_Term, [H|_T], PreferredLabel):-
  rdfs_preferred_label_(RDF_Term, H, PreferredLabel), !.
% Next language...
rdfs_preferred_label(RDF_Term, [_H|T], PreferredLabel):- !,
  rdfs_preferred_label_(RDF_Term, T, PreferredLabel).
rdfs_preferred_label(RDF_Term, Language, Label):-
  \+ is_list(Language), !,
  rdfs_preferred_label(RDF_Term, [Language], Label).

% Ensure that given labels are atoms.
rdfs_preferred_label_(RDF_Term, Language, Label1):-
  nonvar(Label1), \+ atom(Label1), !,
  term_to_atom(Label1, Label2),
  rdfs_preferred_label_(RDF_Term, Language, Label2).
% Labels with the given language code are preferred.
rdfs_preferred_label_(RDF_Term, Language, Label):-
  rdfs_label(RDF_Term, Language, Label), !.

%! rdfs_list_label(+RDF_List:uri, +Label:atom, -Element:uri) is nondet.
% Returns RDF list elements that have the given label.

rdfs_list_label(RDF_List, Label, Element):-
  rdf_list_first(RDF_List, First),
  rdfs_list_label0(First, Label, Element).

rdfs_list_label0(Element, Label, Element):-
  rdfs_label(Element, Label), !.
rdfs_list_label0(Element, Label, Element0):-
  rdf_list_next(Element, NextElement),
  rdfs_list_label0(NextElement, Label, Element0).



% RDF-HAS %

rdfs(Subject, Predicate, Object, Graph):-
  rdf(Subject, Predicate0, Object, Graph),
  rdfs_subproperty_of(Predicate0, Predicate).



% SEQUENCES %

rdfs_seq(Seq, Graph):-
  rdfs_individual_of(Seq, rdf:'Seq'),
  rdf_subject(Graph, Seq).

rdfs_seq(Seq, Contents, Graph):-
  rdfs_seq(Seq, Graph),
  rdfs_collection(Seq, Contents, Graph).

