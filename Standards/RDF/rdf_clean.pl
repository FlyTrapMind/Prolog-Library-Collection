:- module(
  rdf_clean,
  [
    rdf_duplicate/5, % ?Subject:oneof([bnode,uri])
                     % ?Predicate:uri
                     % ?Object:oneof([bnode,literal,uri])
                     % ?Graph1:atom
                     % ?Graph2:atom
    rdf_expand_namespace/4, % ?Subject:oneof([atom,bnode,uri])
                            % ?Predicate:oneof([atom,uri])
                            % ?Object:oneof([atom,bnode,literal,uri])
                            % ?Graph:atom
% DATATYPES %
    rdf_convert_datatype/6, % +Subject:oneof([bnode,uri])
                            % +Predicate:uri
                            % +FromDatatypeName:atom
                            % +FromValue
                            % +ToDatatypeName:atom
                            % +Graph:atom
    rdf_literal_to_datatype/4, % +Subject:oneof([bnode,uri])
                               % +Predicate:uri
                               % +DatatypeName:atom
                               % +Graph:atom
% LITERALS %
    rdf_literal_to_uri/4, % ?Subject:oneof([bnode,uri])
                          % ?Predicate:uri
                          % +Namespace:atom
                          % ?Graph:atom
    rdf_split_literal/5, % +Options:list(nvpair)
                         % ?Subject:oneof([bnode,uri])
                         % ?Predicate:uri
                         % ?Graph:atom
                         % +Split:atom
    rdf_strip_literal/5, % +Options:list(nvpair)
                         % +Strips:list(char)
                         % ?Subject:oneof([bnode,uri])
                         % ?Predicate:uri
                         % ?Graph:atom
% REMOVAL %
    rdf_remove/4, % ?Subject:oneof([bnode,uri])
                  % ?Predicate:uri
                  % ?Object:oneof([bnode,literal,uri])
                  % ?Graph:atom
    rdf_remove_datatype/5 % ?Subject:oneof([bnode,uri])
                          % ?Predicate:uri
                          % ?Datatype:atom
                          % ?Value
                          % ?Graph:atom
  ]
).

/** <module> RDF clean

Predicates that allow RDF graphs to be cleaned in a controlled way.

@author Wouter Beek
@version 2013/03-2013/04, 2013/06, 2013/08-2013/09
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(user_input)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(rdf(rdf_read)).
:- use_module(xsd(xsd)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(rdf_duplicate(r,r,r,?,?)).
:- rdf_meta(rdf_expand_namespace(r,r,r,?)).
% DATATYPES %
:- rdf_meta(rdf_convert_datatype(r,r,+,+,+,+)).
% LITERALS %
:- rdf_meta(rdf_literal_to_uri(r,r,+,?)).
:- rdf_meta(rdf_split_literal(+,r,r,?,+)).
:- rdf_meta(rdf_strip_literal(+,+,r,r,?)).
% REMOVAL %
:- rdf_meta(rdf_remove(r,r,r,?)).
:- rdf_meta(rdf_remove_datatype(r,r,r,?,?)).



%! rdf_duplicate(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:oneof([bnode,literal,uri]),
%!   ?Graph1:atom,
%!   ?Graph2:atom
%! ) is nondet.
% Duplicate triples, that occur in at least two graphs.

rdf_duplicate(S, P, O, G1, G2):-
  rdf(S, P, O, G1:_),
  rdf(S, P, O, G2:_),
  G1 \== G2.

rdf_expand_namespace(BNode, BNode):-
  rdf_is_bnode(BNode), !.
rdf_expand_namespace(
  literal(lang(Language, Literal)),
  literal(lang(Language, Literal))
):- !.
% Datatypes in typed literals are treaded in a special way.
rdf_expand_namespace(literal(type(Atom, Value)), literal(type(URI, Value))):-
  rdf_expand_namespace(Atom, URI).
% No namespace.
rdf_expand_namespace(literal(Literal), literal(Literal)):- !.
% Already a URI.
rdf_expand_namespace(URI, URI):-
  is_uri(URI), !.
% An atom that can be converted to a URI.
rdf_expand_namespace(Atom, URI):-
  split_atom_exclusive(':', Atom, [Namespace, LocalName]),
  rdf_global_id(Namespace:LocalName, URI).

%! rdf_expand_namespace(
%!   ?Subject:oneof([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:oneof([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is det.
% Expands namespaces that currently occur as atoms.
%
% This was used for several RDF files from the OAEI where the datatypes
% of typed literals would sometimes be 'xsd:float' instea of 'xsd':'float'.

rdf_expand_namespace(S1, P1, O1, G):-
  findall(
    [S1,P1,O1,G],
    (
      rdf(S1, P1, O1, G),
      (
        rdf_expand_namespace(S1, S2),
        S1 \== S2
      ;
        rdf_expand_namespace(P1, P2),
        P1 \== P2
      ;
        rdf_expand_namespace(O1, O2),
        O1 \== O2
      )
    ),
    Tuples
  ),
  user_interaction(
    [],
    'EXPAND-NAMESPACE',
    rdf_expand_namespace0,
    ['Subject','Predicate','Object','Graph'],
    Tuples
  ).
:- rdf_meta(rdf_expand_namespace0(r,r,r,?)).
rdf_expand_namespace0(S1, P1, 1, G):-
  maplist(rdf_expand_namespace, [S1,P1,O1], [S2,P2,O2]),
  rdf_retractall(S1, P1, O1, G),
  rdf_assert(S2, P2, O2, G).



% DATATYPES %

%! rdf_convert_datatype(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +FromDatatypeName:iri,
%!   +FromValue,
%!   +ToDatatypeName:iri,
%!   +Graph:atom
%! ) is det.

rdf_convert_datatype(S, P, FromDatatypeName, FromValue, ToDatatypeName, G):-
  forall(
    rdf_datatype(S, P, FromDatatypeName, FromValue, G),
    (
      xsd_datatype(FromDatatypeName, FromDatatype),
      xsd_datatype(ToDatatypeName, ToDatatype),
      xsd_convert_datatype(FromDatatype, FromValue, ToDatatype, ToValue),
      rdf_assert_datatype(S, P, ToDatatype, ToValue, G),
      rdf_retractall_datatype(S, P, FromDatatypeName, G)
    )
  ).

%! rdf_literal_to_datatype(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Datatype:iri,
%!   +Graph:atom
%! ) is det.

rdf_literal_to_datatype(S, P, D, G):-
  findall(
    [S,P,Lit,G],
    rdf_literal(S, P, Lit, G),
    Tuples
  ),
  format(atom(OperationName), 'LITERAL-TO-DATATYPE(~w)', [D]),
  user_interaction(
    [],
    OperationName,
    rdf_literal_to_datatype0(D),
    ['Subject','Predicate','Literal','Graph'],
    Tuples
  ).
:- rdf_meta(rdf_literal_to_datatype0(r,r,r,+,+)).
rdf_literal_to_datatype0(D, S, P, Lit, G):-
  % If the literal belongs to the lexical space of the datatype,
  % then it is mapped onto its value and then back to the canonical
  % lexical for that value.
  % Otherwise, we try to parse the literal as a value in the value space
  % of the datatype.
  (
    xsd_lexicalCanonicalMap(D, Lit, LEX), !
  ;
    xsd_canonicalMap(D, Lit, LEX)
  ),
  rdf_assert_datatype(S, P, D, LEX, G),
  rdf_retractall_literal(S, P, Lit, G).



% LITERALS %

%! rdf_literal_to_uri(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Namespace:atom,
%!   +Graph:atom
%! ) is det.

rdf_literal_to_uri(S, P, Ns, G):-
  xml_current_namespace(Ns, _),
  findall(
    [S,P,Lit,G],
    rdf_literal(S, P, Lit, G),
    Tuples
  ),
  format(atom(OperationName), 'LITERAL-TO-URI(~w)', [Ns]),
  user_interaction(
    [],
    OperationName,
    rdf_literal_to_uri0(Ns),
    ['Subject','Predicate','Literal','Graph'],
    Tuples
  ).
:- rdf_meta(rdf_literal_to_uri(+,r,r,+,+)).
rdf_literal_to_uri0(Ns, S, P, Lit, G):-
  rdf_global_id(Ns:Lit, O),
  rdf_assert(S, P, O, G),
  rdf_retractall_literal(S, P, Lit, G).

rdf_split_literal(O1, S, P, G, Split):-
  findall(
    [S,P,Lit,G],
    (
      rdf_literal(S, P, Lit, G),
      split_atom_exclusive(Split, Lit, Splits),
      length(Splits, Length),
      Length > 1
    ),
    Tuples
  ),
  user_interaction(
    O1,
    'SPLIT-RDF-DATATYPE-STRING',
    rdf_split_string0(Split),
    ['Subject','Predicate','Literal','Graph'],
    Tuples
  ).
:- rdf_meta(rdf_split_string0(+,r,r,+,+)).
rdf_split_string0(Split, S, P, OldLit, G):-
  split_atom_exclusive(Split, OldLit, NewLits),
  forall(
    member(NewLit, NewLits),
    rdf_assert_literal(S, P, NewLit, G)
  ),
  rdf_retractall_literal(S, P, OldLit, G).

%! rdf_strip_literal(
%!   +Options:list(nvpair),
%!   +Strips:list(char),
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Graph:atom
%! ) is det.
% Strip RDF string datatypes.

rdf_strip_literal(O1, Strips, S, P, G):-
  findall(
    [S,P,Literal1,G],
    (
      rdf_literal(S, P, Literal1, G),
      strip_atom(Strips, Literal1, Literal2),
      Literal1 \= Literal2
    ),
    Tuples
  ),
  user_interaction(
    O1,
    'STRIP-RDF-DATATYPE-STRING',
    rdf_strip_literal0(Strips),
    ['Subject','Predicate','Literal','Graph'],
    Tuples
  ).
:- rdf_meta(rdf_strip_literal0(+,r,r,+,+)).
rdf_strip_literal0(Strips, S, P, OldLiteral, G):-
  strip_atom(Strips, OldLiteral, NewLiteral),
  rdf_assert_literal(S, P, NewLiteral, G),
  rdf_retractall_literal(S, P, OldLiteral, G).



% REMOVAL %

%! rdf_remove(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:oneof([bnode,literal,uri]),
%!   ?Graph:atom
%! ) is det.
% Clean RDF triples with explicit user-consent.

rdf_remove(S, P, O, G):-
  findall(
    [S,P,O,G],
    rdf(S, P, O, G),
    Tuples
  ),
  user_interaction(
    [],
    'REMOVE-RDF-TRIPLE',
    rdf_retractall,
    ['Subject','Predicate','Object','Graph'],
    Tuples
  ).

%! rdf_remove_datatype(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Datatype:atom,
%!   ?Value,
%!   ?Graph:atom
%! ) is det.
% Clean RDF datatype triples with explicit user-consent.

rdf_remove_datatype(S, P, Datatype, Value, G):-
  findall(
    [S,P,Datatype,Value,G],
    rdf_datatype(S, P, Datatype, Value, G),
    Tuples
  ),
  user_interaction(
    [],
    'REMOVE-RDF-DATATYPE-TRIPLE',
    rdf_retractall_datatype0,
    ['Subject','Predicate','Datatype','Value','Graph'],
    Tuples
  ).
rdf_retractall_datatype0(S, P, Datatype, _Value, G):-
  rdf_retractall_datatype(S, P, Datatype, G).
