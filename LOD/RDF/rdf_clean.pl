:- module(
  rdf_clean,
  [
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
@version 2013/03-2013/04, 2013/06, 2013/08-2013/09, 2014/01
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(codes_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(user_input)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf(rdf_read)).
:- use_module(xsd(xsd)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(rdf_split_literal(+,r,r,?,+)).
:- rdf_meta(rdf_strip_literal(+,+,r,r,?)).
:- rdf_meta(rdf_remove(r,r,r,?)).
:- rdf_meta(rdf_remove_datatype(r,r,r,?,?)).



rdf_split_literal(O1, S, P, G, Split):-
  findall(
    [S,P,LexicalForm,G],
    (
      rdf_string(S, P, LexicalForm, G),
      atomic_list_concat(Sublits, Split, LexicalForm), % split
      length(Sublits, Length),
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
rdf_split_string0(Split, S, P, OldLexicalForm, G):-
  atomic_list_concat(NewLexicalForms, Split, OldLexicalForm), % split
  forall(
    member(NewLexicalForm, NewLexicalForms),
    rdf_assert_string(S, P, NewLexicalForm, G)
  ),
  rdf_retractall_string(S, P, OldLexicalForm, G).

%! rdf_strip_literal(
%!   +Options:list(nvpair),
%!   +Strips:list(char),
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Graph:atom
%! ) is det.
% Strip RDF literals.

rdf_strip_literal(O1, Strips, S, P, G):-
  findall(
    [S,P,LexicalForm1,G],
    (
      rdf_string(S, P, LexicalForm1, G),
      strip_atom(Strips, LexicalForm1, LexicalForm2),
      LexicalForm1 \= LexicalForm2
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
  rdf_assert_string(S, P, NewLiteral, G),
  rdf_retractall_string(S, P, OldLiteral, G).


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

/*
% URL.
json_value_to_rdf(Graph, _, Individual, Predicate, url, Value1):- !,
  % Remove leading and trailing spaces.
  strip_atom([' '], Value1, Value2),

  (
    is_of_type(iri, Value2)
  ->
    Value3 = Value2
  ;
    atomic_concat('http://', Value2, Value3),
    is_of_type(iri, Value3)
  ->
    true
  ;
    format(atom(Msg), 'Value ~w is not a URL.', [Value2]),
    syntax_error(Msg)
  ),

  % Make sure there are no spaces!
  dcg_phrase(dcg_replace(space, percent_encoding(space)), Value3, Value4),
  (
    Value3 == Value4
  ->
    true
  ;
    debug(ckan, 'URI ~w is no IRI (contains spaces).', [Value3])
  ),

  % Image URL.
  (
    is_image_url(Value4)
  ->
    rdf_assert_image([], Individual, Predicate, Value4, Graph)
  ;
    rdf_assert(Individual, Predicate, Value4, Graph)
  ).
% Email.
json_value_to_rdf(Graph, Module, Individual, Predicate, email, Value1):- !,
  % Remove leading and trailing spaces.
  strip_atom([' '], Value1, Value2),

  (
    is_of_type(email, Value2)
  ->
    atomic_list_concat([mailto,Value2], ':', Value3)
  ;
    format(atom(Msg), 'Value ~w is not an e-mail address.', [Value2]),
    syntax_error(Msg),
    % For links to a contact form.
    json_value_to_rdf(Graph, Module, Individual, Predicate, url, Value2)
  ),
  rdf_assert(Individual, Predicate, Value3, Graph).
*/

