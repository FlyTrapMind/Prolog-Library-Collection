:- module(
  rdf_dcg,
  [
    rdf_term//1 % ?PL_Term
  ]
).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(uri(rfc3987_dcg)).
:- use_module(xml(xml_namespace)).



% TERM %

rdf_term(Type) -->
  rdf_blank_node(Type).
rdf_term(Type) -->
  rdf_literal(Type).
rdf_term(Type) -->
  rdf_iri(Type).



% BLANK NODE %

rdf_blank_node(BNode) -->
  {var(BNode)}, !,
  dcg_multi1(underscore, 2, [H1,H2]),
  dcg_all([output_format(codes)], T),
  {atom_codes(BNode, [H1,H2|T])}.
rdf_blank_node(BNode1) -->
  {atom(BNode1), sub_atom(BNode1, 0, 2, _, BNode2)},
  dcg_multi(underscore, 2),
  atom(BNode2).



% LITERAL %

rdf_language_tag(Language) -->
  {var(Language)}, !,
  dcg_all([output_format(atom)], Language).
rdf_language_tag(Language) -->
  atom(Language).

rdf_literal(Type) -->
  rdf_plain_literal(Type).
rdf_literal(Type) -->
  rdf_typed_literal(Type).

rdf_plain_literal(literal(lang(Language,Value))) -->
  rdf_simple_literal(Value),
  "@",
  rdf_language_tag(Language).
rdf_plain_literal(literal(Value)) -->
  {(var(Value), ! ; atom(Value))},
  rdf_simple_literal(Value).

rdf_simple_literal(Value) -->
  {var(Value)}, !,
  dcg_between(
    double_quote,
    dcg_until([end_mode(exclusive),output_format(atom)], double_quote, Value)
  ).
rdf_simple_literal(Value) -->
  dcg_between(double_quote, atom(Value)).

rdf_typed_literal(literal(type(Datatype,Value))) -->
  {maplist(var, [Datatype,Value])}, !,
  dcg_between(
    double_quote,
    dcg_until([end_mode(exclusive),output_format(atom)], double_quote, Value)
  ),
  dcg_multi(caret, 2),
  rdf_iri(Datatype).
rdf_typed_literal(literal(type(Datatype,Value))) -->
  dcg_between(double_quote, atom(Value)),
  dcg_multi(caret, 2),
  rdf_iri(Datatype).



% IRI %

rdf_iri(IRI) -->
  {var(IRI)},
  dcg_until([end_mode(exclusive),output_format(atom)], colon, Prefix),
  colon,
  dcg_all([output_format(atom)], Postfix),
  {(
    xml_current_namespace(Prefix, _)
  ->
    IRI = Prefix:Postfix
  ;
    atomic_list_concat([Prefix,Postfix], ':', IRI)
  )}.
rdf_iri(IRI) -->
  (
    {IRI = Prefix:Postfix}
  ->
    {(xml_current_namespace(Prefix, _), ! ; existence_error('XML namespace',Prefix))},
    atom(Prefix),
    colon,
    atom(Postfix)
  ;
    {atom(IRI)},
    atom(IRI)
  ).

