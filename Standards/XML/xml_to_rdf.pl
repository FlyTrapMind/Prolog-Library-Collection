:- module(
  xml_to_rdf,
  [
    create_resource/7, % +XML_DOM:list
                       % +XML_PrimaryProperties:list(atom)
                       % :XML2RDF_Translation
                       % +Class:iri
                       % +Graph:atom
                       % -Subject:or([bnode,iri])
                       % -XML_RemainingDOM:list
    create_triples/6 % +XML_DOM:list,
                     % +XML_Properties:list(atom),
                     % :XML2RDF_Translation,
                     % +RDF_Subject:or([bnode,iri]),
                     % +RDF_Graph:atom,
                     % -XML_RemainingDOM:list
  ]
).

/** <module> XML to RDF

Converts XML DOMs to RDF graphs.

@author Wouter Beek
@version 2013/06, 2013/09
*/

:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_read)).
:- use_module(xsd(xsd)).

:- meta_predicate(create_resource(+,+,3,+,+,-,-)).
:- meta_predicate(create_triples(+,+,3,+,+,-)).
:- meta_predicate(rdf_property_trans(+,3,+,+,+)).
:- meta_predicate(rdf_subject_trans(+,+,3,+,+)).

:- rdf_meta(create_resource(+,+,:,r,+,-,-)).
:- rdf_meta(create_triples(+,+,:,r,+,-)).
:- rdf_meta(rdf_property_trans(+,:,r,+,+)).
:- rdf_meta(rdf_subject_trans(+,+,:,r,+)).

:- debug(xml_to_rdf).



%! create_resource(
%!   +XML_DOM:list,
%!   +XML_PrimaryProperties:list(atom),
%!   :XML2RDF_Translation,
%!   +Class:iri,
%!   +Graph:atom,
%!   -Subject:or([bnode,iri]),
%!   -XML_RemainingDOM:list
%! ) is det.

% The resource already exists.
create_resource(DOM, PrimaryPs, Trans, _C, G, S, DOM):-
  rdf_subject_trans(DOM, PrimaryPs, Trans, S, G), !,
  debug(
    xml_to_rdf,
    'The resource for the following XML DOM already exists: ~w',
    [DOM]
  ).
% The resource does not yet exist and it created.
create_resource(DOM1, PrimaryPs, Trans, C, G, S, DOM2):-
  rdf_global_id(Ns:CName1, C),
  flag(CName1, Id, Id + 1),

  % The convention is that only RDF terms denoting RDFS classes
  % start with an uppercase letter.
  downcase_atom(CName1, CName2),

  atomic_list_concat([CName2,Id], '_', SName),
  rdf_global_id(Ns:SName, S),
  rdf_assert_individual(S, C, G),

  create_triples(DOM1, PrimaryPs, Trans, S, G, DOM2).

%! create_triple(
%!   +RDF_Subject:or([bnode,iri]),
%!   +RDF_Predicate:iri,
%!   +ObjectType:atom,
%!   +XML_Content,
%!   +RDF_Graph:atom
%! ) is det.

create_triple(S, P, literal, Content, G):- !,
  rdf_assert_literal(S, P, Content, G).
create_triple(S, P, DName, Content, G):-
  xsd_datatype(DName, _), !,
  rdf_assert_datatype(S, P, DName, Content, G).
create_triple(S, P, _, Content, G):-
  rdf_assert(S, P, Content, G).

%! create_triples(
%!   +XML_DOM:list,
%!   +XML_Properties:list(atom),
%!   :XML2RDF_Translation,
%!   +RDF_Subject:or([bnode,iri]),
%!   +RDF_Graph:atom,
%!   -XML_RemainingDOM:list
%! ) is nondet.

% The XML DOM is fully processed.
create_triples([], _Ps, _Trans, _S, _G, []):- !.
% The XML properties are all processed.
create_triples(DOM, [], _Trans, _S, _G, DOM):- !.
% Process an XML element.
create_triples(DOM1, Ps1, Trans, S, G, RestDOM):-
  % Process only properties that are allowed according to the filter.
  select(element(XML_P, _, Content1), DOM1, DOM2),
  update_property_filter(Ps1, XML_P, Ps2), !,
  
  (
    % XML element with no content.
    Content1 == [], !
  ;
    % XML element with content.
    Content1 = [Content2],
    call(Trans, XML_P, RDF_P, RDF_O_Type),
    create_triple(S, RDF_P, RDF_O_Type, Content2, G)
  ),
  create_triples(DOM2, Ps2, Trans, S, G, RestDOM).
% Neither the DOM nor the propery filter is empty.
% This means that some properties in the filter are optional.
create_triples(DOM, _Ps, _Trans, _S, _G, DOM).

update_property_filter(Ps1, _XML_P, _Ps2):-
  var(Ps1), !.
update_property_filter(Ps1, XML_P, Ps2):-
  selectchk(XML_P, Ps1, Ps2).

%! rdf_object_trans(
%!   ?RDF_Subject:or([bnode,iri]),
%!   +RDF_Predicate:iri,
%!   +RDF_ObjectType:atom,
%!   +XML_Content:atom,
%!   +RDF_Graph:atom
%! ) is nondet.

% Succeeds if there is a subject resource with the given predicate term
% and an object term that is the literal value of the given XML content.
rdf_object_trans(S, P, literal, Content, G):- !,
  rdf_literal(S, P, Content, G).
% Succeeds if there is a subject resource with the given predicate term
% and an object term that translates to the same value as
% the given XML content.
rdf_object_trans(S, P, DName, Content, G):-
  xsd_datatype(DName, _), !,
  rdf_datatype(S, P, DName, Content, G).

%! rdf_property_trans(
%!   +XML_DOM:list,
%!   :XML2RDF_Translation,
%!   +RDF_Subject:or([bnode,iri]),
%!   +XML_Property:atom,
%!   +RDF_Graph:atom
%! ) is nondet.
% Reads a predicate from the DOM and checks whether
% it is already present for some subject resource
% under the given translation.

rdf_property_trans(DOM, Trans, S, XML_P, G):-
  memberchk(element(XML_P, _, [Content]), DOM),
  call(Trans, XML_P, RDF_P, O_Type),
  rdf_object_trans(S, RDF_P, O_Type, Content, G).

%! rdf_subject_trans(
%!   +XML_DOM:list,
%!   +XML_Properties:list(atom),
%!   :XML2RDF_Translation,
%!   ?RDF_Subject:or([bnode,iri]),
%!   +RDF_Graph:atom
%! ) is nondet.

% Returns a resource that has the given XML properties set to
% the object values that occur in the XML DOM.
rdf_subject_trans(DOM, [P|Ps], Trans, S, G):-
  rdf_property_trans(DOM, Trans, S, P, G),
  forall(
    member(P_, Ps),
    rdf_property_trans(DOM, Trans, S, P_, G)
  ).

