:- module(
  xml_entities,
  [
    xml_entities//3, % -Trees:list(compound)
                     % :DCG_Namespace
                     % +EntityRules:list(dcg)
    xml_entity//3, % :DCG_Namespace
                   % :DCG_Name
                   % ?DCG_Attributes
    xml_header//4 % -Tree:compound
                  % :DCG_Namespace
                  % ?Version:compound
                  % ?Standalone:boolean
  ]
).

/** <module> XML_ENTITIES

DCG rules for XML entities.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(xml(xml_attributes)).
:- use_module(xml(xml_datatypes)).

:- meta_predicate(xml_entities(-,//,//,?,?)).
:- meta_predicate(xml_entities(-,//,+,+,?,?)).
:- meta_predicate(xml_entity(//,//,?,?)).
:- meta_predicate(xml_entity(//,//,//,?,?)).
:- meta_predicate(xml_entity(//,//,//,//,?,?)).
:- meta_predicate(xml_entity(//,//,//,//,//,?,?)).
:- meta_predicate(xml_entity_q(//,//,?,?)).
:- meta_predicate(xml_entity_q(//,//,//,?,?)).
:- meta_predicate(xml_header(-,//,?,?,?,?)).



xml_entities(Trees, DCG_Namespace, Mod:L) -->
  xml_entities(Trees, DCG_Namespace, Mod, L).

xml_entities([], _DCG_Namespace, _Mod, []) --> [].
xml_entities([Tree], DCG_Namespace, Mod, [H]) -->
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [Tree,DCG_Namespace|Args]).
xml_entities([Tree|Trees], DCG_Namespace, Mod, [H|T]) -->
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [Tree,DCG_Namespace|Args]),
  xml_entities(Trees, DCG_Namespace, Mod, T).

%! xml_entity(:DCG_Namespace, :DCG_Name, +DCG_Attributes:list(dcg))//
% Processes a regular XML entity (i.e., one that does not use
% question marks in its tags).

xml_entity(DCG_Namespace, DCG_Name, DCG_Attributes) -->
  xml_entity(
    less_than_sign,
    DCG_Namespace,
    DCG_Name,
    DCG_Attributes,
    (forward_slash, greater_than_sign)
  ).

%! xml_entity(
%!   :DCG_Open,
%!   :DCG_Namespace,
%!   :DCG_Name,
%!   :DCG_Attributes,
%!   :DCG_Close
%! )//
% Processes generic XML entities, with explicitly set opening and closing
% tags. This is normally called via xml_entity//3 or xml_entity_q//3.

xml_entity(DCG_Open, DCG_Namespace, DCG_Name, DCG_Attributes, DCG_Close) -->
  DCG_Open,
  xml_namespaced_name(DCG_Namespace, DCG_Name),
  space,
  dcg_list(DCG_Attributes, space),
  DCG_Close.

%! xml_entity_q(:DCG_Name, :DCG_Attributes)//
% @see Like xml_entity_q//3 but without a namespace.

xml_entity_q(DCG_Name, DCG_Attributes) -->
  xml_entity_q(dcg_void, DCG_Name, DCG_Attributes).

%! xml_entity_q(:DCG_Namespace, :DCG_Name, :DCG_Attributes)//
% Processes an XML entity that uses question marks in its tags.

xml_entity_q(DCG_Namespace, DCG_Name, DCG_Attributes) -->
  xml_entity(
    (less_than_sign, question_mark),
    DCG_Namespace,
    DCG_Name,
    DCG_Attributes,
    (question_mark, greater_than_sign)
  ).

%! xml_header(
%!   -Tree:compound,
%!   :DCG_Namespace, 
%!   ?Version:compound,
%!   ?Standalone:boolean
%! )//
% Processes an XML header tag.
%
% @param Tree A compound term representing the parse tree.
% @param DCG_Namespace
% @param Version A compound term of the form
%      `version(?Major:integer,?Minor:integer)`.
% @param Standalone A boolean (i.e, either `true` or `false`).

xml_header(header(T1,T2), DCG_Namespace, Version, Standalone) -->
  xml_entity_q(
    % Note that this cannot be processed by xml_name//1.
    dcg_word(xml),
    [
      xml_version(T1, DCG_Namespace, Version),
      xml_standalone(T2, DCG_Namespace, Standalone)
    ]
  ).

