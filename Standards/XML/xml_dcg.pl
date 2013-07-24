:- module(
  xml_dcg,
  [
    xml_attribute//3, % :DCG_Namespace
                      % :DCG_Name
                      % :DCG_Value
    xml_attribute//4, % :DCG_Namespace
                      % :DCG_Name
                      % :DCG_Value
                      % :DCG_Sepatator
    xml_inject_attributes/4, % :DCG_Namespace,
                             % +Attributes1:list(dcg),
                             % -Attributes2:list(dcg),
                             % -Trees:list(compound)
    xml_entities//3, % -Trees:list(compound)
                     % :DCG_Namespace
                     % +EntityRules:list(dcg)
    xml_entity//3, % :DCG_Namespace
                   % :DCG_Name
                   % ?DCG_Attributes
    xml_header//3 % -Tree:compound
                  % ?Version:compound
                  % ?Standalone:boolean
  ]
).

/** <module> XML_DCG

DCG rules implementing the XML standard.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).

:- meta_predicate(xml_attribute(//,//,?,?)).
:- meta_predicate(xml_attribute(//,//,//,?,?)).
:- meta_predicate(xml_attribute(//,//,//,//,?,?)).
:- meta_predicate(xml_attribute_list(-,//,//,?,?,?)).
:- meta_predicate(xml_entities(-,//,//,?,?)).
:- meta_predicate(xml_entities(-,//,+,+,?,?)).
:- meta_predicate(xml_entity(//,//,?,?)).
:- meta_predicate(xml_entity(//,//,//,?,?)).
:- meta_predicate(xml_entity(//,//,//,//,?,?)).
:- meta_predicate(xml_entity(//,//,//,//,//,?,?)).
:- meta_predicate(xml_entity_q(//,//,?,?)).
:- meta_predicate(xml_entity_q(//,//,//,?,?)).
:- meta_predicate(xml_inject_attributes(//,+,-,-)).
:- meta_predicate(xml_namespaced_name(//,//,?,?)).
:- meta_predicate(xml_value(//,?,?)).



%! xml_attribute(:DCG_Name, :DCG_Value)//
% Processes an XML attribute based on DCG rules for the head and the value.
% XML attributes have an equals signs in between the name and value parts.

xml_attribute(DCG_Name, DCG_Value) -->
  DCG_Name,
  equals_sign,
  xml_value(DCG_Value).

xml_attribute(DCG_Namespace, DCG_Name, DCG_Value) -->
  xml_attribute(xml_namespaced_name(DCG_Namespace, DCG_Name), DCG_Value).

xml_attribute(DCG_Namespace, DCG_Name, DCG_Value1, DCG_Separator) -->
  {DCG_Value1 =.. [P, Trees, A1s], DCG_Value2 =.. [P]},
  xml_attribute(
    xml_namespaced_name(DCG_Namespace, DCG_Name),
    xml_attribute_list(Trees, DCG_Value2, A1s, DCG_Separator)
  ).

xml_attribute_list([], _DCG_Value, [], _DCG_Separator) --> [].
xml_attribute_list([Tree], DCG_Value1, [A1], _DCG_Separator) -->
  {DCG_Value1 =.. [P], DCG_Value2 =.. [P, Tree, A1]},
  dcg_call(DCG_Value2).
xml_attribute_list([Tree|Trees], DCG_Value1, [A1|A1s], DCG_Separator) -->
  {DCG_Value1 =.. [P], DCG_Value2 =.. [P, Tree, A1]},
  dcg_call(DCG_Value2),
  dcg_call(DCG_Separator),
  xml_attribute_list(Trees, DCG_Value1, A1s, DCG_Separator).

xml_boolean(boolean(no), false) --> "no".
xml_boolean(boolean(yes), true) --> "yes".

%! xml_comment(-Tree:compound, ?Comment:atom)//

xml_comment(comment(Comment), Comment) -->
  {nonvar(Comment)}, !,
  {atom_codes(Comment, Codes)},
  less_than_sign, exclamation_mark, hyphen_minus, hyphen_minus, space,
  Codes,
  space, hyphen_minus, hyphen_minus, greater_than_sign.
xml_comment(comment(Comment), Comment) -->
  less_than_sign, exclamation_mark, hyphen_minus, hyphen_minus, space,
  Codes,
  space, hyphen_minus, hyphen_minus, greater_than_sign,
  {atom_codes(Comment, Codes)}.

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

%! xml_header(-Tree:compound, ?Version:compound, ?Standalone:boolean)//
% Processes an XML header tag.
%
% @arg Version A compound term of the form
%      `version(?Major:integer,?Minor:integer)`.
% @arg Standalone A boolean (i.e, either `true` or `false`).

xml_header(header(T1,T2), Version, Standalone) -->
  xml_entity_q(
    "xml",
    [xml_version(T1, Version), xml_standalone(T2, Standalone)]
  ).

%! xml_inject_attributes(
%!   :DCG_Namespace,
%!   +Attributes1:list(dcg),
%!   -Attributes2:list(dcg),
%!   -Trees:list(compound)
%! ) is det.
% Prepares the given XML attributes for being passed to xml_attribute//2.

xml_inject_attributes(_DCG_Namespace, [], [], []).
xml_inject_attributes(DCG_Namespace, [H1|T1], [H2|T2], [Tree|Trees]):-
  H1 =.. [P | Args],
  H2 =.. [P, Tree, DCG_Namespace | Args],
  xml_inject_attributes(DCG_Namespace, T1, T2, Trees).

%! xml_namespaced_name(:DCG_Namespace, :DCG_Name)//

xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  {phrase(DCG_Namespace, "")},
  DCG_Name.
xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  DCG_Namespace,
  colon,
  DCG_Name.

%! xml_standalone(-Tree:compound, ?Standalone:boolean)//
% Processes the XML standalone attribute.

xml_standalone(standalone(T1), Standalone) -->
  xml_attribute("standalone", xml_boolean(T1, Standalone)).

%! xml_value(:DCG_Body)//
% Processes an XML value based on a DCG rule.
% XML values are enclosed in double quotes.

xml_value(DCG_Body) -->
  double_quote,
  DCG_Body,
  double_quote.

%! xml_version(-Tree:compound, ?Version:compound)//
% Processes the XML version attribute.

xml_version(version(major(Major),minor(Minor)), version(Major,Minor)) -->
  xml_attribute(
    "version",
    (decimal_number(Major), dot, decimal_number(Minor))
  ).
