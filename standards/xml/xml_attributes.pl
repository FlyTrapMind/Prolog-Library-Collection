:- module(
  xml_attributes,
  [
    xml_attribute//3, % :DcgNamespace
                      % :DcgName
                      % :DcgValue
    xml_attribute//4, % :DcgNamespace
                      % :DcgName
                      % :DcgValue
                      % :DCG_Sepatator
    xml_id//3, % -Tree:compound
               % :DcgNamespace
               % ?Name:atom
    xml_inject_attributes/4, % :DcgNamespace,
                             % +Attributes1:list(dcg),
                             % -Attributes2:list(dcg),
                             % -Trees:list(compound)
    xml_language//3, % -Tree:compound
                     % :DcgNamespace
                     % ?LanguageTag
    xml_standalone//3, % -Tree:compound
                       % :DcgNamespace
                       % ?Standalone:boolean
    xml_version//3 % -Tree:compound
                   % :DcgNamespace
                   % ?Version:compound
  ]
).

/** <module> XML: Attributes

Grammar for XML attributes.

@author Wouter Beek
@compat XML 1.0-5 http://www.w3.org/TR/2008/REC-xml-20081126/
@version 2013/07, 2014/03, 2014/05, 2014/10
*/

:- use_module(lang(rfc3066)).
:- use_module(uri(rfc2396_dcg)).
:- use_module(xml(xml)).
:- use_module(xml(xml_datatypes)).
:- use_module(xml(xml_word)).

:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_cardinal)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).

:- meta_predicate(xml_attribute(//,//,?,?)).
:- meta_predicate(xml_attribute(//,//,//,?,?)).
:- meta_predicate(xml_attribute(//,//,//,//,?,?)).
:- meta_predicate(xml_attribute0(//,//,//,?,?)).
:- meta_predicate(xml_attribute_list(-,//,//,?,?,?)).
:- meta_predicate(xml_id(-,//,?,?,?)).
:- meta_predicate(xml_inject_attributes(//,+,-,-)).
:- meta_predicate(xml_language(-,//,?,?,?)).
:- meta_predicate(xml_standalone(-,//,?,?,?)).
:- meta_predicate(xml_value(//,?,?)).
:- meta_predicate(xml_version(-,//,?,?,?)).



%! xml_attribute(:DcgName, :DcgValue)//
% Processes an XML attribute based on DCG rules for the head and the value.
% XML attributes have an equals signs in between the name and value parts.

xml_attribute(DcgName, DcgValue) -->
  DcgName,
  equals_sign,
  xml_value(DcgValue).



%! xml_attribute(:DcgNamespace, :DcgName, :DcgValue)// .

xml_attribute(DcgNamespace, DcgName, DcgValue) -->
  xml_attribute(xml_namespaced_name(DcgNamespace, DcgName), DcgValue).



%! xml_attribute(DcgNamespace, DcgName, DcgValue1, DcgSeparator)// .

xml_attribute(DcgNamespace, DcgName, DcgValue1, DcgSeparator) -->
  {DcgValue1 =.. [P, Trees, A1s], DcgValue2 =.. [P]},
  xml_attribute(
    xml_namespaced_name(DcgNamespace, DcgName),
    xml_attribute_list(Trees, DcgValue2, A1s, DcgSeparator)
  ).

xml_attribute0(DcgNamespace, DcgName, DcgValue) -->
  xml_attribute(xml_namespace(DcgNamespace), DcgName, DcgValue).

xml_attribute_list([], _, [], _) --> [].
xml_attribute_list([Tree], DcgValue1, [A1], _) -->
  {DcgValue1 =.. [P], DcgValue2 =.. [P, Tree, A1]},
  phrase(DcgValue2).
xml_attribute_list([Tree|Trees], DcgValue1, [A1|A1s], DcgSeparator) -->
  {DcgValue1 =.. [P], DcgValue2 =.. [P, Tree, A1]},
  phrase(DcgValue2),
  phrase(DcgSeparator),
  xml_attribute_list(Trees, DcgValue1, A1s, DcgSeparator).



%! xml_id(-Tree:compound, :DcgNamespace, ?Name:atom)// .
% Standard XML attribute for assigning a unique name to an element.
%
% =ID= is a =TokenizedType= of an =AttType=.
% Values of type =ID= MUST match the =Name= production. A name MUST NOT
% appear more than once in an XML document as a value of this type;
% i.e., =ID= values MUST uniquely identify the elements which bear them.
%
% An element type MUST NOT have more than one =ID= attribute specified.
%
% An =ID= attribute MUST have a declared default of
% =|#IMPLIED|= or =|#REQUIRED|=.
%
% ~~~{.bnf}
% id = "name"
% ~~~

xml_id(id(Name), DcgNamespace, Name) -->
  xml_attribute0(DcgNamespace, 'Name'(id), 'Name'(Name)).



%! xml_inject_attributes(
%!   :DcgNamespace,
%!   +Attributes1:list(callable),
%!   -Attributes2:list(callable),
%!   -Trees:list(compound)
%! ) is det.
% Prepares the given XML attributes for being passed to xml_attribute//2.

xml_inject_attributes(_DCG_Namespace, [], [], []).
xml_inject_attributes(DcgNamespace, [H1|T1], [H2|T2], [Tree|Trees]):-
  H1 =.. [P | Args],
  H2 =.. [P, Tree, DcgNamespace | Args],
  xml_inject_attributes(DcgNamespace, T1, T2, Trees).



%! xml_language(-Tree:compound, :DcgNamespace, ?LanguageTag:atom)//
% Specifies the language used in the contents and attribute values of any
% element in an XML document. In valid documents, this attribute, like any
% other, MUST be declared if it is used.
%
% ~~~{.bnf}
% xml:base = "<iri>"
% ~~~
%
% The values of the attribute are language identifiers as defined by
% IETF BCP 47, "Tags for the Identification of Languages"; in addition,
% the empty string may be specified.
%
% Examples of declarations:
% ~~~{.dtd}
% <!ATTLIST example xml:lang CDATA #IMPLIED>
% <!ATTLIST poem    xml:lang CDATA 'fr'    >
% <!ATTLIST gloss   xml:lang CDATA 'en'    >
% <!ATTLIST note    xml:lang CDATA 'en'    >
% ~~~
%
% @see The XML 1.0-5 specification references IETF RFC 4646 and RFC 4647
%      for the valid values of this attribute.
%      The XML 1.1-2 specification references IETF RFC 3066, a precursor of
%      RFC 4646 and RFC 4647, instead.
%      This module implements RFC 5646, the successor to 4646.
% @tbd Implement RFC 4647.

xml_language(xml_language(T1), DcgNamespace, LanguageTag) -->
  xml_attribute0(
    DcgNamespace,
    'Name'(lang),
    rfc3066_language_tag(T1, Primary, Secondary)
  ),
  {atomic_list_concat([Primary,Secondary], '-', LanguageTag)}.



%! xml_standalone(-Tree:compound, :DcgNamespace, ?Standalone:boolean)//
% Processes the XML standalone attribute.

xml_standalone(standalone(T1), DcgNamespace, Standalone) -->
  xml_attribute0(
    DcgNamespace,
    'Name'(standalone),
    xml_yes_no(T1, Standalone)
  ).



%! xml_value(:Dcg)//
% Processes an XML value based on a DCG rule.
% XML values are enclosed in double quotes.

xml_value(Dcg) -->
  quoted(double_quote, Dcg).



%! xml_version(-Tree:compound, :DcgNamespace, ?Version:compound)//
% Processes the XML version attribute.
%
% @arg Tree A compound term representing a parse tree.
% @arg DcgNamespace A DCG_Rule.
% @arg Version A compound term of the form
%        =|version(Major:positive_integer,Minor:positive_integer)|=.

xml_version(
  version(major(Major),minor(Minor)),
  DcgNamespace,
  version(Major,Minor)
) -->
  xml_attribute0(
    DcgNamespace,
    'Name'(version),
    (
      integer(Major),
      `.`,
      integer(Minor)
    )
  ).

