:- module(
  xml_elements,
  [
    xml_elements//3, % -Trees:list(compound)
                     % :Namespace
                     % :ElementRules:list
    xml_element//3, % :Namespace
                   % :Name
                   % :Attributes:list
    xml_header//4 % -Tree:compound
                  % :Namespace
                  % ?Version:compound
                  % ?Standalone:boolean
  ]
).

/** <module> XML entities

DCG rules for XML entities.

@author Wouter Beek
@version 2013/07, 2014/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(xml(xml_attributes)).
:- use_module(xml(xml_datatypes)).

:- meta_predicate(xml_elements(-,//,//,?,?)).
:- meta_predicate(xml_elements(-,//,+,+,?,?)).
:- meta_predicate(xml_element(//,//,//,?,?)).
:- meta_predicate(xml_element(//,//,//,//,//,?,?)).
:- meta_predicate(xml_element_q(//,//,?,?)).
:- meta_predicate(xml_element_q(//,//,//,?,?)).
:- meta_predicate(xml_header(-,//,?,?,?,?)).



%! xml_elements(-Trees:list(compound), :Namespace, :ElementRules:list)// .
% Processes the given list of XML element rules.

xml_elements([], _, _:[]) --> [].
xml_elements([Tree], Namespace, Mod:[H]) -->
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [Tree,Namespace|Args]).
xml_elements([Tree|Trees], Namespace, Mod:[H|T]) -->
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [Tree,Namespace|Args]),
  xml_elements(Trees, Namespace, Mod:T).


%! xml_element(:Namespace, :Name, :Attributes:list)// .
% Processes a regular XML entity (i.e., one that does not use
% question marks in its tags).

xml_element(Namespace, Name, Attributes) -->
  xml_element(`<`, Namespace, Name, Attributes, `/>`).


%! xml_element(:Open, :Namespace, :Name, :Attributes, :Close)// .
% Processes generic XML entities, with explicitly set opening and closing
% tags. This is normally called via xml_element//3 or xml_element_q//3.

xml_element(Open, Namespace, Name, Attributes, Close) -->
  dcg_between(
    Open,
    (
      xml_namespaced_name(Namespace, Name),
      ` `,
      dcg_sequence(Attributes, ` `)
    ),
    Close
  ).


%! xml_element_q(:Name, :Attributes)// .
% @see Like xml_element_q//3 but without a namespace.

xml_element_q(Name, Attributes) -->
  xml_element_q(void, Name, Attributes).

%! xml_element_q(:Namespace, :Name, :Attributes)//
% Processes an XML entity that uses question marks in its tags.

xml_element_q(Namespace, Name, Attributes) -->
  xml_element(`<?`, Namespace, Name, Attributes, `?>`).


%! xml_header(
%!   -Tree:compound,
%!   :Namespace,
%!   ?Version:compound,
%!   ?Standalone:boolean
%! )//
% Processes an XML header tag.
%
% @arg Tree A compound term representing the parse tree.
% @arg Namespace
% @arg Version A compound term of the form
%      `version(?Major:integer,?Minor:integer)`.
% @arg Standalone A boolean (i.e, either `true` or `false`).

xml_header(header(T1,T2), Namespace, Version, Standalone) -->
  xml_element_q(
    % Note that this cannot be processed by 'Name'//1.
    word(xml),
    [
      xml_version(T1, Namespace, Version),
      xml_standalone(T2, Namespace, Standalone)
    ]
  ).

