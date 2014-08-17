:- module(
  xml_datatypes,
  [
    xml_boolean//2, % -Tree:compound
                    % ?Value:boolean
    xml_namespaced_name//2, % :DCG_Namespace
                            % :DCG_Name
    xml_yes_no//2 % -Tree:compound
                  % ?Boolean:boolean
  ]
).

/** <module> XML datatypes

DCG rules for XML datatypes.

@author Wouter Beek
@version 2013/07-2013/08, 2014/02-2014/05
*/

:- use_module(plDcg(dcg_ascii)).

:- meta_predicate(xml_namespaced_name(//,//,?,?)).



xml_boolean(xml_boolean(false), false) --> `false`.
xml_boolean(xml_boolean(true),  true) --> `true`.


%! xml_namespaced_name(:DCG_Namespace, :DCG_Name)//

xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  {phrase(DCG_Namespace, [])},
  DCG_Name.
xml_namespaced_name(DCG_Namespace, DCG_Name) -->
  DCG_Namespace,
  colon,
  DCG_Name.


xml_yes_no(xml_yes_no(no), false) --> `no`.
xml_yes_no(xml_yes_no(yes), true) --> `yes`.

