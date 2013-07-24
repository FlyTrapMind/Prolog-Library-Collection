:- module(
  xlink,
  [
    xlink_href//3 % -Tree:compound
                  % :DCG_Namespace
                  % ?Fragment:atom
  ]
).

/** <module> XLINK

Support for the XML Linking Language.

Create and describe links between resources from within XML documents.

#

  * Link
    An explicit relationship between (portions of) resources.
  * Linking element
    An XLink-conforming XML element that asserts the existence of a link.
  * Resource
    Any addressable unit of information or service.
  * Hyperlink
    A link that is intended primarily for human presentation.
  * Traversal
    Using or following a link for any purpose.
  * Starting resource
    The source from which traversal is begun.
  * Ending resource
    The destination where traversal ends.
  * Arc
    Information about how to traverse a pair or resources
    (e.g. direction of traversal, application behavior information).
  * Local resource
  * Remote resource
  * Linkbases
  * Extended link
    A link that associates an arbitrary number of resources.

# Simple link

~~~{.dtd}
<ELEMENT xlink:type="simple" xlink:href="URI">CONTENT</ELEMENT>
~~~

@author Wouter Beek
@version 2013/05, 2013/07
*/

:- use_module(dcg(dcg_content)).
:- use_module(rfc(rfc_2396)).
:- use_module(xml(xml_dcg)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(xlink, 'http://www.w3.org/1999/xlink').

:- meta_predicate(xlink_attribute(//,+,//,?,?)).
:- meta_predicate(xlink_href(-,//,?,?,?)).



% Attributes inside namespace `xlink` need no namespace prefix.
xlink_attribute(DCG_Namespace, Name, DCG_Value) -->
  {phrase(DCG_Namespace, "xlink")}, !,
  xlink_attribute(void, Name, DCG_Value).
xlink_attribute(DCG_Namespace, Name, DCG_Value) -->
  xml_attribute(DCG_Namespace, word(Name), DCG_Value).

%! xlink_href(-Tree:compound, :DCG_Namespace, ?Fragment:atom)//
% @tbd Support for IRIs.

xlink_href(href(T1), DCG_Namespace, Fragment) -->
  xlink_attribute(
    DCG_Namespace,
    href,
    uri_reference(T1, _Scheme, _Authority, _Path, _Query, Fragment)
  ).
