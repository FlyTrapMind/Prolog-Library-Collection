:- module(
  rfc,
  [
  ]
).

/** <module> RFC

@author Wouter Beek
@version 2013/05, 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').

:- initialization(init_rfc).



init_rfc:-
  RFC_Graph = rfc,
  rdfs_assert_class(rfc:'Standard', RFC_Graph),
  init_rfc_1630(RFC_Graph),
  init_rfc_1736(RFC_Graph),
  init_rfc_1737(RFC_Graph),
  init_rfc_1738(RFC_Graph),
  init_rfc_1808(RFC_Graph),
  init_rfc_2396(RFC_Graph),
  W3C_Graph = w3c,
  init_xml(W3C_Graph).

init_rfc_1630(Graph):-
  rdf_global_id(rfc:'1630', This),
  rdf_assert_individual(This, rfc:'Standard', Graph),
  rdf_assert_datatype(This, rfc:year, gYear, 1994, Graph),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Universal Resource Identifiers in WWW',
    Graph
  ),
  rdf_assert_literal(
    This,
    rfc:subtitle,
    en,
    'A Unifying Syntax for the Expression of Names and Addresses of Objects on the Network as used in the World-Wide Web',
    Graph
  ),
  rdf_assert_literal(This, rfc:author, en, 'Tim Berners-Lee', Graph),
  rdf_assert(This, foaf:homepage, rfc:'rfc1630.txt', Graph).

init_rfc_1736(Graph):-
  rdf_global_id(rfc:'1736', This),
  rdf_assert_individual(This, rfc:'Standard', Graph),
  rdf_assert_datatype(This, rfc:year, gYear, 1995, Graph),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Functional Recommendations for Internet Resource Locators',
    Graph
  ),
  rdf_assert_literal(This, rfc:author, en, 'J. Kunze', Graph),
  rdf_assert(This, foaf:homepage, rfc:'rfc1736.txt', Graph).

init_rfc_1737(Graph):-
  rdf_global_id(rfc:'1737', This),
  rdf_assert_individual(This, rfc:'Standard', Graph),
  rdf_assert_datatype(This, rfc:year, gYear, 1994, Graph),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Functional Requirements for Uniform Resource Names',
    Graph
  ),
  rdf_assert_literal(This, rfc:author, en, 'K. Sollins', Graph),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', Graph),
  rdf_assert(This, foaf:homepage, rfc:'rfc1737.txt', Graph).

init_rfc_1738(Graph):-
  rdf_global_id(rfc:'1738', This),
  rdf_assert_individual(This, rfc:'Standard', Graph),
  rdf_assert_datatype(This, rfc:year, gYear, 1994, Graph),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Uniform Resource Locators (URL)',
    Graph
  ),
  rdf_assert_literal(This, rfc:author, en, 'Tim Berners-Lee', Graph),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', Graph),
  rdf_assert_literal(This, rfc:author, en, 'M. McCahill', Graph),
  rdf_assert(This, foaf:homepage, rfc:'rfc1738.txt', Graph),
  rdf_assert(This, rfc:mentions, rfc:'822', Graph), % MAILTO, BNF
  rdf_assert(This, rfc:mentions, rfc:'959', Graph), % FTP
  rdf_assert(This, rfc:mentions, rfc:'977', Graph), % NNTP
  rdf_assert(This, rfc:mentions, rfc:'1036', Graph), % NEWS
  rdf_assert(This, rfc:mentions, rfc:'1436', Graph), % GOPHER
  rdf_assert(This, rfc:mentions, rfc:'1625', Graph), % WAIS
  rdf_assert(This, rfc:mentions, rfc:'1630', Graph). % URIs in WWW

init_rfc_1808(Graph):-
  rdf_global_id(rfc:'1808', This),
  rdf_assert_individual(This, rfc:'Standard', Graph),
  rdf_assert_datatype(This, rfc:year, gYear, 1995, Graph),
  rdf_assert_literal(
    This,
    rfc:title,
    'Relative Uniform Resource Locators',
    Graph
  ),
  rdf_assert_literal(This, rfc:author, 'R. Fielding', Graph),
  rdf_assert(This, foaf:homepage, rfc:'rfc1808.txt', Graph),
  rdf_assert(This, rfc:mentions, rfc:'822', Graph), % BNF
  rdf_assert(This, rfc:mentions, rfc:'1521', Graph), % MIME
  rdf_assert(This, rfc:mentions, rfc:'1630', Graph), % Partial URLs
  rdf_assert(This, rfc:mentions, rfc:'1738', Graph). % URL

init_rfc_2396(Graph):-
  rdf_global_id(rfc:'2396', This),
  rdf_assert_individual(This, rfc:'Standard', Graph),
  rdf_assert_datatype(This, rfc:year, gYear, 1998, Graph),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Uniform Resource Identifiers (URI): Generic Syntax',
    Graph
  ),
  rdf_assert_literal(This, rfc:author, en, 'Tim Berners-Lee', Graph),
  rdf_assert_literal(This, rfc:author, en, 'R. Fielding', Graph),
  rdf_assert_literal(This, rfc:author, en, 'U.C. Irvine', Graph),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', Graph),
  rdf_assert(This, foaf:homepage, rfc:'rfc2396.txt', Graph),
  rdf_assert(This, rfc:mentions, rfc:'1630', Graph),
  rdf_assert(This, rfc:implements, rfc:'1736', Graph),
  rdf_assert(This, rfc:implements, rfc:'1737', Graph),
  rdf_assert(This, rfc:updates, rfc:'1738', Graph),
  rdf_assert(This, rfc:updates, rfc:'1808', Graph).

init_xml(Graph):-
  % Atoms that are used multiple times.
  rdf_global_id(w3c:'XML/Core/', XMLWG),
  rdf_global_id(w3c:'TR/2008/REC-xml-20081126/', This),

  % XML Working Group
  rdfs_assert_label(XMLWG, 'XML Core Working Group', Graph),

  % XML Recommendation
  rdf_assert_individual(This, w3c:'Recommendation', Graph),
  rdf_assert_datatype(This, w3c:year, gYear, 2008, Graph),
  rdf_assert_literal(
    This,
    w3c:title,
    'Extensible Markup Language (XML) 1.0 (Fifth Edition)',
    Graph
  ),
  rdf_assert(This, w3c:developed_by, XMLWG, Graph),
  rdf_assert_literal(This, w3c:author, 'Tim Bray', Graph),
  rdf_assert_literal(This, w3c:author, 'Jean Paoli', Graph),
  rdf_assert_literal(This, w3c:author, 'C. M. Sperberg-McQueen', Graph),
  rdf_assert_literal(This, w3c:author, 'Eve Maler', Graph),
  rdf_assert_literal(This, w3c:author, 'François Yergeau', Graph),
  rdf_assert(This, w3c:supercedes, w3c:'TR/2006/REC-xml-20060816/', Graph),
  % SGML
  rdf_assert(This, w3c:mentions, iso:'8879', Graph),
  % Characters
  rdf_assert(This, w3c:requires, iso:'10646', Graph),
  rdf_assert(This, w3c:requires, std:'BCP47', Graph),
  % Language identification tags
  rdf_assert(This, w3c:requires, std:'IANA-LANGCODES', Graph),
  % Unicode
  rdf_assert(This, w3c:requires, std:'Unicode', Graph).
