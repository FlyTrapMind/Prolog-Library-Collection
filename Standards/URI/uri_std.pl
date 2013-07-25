:- module(uri_std, []).

/** <module> URI_STD

Standard support for URIs, IRIs, URNs, and URLs.

# Overview of standardization process

In December 1994, RFC 1738 formally defined relative and absolute URLs,
refined the general URL syntax, defined how to resolve relative URLs
to absolute form, and better enumerated the URL schemes then in use.
The agreed definition and syntax of URNs had to wait until the
publication of RFC 2141 in May 1997.

The publication of RFC 2396 in August 1998 saw the URI syntax become a
separate specification and most of the parts of RFCs 1630 and 1738
relating to URIs and URLs in general were revised and expanded by the
IETF. The new RFC changed the significance of the "U" in "URI":
it came to represent "Uniform" rather than "Universal".
The sections of RFC 1738 that summarized existing URL schemes migrated
into a separate document. IANA keeps a registry of those schemes;
RFC 2717 first described the procedure to register them.

In December 1999, RFC 2732 provided a minor update to RFC 2396,
allowing URIs to accommodate **IPv6 addresses**.
Some time later, a number of shortcomings discovered in the two
specifications led to the development of a number of draft revisions
under the title rfc2396bis. This community effort, coordinated by
RFC 2396 co-author Roy Fielding, culminated in the publication of
RFC 3986 in January 2005. This RFC, as of 2009 the current version of
the URI syntax recommended for use on the Internet, renders RFC 2396
obsolete. It does not, however, render the details of existing
URL schemes obsolete; RFC 1738 continues to govern such schemes
except where otherwise superseded – RFC 2616 for example, refines
the `http` scheme. Simultaneously, the IETF published the content of
RFC 3986 as the full standard STD 66, reflecting the establishment of
the URI generic syntax as an official Internet protocol.

In August 2002, RFC 3305 pointed out that the term 'URL' has,
despite its widespread use in the vernacular of the Internet-aware public
at large, faded into near obsolescence. It now serves only as a reminder
that some URIs act as addresses because they have schemes that imply
some kind of network accessibility, regardless of whether systems actually
use them for that purpose. As URI-based standards such as
Resource Description Framework make evident, resource identification
need not suggest the retrieval of resource representations over the
Internet, nor need they imply network-based resources at all.

On November 1, 2006, the W3C Technical Architecture Group published
*On Linking Alternative Representations To Enable Discovery And Publishing*
a guide to best practices and canonical URIs for publishing multiple
versions of a given resource. For example, content might differ by
language or by size to adjust for capacity or settings of the device used
to access that content.

The Semantic Web uses the HTTP URI scheme to identify both documents and
concepts in the real world: this has caused confusion as to how to
distinguish the two. The Technical Architecture Group of W3C (TAG)
published an e-mail in June 2005 on how to solve this problem.
The e-mail became known as the *httpRange-14 resolution*.
To expand on this (rather brief) email, W3C published in March 2008
the Interest Group Note *Cool URIs for the Semantic Web*.
This explains the use of content negotiation and the 303-redirect code
in more detail.

@author Wouter Beek
@see http://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Refinement_of_specifications
@tbd Add RFC 2141 on URNs.
@tbd Add RFC 2616 on the HTTP URI scheme.
@tbd Add RFC 2717 on IANA registration process for schemes.
@tbd Add RFC 2732 updating RFC 2396 on URIs.
@tbd Add RFC 2386.
@tbd Add RFC 3989 on revisions to RFC 2396 and RFC 2732 on URIs.
@tbd Add RFC 3305.
@version 2013/07
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(standards)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').

:- initialization(init_uri_std).



init_uri_std:-
  standards_graph(G),
  init_rfc1630(G),
  init_rfc1736(G),
  init_rfc1737(G),
  init_rfc1738(G),
  init_rfc1808(G),
  init_rfc2396(G).

init_rfc1630(G):-
  rdf_global_id(rfc:'1630', This),
  rdfs_assert_class(rfc:'Standard', G),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 1994, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Universal Resource Identifiers in WWW',
    G
  ),
  rdf_assert_literal(
    This,
    rfc:subtitle,
    en,
    'A Unifying Syntax for the Expression of Names and Addresses\c
     of Objects on the Network as used in the World-Wide Web',
    G
  ),
  rdf_assert_literal(This, rfc:author, en, 'Tim Berners-Lee', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc1630.txt', G).

init_rfc1736(G):-
  rdf_global_id(rfc:'1736', This),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 1995, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Functional Recommendations for Internet Resource Locators',
    G
  ),
  rdf_assert_literal(This, rfc:author, en, 'J. Kunze', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc1736.txt', G).

init_rfc1737(G):-
  rdf_global_id(rfc:'1737', This),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 1994, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Functional Requirements for Uniform Resource Names',
    G
  ),
  rdf_assert_literal(This, rfc:author, en, 'K. Sollins', G),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc1737.txt', G).

init_rfc1738(Graph):-
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

init_rfc1808(G):-
  rdf_global_id(rfc:'1808', This),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 1995, G),
  rdf_assert_literal(
    This,
    rfc:title,
    'Relative Uniform Resource Locators',
    G
  ),
  rdf_assert_literal(This, rfc:author, 'R. Fielding', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc1808.txt', G),
  rdf_assert(This, rfc:mentions, rfc:'822', G), % BNF
  rdf_assert(This, rfc:mentions, rfc:'1521', G), % MIME
  rdf_assert(This, rfc:mentions, rfc:'1630', G), % Partial URLs
  rdf_assert(This, rfc:mentions, rfc:'1738', G). % URL

init_rfc2396(G):-
  rdf_global_id(rfc:'2396', This),
  rdf_assert_individual(This, rfc:'Standard', G),
  rdf_assert_datatype(This, rfc:year, gYear, 1998, G),
  rdf_assert_literal(
    This,
    rfc:title,
    en,
    'Uniform Resource Identifiers (URI): Generic Syntax',
    G
  ),
  rdf_assert_literal(This, rfc:author, en, 'Tim Berners-Lee', G),
  rdf_assert_literal(This, rfc:author, en, 'R. Fielding', G),
  rdf_assert_literal(This, rfc:author, en, 'U.C. Irvine', G),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', G),
  rdf_assert(This, foaf:homepage, rfc:'rfc2396.txt', G),
  rdf_assert(This, rfc:mentions, rfc:'1630', G),
  rdf_assert(This, rfc:implements, rfc:'1736', G),
  rdf_assert(This, rfc:implements, rfc:'1737', G),
  rdf_assert(This, rfc:updates, rfc:'1738', G),
  rdf_assert(This, rfc:updates, rfc:'1808', G).

