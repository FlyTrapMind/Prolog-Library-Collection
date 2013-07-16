:- module(rfc_2396, []).

/** <module> URI

A grammar and a description of basic functionality for URI.

"This document defines a grammar that is a superset of all valid URI,
such that an implementation can parse the common components of a URI
reference without knowing the scheme-specific requirements of every
possible identifier type. This document does not define a generative
grammar for URI; that task will be performed by the individual
specifications of each URI scheme."

# Abbreviations

  * Uniform Resource Identifier (URI)
    A means for identifying a resource.
  * Resource
    ???

# Variants

  * URL
    A location or name or both.
    Identification by primary access mechanism (e.g., network location)
    rather than by name.
  * URN
    Globally unique and persistent.
    Even when the resource caeses to exist or becomes unavailable.

@author Wouter Beek
@compat RFC 2396
@see http://www.ietf.org/rfc/rfc2396.txt
@tbd
@version 2013/05
*/

