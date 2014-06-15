:- module(rfc5234_basic, []).
:- reexport(
  rfc2234,
  [
    'ALPHA'//0,
    'ALPHA'//1, % ?Code:code
    'BIT'//0,
    'BIT'//1, % ?Code:code
    'BIT'//2, % ?Code:code
              % ?DecimalDigit:between(0,1)
    'CHAR'//0,
    'CHAR'//1, % ?Code:code
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//0,
    'DIGIT'//1, % ?Code:code
    'DIGIT'//2, % ?Code:code
                % ?DecimalDigit:between(0,9)
    'DQUOTE'//0,
    'HEXDIG'//0,
    'HEXDIG'//1, % ?Code:code
    'HEXDIG'//2, % ?Code:code
                 % ?DecimalNumber:between(0,15)
    'HTAB'//0,
    'HTAB'//1, % ?Code:code
    'LF'//0,
    'LWSP'//0,
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1, % ?Code:code
    'VCHAR'//0,
    'VCHAR'//1, % ?Code:code
    'WSP'//0,
    'WSP'//1 % ?Code:code
  ]
).

/** <module> RFC 4234 basic rules

DCGs for the basic rules defined in RFC 4234,
 Augmented Backus Naur Form (ABNF).

@author Wouter Beek
@see Obsoletes RFC 2234
@see Obsoletes RFC 4234
@see [RFC 5234](http://tools.ietf.org/html/rfc5234)
@version 2013/07-2013/08, 2013/12, 2014/05-2014/06
*/

