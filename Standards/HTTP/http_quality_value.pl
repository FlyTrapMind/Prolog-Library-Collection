:- module(
  http_quality_value,
  [
    qvalue//1 % -QualityValue:between(0.0,1.0)
  ]
).

/** <module> Quality Values

# Quality value specification in RFC 2616

HTTP content negotiation uses short floating point numbers to indicate
  the relative importance ("weight") of various negotiable parameters.

A weight is normalized to a real number in the range 0 through 1,
  where 0 is the minimum and 1 the maximum value.

## Semantics of =|0.0|=

If a parameter has a quality value of 0,
  then content with this parameter is `not acceptable' for the client.

## Precision

HTTP/1.1 applications MUST NOT generate more than three digits after
  the decimal point.
User configuration of these values SHOULD also be limited in this fashion.

## BNF definition

~~~{.abnf}
qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
~~~

## Nomenclature

"Quality values" is a misnomer, since these values merely represent
  relative degradation in desired quality.

--

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(http(rfc2616_basic)).
:- use_module(math(radix)).



%! qvalue(-QualityValue:between(0.0,1.0)//
% ~~~{.abnf}
% qvalue = ( "0" [ "." 0*3DIGIT ] ) | ( "1" [ "." 0*3("0") ] )
% ~~~

qvalue(D) -->
  "0",
  (
    ""
  ;
    ".",
    dcg_multi('DIGIT', 0-3, Ds),
    {digits_to_decimal(Ds, D)}
  ).
qvalue(1.0) -->
  "1",
  ("" ; ".", dcg_multi("0", 0-3)).
