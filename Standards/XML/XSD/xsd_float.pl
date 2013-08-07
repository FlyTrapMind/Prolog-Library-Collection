:- module(
  xsd_float,
  [
  ]
).

/** <module> XSD_FLOAT

The *|=float= datatype|* is patterned after the IEEE single-precision 32-bit
floating point datatype IEEE 754-2008. Its value space is a subset of the
rational numbers. Floating point numbers are often used to approximate
arbitrary real numbers.

### Value Space

The value space of float contains the non-zero numbers =|M * 2 ** E|=,
where =M= is an integer whose absolute value is less than =|2 ** 24|=, and =E=
is an integer between =|−149|= and =104=, inclusive.
In addition to these values, the value space of float also contains the
following special values:
  * =positiveZero=, sometimes called '0'.
  * =negativeZero=, sometimes called '-0'.
  * =positiveInfinity=, sometimes called 'INF'.
  * =negativeInfinity=, sometimes called '-INF'.
  * =notANumber=, sometimes called 'NaN'.

#### Equality & identity

Equality is identity, except that =|0 = −0|= (although they are not identical)
and =|NaN ≠ NaN|=  (although =NaN= is of course identical to itself).

=0= and =|−0|= are thus equivalent for purposes of enumerations and
identity constraints, as well as for minimum and maximum values.

#### Order

For the basic values, the order relation on float is the order relation for
rational numbers. =INF= is greater than all other non-=NaN= values;
=|−INF|= is less than all other non-=NaN= values. =NaN= is incomparable with
any value in the value space including itself. =0= and =|−0|= are greater than
all the negative numbers and less than all the positive numbers.

#### Bounding facets

Any value incomparable with the value used for the four bounding facets
(=minInclusive=, =maxInclusive=, =minExclusive=, and =maxExclusive=)
will be excluded from the resulting restricted ·value space·.
In particular, when NaN is used as a facet value for a bounding facet,
since no float values are comparable with it, the result is a value space
that is empty. If any other value is used for a bounding facet, NaN will be
excluded from the resulting restricted value space; to add NaN back in
requires union with the NaN-only space (which may be derived using the
pattern 'NaN').

#### Compatibility

The Schema 1.0 version of this datatype did not differentiate between
0 and −0 and NaN was equal to itself. The changes were made to make the
datatype more closely mirror IEEE 754-2008.

