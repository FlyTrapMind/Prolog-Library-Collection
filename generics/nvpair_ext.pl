:- module(
  nvpair_ext,
  [
    nvpair/3 % ?Name:atom
             % ?Value
             % ?NameValuePair:compound
  ]
).

/** <module> Name-value pair extensions

Support for name-value pairs.

@author Wouter Beek
@version 2014/08, 2014/10
*/

:- use_module(pl(pl_mode)).



%! nvpair(+Name:atom, +Value, +NameValuePair:compound) is semidet.
%! nvpair(+Name:atom, +Value, -NameValuePair:compound) is multi.
%! nvpair(-Name:atom, -Value, +NameValuePair:compound) is det.

nvpair(Name, Value, NVPair):-
  call_det(nvpair0, term-Name, term-Value, nonvar-NVPair).

nvpair0(Name, Value, Name=Value).
nvpair0(Name, Value, Name-Value).
nvpair0(Name, Value, NVPair):-
  NVPair =.. [Name,Value].

