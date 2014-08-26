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
@version 2014/08
*/



%! nvpair(+Name:atom, +Value, +NameValuePair:compound) is semidet.
%! nvpair(-Name:atom, -Value, +NameValuePair:compound) is det.
%! nvpair(+Name:atom, +Value, -NameValuePair:compound) is multi.

nvpair(Name, Value, NVPair):-
  nonvar(NVPair), !,
  nvpair0(Name, Value, NVPair), !.
nvpair(Name, Value, NVPair):-
  nvpair0(Name, Value, NVPair).

nvpair0(Name, Value, Name=Value).
nvpair0(Name, Value, NVPair):-
  NVPair =.. [Name,Value].

