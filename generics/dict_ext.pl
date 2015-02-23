:- module(
  dict_ext,
  [
    dict_tag/3 % +Dict1:dict
               % +Tag:atom
               % ?Dict2:dict
  ]
).

/** <module> Dictionary extensions

@author Wouter Beek
@version 2014/11
*/




%! dict_tag(+Dict1:dict, +Tag:atom, +Dict2:dict) is semidet.
%! dict_tag(+Dict1:dict, +Tag:atom, -Dict2:dict) is det.

dict_tag(Dict1, Tag, Dict2):-
  dict_pairs(Dict1, _, Pairs),
  dict_pairs(Dict2, Tag, Pairs).



Dict.toNumber(Key) := Number :-
  atom_number(Dict.Key, Number).

Dict1.subtract(Key,Value) := Dict2 :-
  get_dict(Key, Dict1, _, Dict2, Dict1.toNumber(Key) - Value).
