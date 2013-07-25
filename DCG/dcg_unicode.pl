:- module(
  dcg_unicode,
  [
    character_tie//0,
    character_tie//1,
    middle_dot//0,
    middle_dot//1,
    undertie//0,
    undertie//1,
    zero_width_joiner//0,
    zero_width_joiner//1,
    zero_width_non_joiner//0,
    zero_width_non_joiner//1
  ]
).

/** <module> DCG_ASCII

DCG rules that encode characters from the UNICODE standard.

Characters that are within the ASCII range are defined in module [[dcg_ascii]].

@author Wouter Beek
@version 2013/07
*/



character_tie --> [8256].
character_tie(8256) --> [8256].

middle_dot --> [183].
middle_dot(183) --> [183].

undertie --> [8255].
undertie(8255) --> [8255].

zero_width_joiner --> [8203].
zero_width_joiner(8203) --> [8203].

zero_width_non_joiner --> [8204].
zero_width_non_joiner(8204) --> [8204].
