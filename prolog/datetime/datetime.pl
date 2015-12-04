:- module(
  datetime,
  [
    date_mask/3, % +Mask:oneof([year,month,day,hour,minute,second,offset])
                 % +Datetime:compound
                 % -MaskedDatetime:compound
    date_to_datetime/2, % +Date:compound
                        % -Datetime:compound
    datetime_to_date/2, % +Datetime:compound
                        % -Date:compound
    get_datetime/1 % -Datetime:compound
  ]
).

/** <module> Date-time support

Support predicates for dealing with date and time representations.

@author Wouter Beek
@version 2015/08, 2015/11-2015/12
*/

:- use_module(library(apply)).





%! date_mask(
%!   +Mask:oneof([year,month,day,hour,minute,second,offset]),
%!   +Datetime:compound,
%!   -MaskedDatetime:compound
%! ) is det.

date_mask(year,   datetime(_,Mo,D,H,Mi,S,Off), datetime(_,Mo,D,H,Mi,S,Off)).
date_mask(month,  datetime(Y,_, D,H,Mi,S,Off), datetime(Y,_, D,H,Mi,S,Off)).
date_mask(day,    datetime(Y,Mo,_,H,Mi,S,Off), datetime(Y,Mo,_,H,Mi,S,Off)).
date_mask(hour,   datetime(Y,Mo,D,_,Mi,S,Off), datetime(Y,Mo,D,_,Mi,S,Off)).
date_mask(minute, datetime(Y,Mo,D,H,_, S,Off), datetime(Y,Mo,D,H,_, S,Off)).
date_mask(second, datetime(Y,Mo,D,H,Mi,_,Off), datetime(Y,Mo,D,H,Mi,_,Off)).
date_mask(offset, datetime(Y,Mo,D,H,Mi,S,_  ), datetime(Y,Mo,D,H,Mi,S,_  )).



%! date_to_datetime(+Date:compound, -Datetime:compound) is det.

date_to_datetime(time(H,Mi,S1), datetime(_,_,_,H,Mi,S2,_)):- !,
  S2 is rationalize(S1).
date_to_datetime(date(Y,Mo,D), datetime(Y,Mo,D,_,_,_,_)):- !.
date_to_datetime(date(Y,Mo,D,H,Mi,S1,Off1,_,_), datetime(Y,Mo,D,H,Mi,S2,Off2)):-
  S2 is rationalize(S1),
  Off2 is Off1 // 60.



%! datetime_to_date(+DateTime:compound, -Date:compound) is det.
% Conversion from XSD-inspired date-time representation to
% the three Prolog date-time compound term representations.

datetime_to_date(datetime(Y,Mo,D,H,Mi,S1,Off), time(H,Mi,S2)):-
  maplist(var, [Y,Mo,D,Off]), !,
  S2 is float(S1).
datetime_to_date(datetime(Y,Mo,D,H,Mi,S,Off), date(Y,Mo,D)):-
  maplist(var, [H,Mi,S,Off]), !.
datetime_to_date(datetime(Y,Mo,D,H,Mi,S1,Off1), date(Y,Mo,D,H,Mi,S2,Off2,-,-)):-
  (var(Off1) -> true ; Off2 is Off1 * 60),
  S2 is float(S1).



%! get_datetime(-Datetime:compound) is det.

get_datetime(DT):-
  get_time(TS),
  stamp_date_time(TS, D, local),
  date_to_datetime(D, DT).
