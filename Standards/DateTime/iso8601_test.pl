:- module(iso8601_test, []).

/** <module> ISO8601_TEST

Unit tests for the ISO 8061 DCG.

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(library(plunit)).

:- begin_tests(iso8601).

:- use_module(datetime(iso8601_date)).
:- use_module(datetime(iso8601_date_time)).
:- use_module(datetime(iso8601_time_interval)).
:- use_module(datetime(iso8601_time_point)).
:- use_module(generics(print_ext)).
:- use_module(library(lists)).

:- discontiguous(test/2).



% API %

%! iso8604_compound(Year, MonthInYear, DayInMonth, WeekInYear, DayInWeek)

iso8601_compound(1995, 1, 1, 1994-52, 7).
iso8601_compound(1996, 12, 31, 1997-1, 2).

test(
  day_in_week,
  [
    forall(iso8601_compound(Y, M, D, _WeekInYear1, DayInWeek1)),
    true(DayInWeek1 == DayInWeek2)
  ]
):-
  day_in_week(Y, M, D, DayInWeek2).

test(
  week_in_year,
  [
    forall(iso8601_compound(Y, M, D, WeekInYear1, _DayInWeek1)),
    true(WeekInYear1 == WeekInYear2)
  ]
):-
  week_in_year(Y, M, D, WeekInYear2).



% DATE & TIME

iso8601_calendar_date_time_example(Atom, Format, date_time(Date,UTC_Time)):-
  iso8601_calendar_date_example(Atom1, Format, Date),
  iso8601_local_time_example(Atom2, Format, true, UTC_Time),
  atomic_concat(Atom1, Atom2, Atom).

test(
  iso8601_calendar_date_time_generate,
  [
    forall(iso8601_calendar_date_time_example(Atom1, Format, DateTime)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_calendar_date_time(_T0, Format, DateTime), Codes)),
  atom_codes(Atom2, Codes),
  formatnl(Atom2).

test(
  iso8601_calendar_date_time_parse,
  [
    forall(iso8601_calendar_date_time_example(Atom, Format, DateTime1)),
    true(DateTime1 = DateTime2)
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_calendar_date_time(_T0, Format, DateTime2), Codes)).



% CALENDAR DATE %

%! iso8601_calendar_date_example(
%!   ?Date:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Date:compound
%! ) is nondet.

iso8601_calendar_date_example('19850412'  , basic,    date(1985,4,_,12)).
iso8601_calendar_date_example('1985-04-12', extended, date(1985,4,_,12)).
% Reduced examples:
iso8601_calendar_date_example('1985-04'   , basic,    date(1985,4,_,_ )).
iso8601_calendar_date_example('1985'      , basic,    date(1985,_,_,_ )).
iso8601_calendar_date_example('19'        , basic,    date(1900,_,_,_ )).

test(
  iso8601_calendar_date_generate,
  [
    forall(iso8601_calendar_date_example(Atom1, Format, Date)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_calendar_date(_T0, Format, Date), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_calendar_date_parse,
  [
    forall(iso8601_calendar_date_example(Atom, Format, Date1)),
    true(Date1 = Date2)
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_calendar_date(_T0, Format, Date2), Codes)).



% LOCAL TIME %

%! iso8601_local_time_example(
%!   ?Time:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?UTC_Time:compound
%! ) is nondet.

iso8601_local_time_example(Atom, Format, T, utc_time(Time,true)):-
  % UTC correction cannot occur with the UTC indicator.
  iso8601_local_time_example_(Atom1, Format, Time, true),
  (
    T = false, Atom = Atom1
  ;
    T = true, atomic_concat('T', Atom1, Atom)
  ).
iso8601_local_time_example(
  Atom,
  Format,
  T,
  utc_time(Time,UTC_Correction)
):-
  iso8601_local_time_example_(Atom1, Format, Time, false),
  iso8601_utc_correction_example(Atom2, Format, UTC_Correction),
  (
    T = false, atomic_concat(Atom1, Atom2, Atom)
  ;
    T = true, atomic_list_concat(['T',Atom1,Atom2], Atom)
  ).

%! iso8601_local_time_example_(
%!   ?Time:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Time:compound,
%!   ?UTC:boolean
%! ) is nondet.

iso8601_local_time_example_('232050',     basic,    time(23,  20,  50  ), false).
iso8601_local_time_example_('23:20:50',   extended, time(23,  20,  50  ), false).
iso8601_local_time_example_('2320',       basic,    time(23,  20,  _   ), false).
iso8601_local_time_example_('23:20',      extended, time(23,  20,  _   ), false).
iso8601_local_time_example_('23',         basic,    time(23,  _,   _   ), false).
iso8601_local_time_example_('232050,5',   basic,    time(23,  20,  50.5), false).
iso8601_local_time_example_('23:20:50,5', extended, time(23,  20,  50.5), false).
iso8601_local_time_example_('2320,8',     basic,    time(23,  20.8,_   ), false).
iso8601_local_time_example_('23:20,8',    extended, time(23,  20.8,_   ), false).
iso8601_local_time_example_('23,3',       basic,    time(23.3,_,   _   ), false).
iso8601_local_time_example_('000000',     basic,    time(0,   0,   0   ), false).
iso8601_local_time_example_('00:00:00',   extended, time(0,   0,   0   ), false).
iso8601_local_time_example_('240000',     basic,    time(24,  0,   0   ), false).
iso8601_local_time_example_('24:00:00',   extended, time(24,  0,   0   ), false).
iso8601_local_time_example_('232030Z',    basic,    time(23,  20,  30  ), true ).
iso8601_local_time_example_('2320Z',      basic,    time(23,  20,  _   ), true ).
iso8601_local_time_example_('23Z',        basic,    time(23,  _,   _   ), true ).
iso8601_local_time_example_('23:20:30Z',  extended, time(23,  20,  30  ), true ).
iso8601_local_time_example_('23:20Z',     extended, time(23,  20,  _   ), true ).
iso8601_local_time_example_('152746',     basic,    time(15,  27,  46  ), false).
iso8601_local_time_example_('15:27:46',   extended, time(15,  27,  46  ), false).

%! iso8601_utc_correction_example(
%!   ?Correction:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?UTC_Correction:compound
%! ) is nondet.

iso8601_utc_correction_example('',       _,        false           ).
iso8601_utc_correction_example('+0100',  basic,    utc(true,  1, 0)).
iso8601_utc_correction_example('+01:00', extended, utc(true,  1, 0)).
iso8601_utc_correction_example('+01',    basic,    utc(true,  1, _)).
iso8601_utc_correction_example('+01',    extended, utc(true,  1, _)).
iso8601_utc_correction_example('-0500',  basic,    utc(false, 5, 0)).
iso8601_utc_correction_example('-05:00', extended, utc(false, 5, 0)).
iso8601_utc_correction_example('-05',    basic,    utc(false, 5, _)).
iso8601_utc_correction_example('-05',    extended, utc(false, 5, _)).

test(
  iso8601_local_time_generate,
  [
    forall(iso8601_local_time_example(Atom1, Format, T, UTC_Time)),
    true(Atom1 = Atom2)
  ]
):-
  once(phrase(iso8601_local_time(_T0, Format, T, UTC_Time), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_local_time_parse,
  [
    forall(iso8601_local_time_example(Atom, Format, T1, UTC_Time1)),
    true(maplist(=, [T1,UTC_Time1], [T2,UTC_Time2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_local_time(_T0, Format, T2, UTC_Time2), Codes)).



% ORDINAL DATE %

%! iso8601_ordinal_date_example(
%!   ?Date:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Date:compound
%! ) is nondet.

iso8601_ordinal_date_example('1985102',  basic,    date(1985,_,_,102)).
iso8601_ordinal_date_example('1985-102', extended, date(1985,_,_,102)).

test(
  iso8601_ordinal_date_generate,
  [
    forall(iso8601_ordinal_date_example(Atom1, Format, Date)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_ordinal_date(_T0, Format, Date), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_ordinal_date_parse,
  [
    forall(iso8601_ordinal_date_example(Atom, Format, Date1)),
    true(Date1 = Date2)
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_ordinal_date(_T0, Format, Date2), Codes)).



% TIME INTERVALS %

%! iso8601_time_interval_example(
%!   ?TimeInterval:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?DateTime1:compound,
%!   ?DateTime2:compound
%! ) is nondet.

iso8601_time_interval_example(
  '19850412T232050/19850625T103000',
  basic,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1985,6,_,25),utc_time(time(10,30,0 ),false))
).
iso8601_time_interval_example(
  '1985-04-12T23:20:50/1985-06-25T10:30:00',
  extended,
  date_time(date(1985,4,_,12),utc_time(time(23,20,50),false)),
  date_time(date(1985,6,_,25),utc_time(time(10,30,0 ),false))
).

test(
  iso8601_time_interval_generate,
  [
    forall(
      iso8601_time_interval_example(Atom1, Format, DateTime1, DateTime2)
    ),
    true(Atom1 == Atom2)
  ]
):-
  once(
    phrase(iso8601_time_interval(_T0, Format, DateTime1, DateTime2), Codes)
  ),
  atom_codes(Atom2, Codes).

test(
  iso8601_time_interval_parse,
  [
    forall(iso8601_time_interval_example(Atom, Format, DateTime1, DateTime2)),
    true(maplist(=, [DateTime1, DateTime2], [DateTime3, DateTime4]))
  ]
):-
  atom_codes(Atom, Codes),
  once(
    phrase(iso8601_time_interval(_T0, Format, DateTime3, DateTime4), Codes)
  ).

%! iso8601_time_interval_example(
%!   ?TimeInterval:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?DateTime:compound
%! ) is nondet.

iso8601_time_interval_example(
  'P2Y10M15DT10H30M20S',
  basic,
  date_time(date(2,10,_,15),utc_time(time(10,30,20),false))
).
iso8601_time_interval_example(
  'P2Y10M15DT10H30M20S',
  extended,
  date_time(date(2,10,_,15),utc_time(time(10,30,20),false))
).
iso8601_time_interval_example(
  'P6W',
  basic,
  date_time(date(_,_,6,_),utc_time(time(_,_,_),false))
).
iso8601_time_interval_example(
  'P6W',
  extended,
  date_time(date(_,_,6,_),utc_time(time(_,_,_),false))
).

test(
  iso8601_time_interval_generate,
  [
    forall(iso8601_time_interval_example(Atom1, Format, DateTime)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_time_interval(_T0, Format, DateTime), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_time_interval_parse,
  [
    forall(iso8601_time_interval_example(Atom, Format, DateTime1)),
    true(DateTime1 = DateTime2)
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_time_interval(_T0, Format, DateTime2), Codes)).



% WEEK DATE %

%! iso8601_week_date_example(
%!   ?Date:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Date:compound
%! ) is nondet.

iso8601_week_date_example('1985W155',   basic,    date(1985,_,15,5)).
iso8601_week_date_example('1985-W15-5', extended, date(1985,_,15,5)).
% Reduced accuracy:
iso8601_week_date_example('1985W15',    basic,    date(1985,_,15,_)).
iso8601_week_date_example('1985-W15',   extended, date(1985,_,15,_)).

test(
  iso8601_week_date_generate,
  [
    forall(iso8601_week_date_example(Atom1, Format, Date)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_week_date(_Tree, Format, Date), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_week_date_parse,
  [
    forall(iso8601_week_date_example(Atom, Format, Date1)),
    true(Date1 = Date2)
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_week_date(_Tree, Format, Date2), Codes)).

:- end_tests(iso8601).

