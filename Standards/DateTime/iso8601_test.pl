:- module(iso8601_test, []).

/** <module> ISO8601_TEST

Unit tests for the ISO 8061 DCG.

@author Wouter Beek
@version 2013/07-2013/08
*/

:- use_module(library(plunit)).

:- begin_tests(iso8601_dcg).

:- use_module(datetime(iso8601_dcg)).
:- use_module(generics(print_ext)).
:- use_module(library(lists)).

:- discontiguous(test/2).



% DATE & TIME

iso8601_calendar_date_time_example(
  Atom, Format, Y, M, D, T, H, MM, S, UTC, Sign, HH, MMM
):-
  iso8601_calendar_date_example(Atom1, Format, Y, M, D),
  % Notice that we are strict in that we require the time designator to be
  % in place. Otherwise, there are abmbiguities.
  % For example =198504122320Z= can be parsed as =|1985-04-12T23:20Z=|
  % or as =|1985-04T12:23:20Z|=.
  T = true,
  iso8601_local_time_example(
    Atom2, Format, T, H, MM, S, UTC, Sign, HH, MMM
  ),
  atomic_concat(Atom1, Atom2, Atom).

test(
  iso8601_calendar_date_time_generate,
  [
    forall(
      iso8601_calendar_date_time_example(
        Atom1, Format, Y, M, D, T, H, MM, S, UTC, Sign, HH, MMM
      )
    ),
    true(Atom1 == Atom2)
  ]
):-
  once(
    phrase(
      iso8601_calendar_date_time(
        _T0, Format, Y, M, D, T, H, MM, S, UTC, Sign, HH, MMM
      ),
      Codes
    )
  ),
  atom_codes(Atom2, Codes),
  formatnl(Atom2).

test(
  iso8601_calendar_date_time_parse,
  [
    forall(
      iso8601_calendar_date_time_example(
        Atom, Format, Y1, M1, D1, T1, H1, MM1, S1, UTC1, Sign1, HH1, MMM1
      )
    ),
    true(
      maplist(
        =,
        [Y1,M1,D1,T1,H1,MM1,S1,UTC1,Sign1,HH1,MMM1],
        [Y2,M2,D2,T2,H2,MM2,S2,UTC2,Sign2,HH2,MMM2]
      )
    )
  ]
):-
  atom_codes(Atom, Codes),
  once(
    phrase(
      iso8601_calendar_date_time(
        _T0, Format, Y2, M2, D2, T2, H2, MM2, S2, UTC2, Sign2, HH2, MMM2
      ),
      Codes
    )
  ).



% CALENDAR DATE %

%! iso8601_calendar_date_example(
%!   ?Date:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(1,31)
%! ) is nondet.

iso8601_calendar_date_example('19850412'  , basic,    1985, 4, 12).
iso8601_calendar_date_example('1985-04-12', extended, 1985, 4, 12).
% Reduced examples:
iso8601_calendar_date_example('1985-04'   , basic,    1985, 4, _ ).
iso8601_calendar_date_example('1985'      , basic,    1985, _, _ ).
iso8601_calendar_date_example('19'        , basic,    1900, _, _ ).

test(
  iso8601_calendar_date_generate,
  [
    forall(iso8601_calendar_date_example(Atom1, Format, Y, M, D)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_calendar_date(_T0, Format, Y, M, D), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_calendar_date_parse,
  [
    forall(iso8601_calendar_date_example(Atom, Format, Y1, M1, D1)),
    true(maplist(=, [Y1, M1, D1], [Y2, M2, D2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_calendar_date(_T0, Format, Y2, M2, D2), Codes)).



% LOCAL TIME %

%! iso8601_local_time_example(
%!   ?Time:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Hour:between(0,24),
%!   ?Minute:between(0,59),
%!   ?Second:between(0,60),
%!   ?UTC:boolean
%! ) is nondet.
%
% ### RE replace
% Swap the first two arguments, preserving spaces:
%   * From:
%     =|iso8601_local_time_example\(([a-z]+),([" "]+)'([0-9:,A-Za-z]+)',([" "]+)|=
%   * To: =|iso8601_local_time_example\('\3',\4\1,\2|=

iso8601_local_time_example('232050',     basic,    23,   20,   50,   false).
iso8601_local_time_example('23:20:50',   extended, 23,   20,   50,   false).
iso8601_local_time_example('2320',       basic,    23,   20,   _,    false).
iso8601_local_time_example('23:20',      extended, 23,   20,   _,    false).
iso8601_local_time_example('23',         basic,    23,   _,    _,    false).
iso8601_local_time_example('232050,5',   basic,    23,   20,   50.5, false).
iso8601_local_time_example('23:20:50,5', extended, 23,   20,   50.5, false).
iso8601_local_time_example('2320,8',     basic,    23,   20.8, _,    false).
iso8601_local_time_example('23:20,8',    extended, 23,   20.8, _,    false).
iso8601_local_time_example('23,3',       basic,    23.3, _,    _,    false).
iso8601_local_time_example('000000',     basic,    0,    0,    0,    false).
iso8601_local_time_example('00:00:00',   extended, 0,    0,    0,    false).
iso8601_local_time_example('240000',     basic,    24,   0,    0,    false).
iso8601_local_time_example('24:00:00',   extended, 24,   0,    0,    false).
iso8601_local_time_example('232030Z',    basic,    23,   20,   30,   true ).
iso8601_local_time_example('2320Z',      basic,    23,   20,   _,    true ).
iso8601_local_time_example('23Z',        basic,    23,   _,    _,    true ).
iso8601_local_time_example('23:20:30Z',  extended, 23,   20,   30,   true ).
iso8601_local_time_example('23:20Z',     extended, 23,   20,   _,    true ).
iso8601_local_time_example('152746',     basic,    15,   27,   46,   false).
iso8601_local_time_example('15:27:46',   extended, 15,   27,   46,   false).

%! iso8601_local_time_example(
%!   ?Time:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?Hour:between(0,24),
%!   ?Minute:between(0,59),
%!   ?Second:between(0,60),
%!   ?UTC:boolean,
%!   ?Sign:boolean,
%!   ?Hour:between(0,24),
%!   ?Minute:between(0,59)
%! ) is nondet.

iso8601_local_time_example(Atom, Format, T, H, M, S, true, _Sign, _HH, _MM):-
  % UTC correction cannot occur with the UTC indicator.
  iso8601_local_time_example(Atom1, Format, H, M, S, true),
  (
    T = false, Atom = Atom1
  ;
    T = true, atomic_concat('T', Atom1, Atom)
  ).
iso8601_local_time_example(Atom, Format, T, H, M, S, false, Sign, HH, MM):-
  iso8601_local_time_example(Atom1, Format, H, M, S, false),
  iso8601_utc_correction_example(Atom2, Format, Sign, HH, MM),
  (
    T = false, atomic_concat(Atom1, Atom2, Atom)
  ;
    T = true, atomic_list_concat(['T',Atom1,Atom2], Atom)
  ).

%! iso8601_utc_correction_example(
%!   ?Correction:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?TimeDesignator:boolean,
%!   ?Sign:boolean,
%!   ?Hour:between(0,24),
%!   ?Minute:between(0,59)
%! ) is nondet.

iso8601_utc_correction_example('',       _,        _,     _, _).
iso8601_utc_correction_example('+0100',  basic,    true,  1, 0).
iso8601_utc_correction_example('+01:00', extended, true,  1, 0).
iso8601_utc_correction_example('+01',    basic,    true,  1, _).
iso8601_utc_correction_example('+01',    extended, true,  1, _).
iso8601_utc_correction_example('-0500',  basic,    false, 5, 0).
iso8601_utc_correction_example('-05:00', extended, false, 5, 0).
iso8601_utc_correction_example('-05',    basic,    false, 5, _).
iso8601_utc_correction_example('-05',    extended, false, 5, _).

test(
  iso8601_local_time_generate,
  [
    forall(
      iso8601_local_time_example(Atom1, Format, T, H, M, S, UTC, Sign, H_, M_)
    ),
    true(Atom1 = Atom2)
  ]
):-
  once(
    phrase(
      iso8601_local_time(_T0, Format, T, H, M, S, UTC, Sign, H_, M_),
      Codes
    )
  ),
  atom_codes(Atom2, Codes).

test(
  iso8601_local_time_parse,
  [
    forall(
      iso8601_local_time_example(
        Atom, Format, T1, H1, M1, S1, UTC1, Sign1, HH1, MM1)
    ),
    true(
      maplist(
        =,
        [T1,H1,M1,S1,UTC1,Sign1,HH1,MM1],
        [T2,H2,M2,S2,UTC2,Sign2,HH2,MM2]
      )
    )
  ]
):-
  atom_codes(Atom, Codes),
  once(
    phrase(
      iso8601_local_time(_T0, Format, T2, H2, M2, S2, UTC2, Sign2, HH2, MM2),
      Codes
    )
  ).



% ORDINAL DATE %

%! iso8601_ordinal_date_example(
%!   ?Date:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:between(0,9999),
%!   ?Day:between(1,366)
%! ) is nondet.

iso8601_ordinal_date_example('1985102',  basic,    1985, 102).
iso8601_ordinal_date_example('1985-102', extended, 1985, 102).

test(
  iso8601_ordinal_date_generate,
  [
    forall(iso8601_ordinal_date_example(Atom1, Format, Y, D)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_ordinal_date(_T0, Format, Y, D), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_ordinal_date_parse,
  [
    forall(iso8601_ordinal_date_example(Atom, Format, Y1, D1)),
    true(maplist(=, [Y1, D1], [Y2, D2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_ordinal_date(_T0, Format, Y2, D2), Codes)).



% TIME INTERVALS %

iso8601_time_interval_example(
  '19850412T232050/19850625T103000', basic,
  1985, 4, 12, true, 23, 20, 50, false, _, _, _,
  1985, 6, 25, true, 10, 30, 0,  false, _, _, _
).
iso8601_time_interval_example(
  '1985-04-12T23:20:50/1985-06-25T10:30:00', extended,
  1985, 4, 12, true, 23, 20, 50, false, _, _, _,
  1985, 6, 25, true, 10, 30, 0,  false, _, _, _
).

test(
  iso8601_time_interval_generate,
  [
    forall(
      iso8601_time_interval_example(
        Atom1, Format,
        Y1, M1, D1, T1, H1, MM1, S1, UTC1, Sign1, HH1, MMM1,
        Y2, M2, D2, T2, H2, MM2, S2, UTC2, Sign2, HH2, MMM2
      )
    ),
    true(Atom1 == Atom2)
  ]
):-
  once(
    phrase(
      iso8601_time_interval(
        _T0, Format,
        Y1, M1, D1, T1, H1, MM1, S1, UTC1, Sign1, HH1, MMM1,
        Y2, M2, D2, T2, H2, MM2, S2, UTC2, Sign2, HH2, MMM2
      ),
      Codes
    )
  ),
  atom_codes(Atom2, Codes).

test(
  iso8601_time_interval_parse,
  [
    forall(
      iso8601_time_interval_example(
        Atom, Format,
        Y1, M1, D1, T1, H1, MM1, S1, UTC1, Sign1, HH1, MMM1,
        Y2, M2, D2, T2, H2, MM2, S2, UTC2, Sign2, HH2, MMM2
      )
    ),
    true(
      maplist(
        =,
        [
          Y1,M1,D1,T1,H1,MM1,S1,UTC1,Sign1,HH1,MMM1,
          Y2,M2,D2,T2,H2,MM2,S2,UTC2,Sign2,HH2,MMM2
        ],
        [
          Y3,M3,D3,T3,H3,MM3,S3,UTC3,Sign3,HH3,MMM3,
          Y4,M4,D4,T4,H4,MM4,S4,UTC4,Sign4,HH4,MMM4
        ]
      )
    )
  ]
):-
  atom_codes(Atom, Codes),
  once(
    phrase(
      iso8601_time_interval(
        _T0, Format,
        Y3, M3, D3, T3, H3, MM3, S3, UTC3, Sign3, HH3, MMM3,
        Y4, M4, D4, T4, H4, MM4, S4, UTC4, Sign4, HH4, MMM4
      ),
      Codes
    )
  ).

%! iso8601_time_interval_example(
%!   ?TimeInterval:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:integer,
%!   ?Month:integer,
%!   ?Week:integer,
%!   ?Day:integer
%!   ?TimeSeparator:boolean,
%!   ?Hour:integer,
%!   ?Minute:integer,
%!   ?Second:integer
%! ) is nondet.

iso8601_time_interval_example(
  'P2Y10M15DT10H30M20S', basic, 2, 10, _, 15, true, 10, 30, 20
).
iso8601_time_interval_example(
  'P2Y10M15DT10H30M20S', extended, 2, 10, _, 15, true, 10, 30, 20
).
iso8601_time_interval_example('P6W', basic, _, _, 6, _, false, _, _, _).
iso8601_time_interval_example('P6W', extended, _, _, 6, _, false, _, _, _).

test(
  iso8601_time_interval_generate,
  [
    forall(
      iso8601_time_interval_example(Atom1, Format, Y, M, W, D, T, H, MM, S)
    ),
    true(Atom1 == Atom2)
  ]
):-
  once(
    phrase(
      iso8601_time_interval(_T0, Format, Y, M, W, D, T, H, MM, S),
      Codes
    )
  ),
  atom_codes(Atom2, Codes).

test(
  iso8601_time_interval_parse,
  [
    forall(
      iso8601_time_interval_example(Atom, Format, Y1, M1, W1, D1, T1, H1, MM1, S1)
    ),
    true(maplist(=, [Y1,M1,W1,D1,T1,H1,MM1,S1], [Y2,M2,W2,D2,T2,H2,MM2,S2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(
    phrase(
      iso8601_time_interval(_T0, Format, Y2, M2, W2, D2, T2, H2, MM2, S2),
      Codes
    )
  ).



% WEEK DATE %

%! iso8601_week_date_example(
%!   ?Date:atom,
%!   ?Format:oneof([basic,extended]),
%!   ?Year:between(0,9999),
%!   ?Week:between(1,53),
%!   ?Day:between(1,7)
%! ) is nondet.

iso8601_week_date_example('1985W155',   basic,    1985, 15, 5).
iso8601_week_date_example('1985-W15-5', extended, 1985, 15, 5).
% Reduced accuracy:
iso8601_week_date_example('1985W15',    basic,    1985, 15, _).
iso8601_week_date_example('1985-W15',   extended, 1985, 15, _).

test(
  iso8601_week_date_generate,
  [
    forall(iso8601_week_date_example(Atom1, Format, Y, W, D)),
    true(Atom1 == Atom2)
  ]
):-
  once(phrase(iso8601_week_date(_Tree, Format, Y, W, D), Codes)),
  atom_codes(Atom2, Codes).

test(
  iso8601_week_date_parse,
  [
    forall(iso8601_week_date_example(Atom, Format, Y1, W1, D1)),
    true(maplist(=, [Y1, W1, D1], [Y2, W2, D2]))
  ]
):-
  atom_codes(Atom, Codes),
  once(phrase(iso8601_week_date(_Tree, Format, Y2, W2, D2), Codes)).

:- end_tests(iso8601_dcg).

