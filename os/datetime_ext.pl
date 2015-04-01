:- module(
  datetime_ext,
  [
    current_date/1, % ?Date:atom
    current_date_time/1, % ?DateTime:atom
    current_time/1, % ?Time:atom
    date_time_dict/2, % ?DateTime:compound
                      % ?Dict:dict
    hash_date/1, % -Hash:atom
    iso8601_dateTime/1, % -DateTime:atom
    posix_date/1, % -Date:atom
    posix_time/1, % -Time:atom
    seconds/3 % ?Hours:integer
              % ?Minutes:integer
              % ?Second:integer
  ]
).

/** <module> Datetime extensions

Extensions for date and time.

@author Wouter Beek
@version 2013/06-2013/07, 2013/11, 2014/10, 2015/02
*/

:- use_module(plc(generics/meta_ext)).
:- use_module(plc(io/dir_ext)).





%! current_date(-Date:atom) is det.
% Returns an atom representing the current date.
%
% This can be used for file names.
%
% @compat This uses the ISO 8601 date format, but with underscores instead
%         of dashes.

current_date(Date):-
  get_time(TimeStamp),
  format_time(atom(Date), '%Y_%m_%d', TimeStamp).



%! current_date_time(+DateTime:atom) is semidet.
%! current_date_time(-DateTime:atom) is det.
% @see Combines the result of current_date/1 and current_time/1.

current_date_time(DateTime):-
  current_date(Date),
  current_time(Time),
  atomic_list_concat([Date,Time], ':', DateTime).



%! current_time(-Time:atom) is det.
% Returns an atomic representation of the current time.
%
% This can be used for file names.

current_time(Time):-
  get_time(TimeStamp),
  format_time(atom(Time), '%H_%M_%S', TimeStamp).



%! date_time_dict(?DateTime:compound, ?Dict:dict) is det.

date_time_dict(
  date(Year,Month,Day,Hour,Minute,Second,Offset,Timezone,DaylightSavingTime),
  date_time{
    day:Day,
    'daylight-saving-time':DaylightSavingTime,
    hour:Hour,
    minute:Minute,
    month:Month,
    offset:Offset,
    second:Second,
    timezone:Timezone,
    year:Year
  }
).



%! hash_date(-Hash:atom) is det.
% Returns the hash of the current timestamp.
%
% @arg Hash An atomic hash.

hash_date(Hash):-
  get_time(TimeStamp),
  variant_sha1(TimeStamp, Hash).



%! iso8601_dateTime(-ISO8601_DateTime) is det.

iso8601_dateTime(DT):-
  get_time(TimeStamp),
  format_time(atom(DT), '%FT%T%z', TimeStamp).



%! posix_date(-Date:atom) is det.
% Returns the current date in POSIX format.
%
% @compat POSIX strfdate()
% @arg Date A compound term of the form =Year/Month/Day=,
%        where =Year= consists of 4, =Month= consists of 2,
%        and =Day= consists of 2 digits.

posix_date(Date):-
  get_time(TimeStamp),
  format_time(atom(Date), '%F', TimeStamp).



%! posix_time(Time) is det.
% Returns the current time in POSIX format.
%
% @compat POSIX strftime()
% @arg Time The atomic default textual representation of a time in PraSem,
%        i.e. =Hour:Minute:Second=.

posix_time(Time):-
  get_time(TimeStamp),
  format_time(atom(Time), '%T', TimeStamp).



%! seconds(?Hours:integer, ?Minutes:integer, ?Seconds:integer) is det.
% Converts hours and minutes into seconds and vice versa.
%
% @arg Hours An integer
% @arg Minutes An integer
% @arg Seconds An integer

seconds(Hours, Minutes, Seconds):-
  nonvar(Seconds), !,
  Minutes is Seconds mod 60,
  Hours is Seconds / 60.
seconds(Hours, Minutes, Seconds):-
  default(0, Hours),
  default(0, Minutes),
  Seconds is (Minutes + (Hours * 60)) * 60.

