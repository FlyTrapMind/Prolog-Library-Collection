:- module(
  datetime_ext,
  [
    current_date/1, % ?Date:atom
    current_date_time/1, % ?DateTime:atom
    current_time/1, % ?Time:atom
    date_directories/2, % +Dir:atom
                        % -DateDir:atom
    date_time/1, % -DateTime:term
    hash_date/1, % -Hash:atom
    iso8601_dateTime/1, % -DateTime:atom
    latest_file/2, % +Files:list(atom)
                   % -File:atom
    posix_date/1, % -Date:atom
    posix_time/1, % -Time:atom
    posix_timestamp_to_xsd_dateTime/2, % +POSIX_TimeStamp:float
                                       % -XSD_DateTime:compound
    prolog_date_to_xsd_dateTime/2, % +SWI_Prolog_Date:compound
                                   % -XSD_DateTime:compound
    seconds/3, % ?Hours:integer
               % ?Minutes:integer
               % ?Second:integer
    xsd_dateTime/1 % -XSD_DateTime:atom
  ]
).

/** <module> DATETIME_EXT

Extensions for date and time.

@author Wouter Beek
@version 2013/06-2013/07, 2013/11
*/

:- use_module(generics(meta_ext)).
:- use_module(library(http/http_header)).
:- use_module(os(dir_ext)).
:- use_module(xsd(xsd_dateTime)).



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

%! date_directories(+Dir:atom, -DateDir:atom) is det.
% Create and retuns the current date subdirectory of the given absolute
% directory name.
%
% Example: from =|/home/wouterbeek/tmp|= to
% =|/home/wouterbeek/tmp/2013/05/10|=

date_directories(Dir, DateDir):-
  get_time(TimeStamp),
  format_time(atom(Day), '%d', TimeStamp),
  format_time(atom(Month), '%m', TimeStamp),
  RelativeSubDirs1 =.. [Month,Day],
  format_time(atom(Year), '%Y', TimeStamp),
  RelativeSubDirs2 =.. [Year,RelativeSubDirs1],
  RelativeDirs =.. [Dir,RelativeSubDirs2],
  create_nested_directory(RelativeDirs, DateDir).

%! date_time(-DateTime:term) is det.
% Returns a term describing the current date and time.
%
% @compat RFC 112

date_time(DateTime):-
  get_time(TimeStamp),
  http_timestamp(TimeStamp, DateTime).

%! hash_date(-Hash:atom) is det.
% Returns the hash of the current timestamp.
%
% @param Hash An atomic hash.

hash_date(Hash):-
  get_time(TimeStamp),
  variant_sha1(TimeStamp, Hash).

%! iso8601_dateTime(-ISO8601_DateTime) is det.

iso8601_dateTime(DT):-
  get_time(TimeStamp),
  format_time(atom(DT), '%FT%T%z', TimeStamp).

%! latest_file(+Files:list(atom), -Latest:atom) is det.
% Returns the most recently created or altered file from within a list of
% files.
%
% @param Files A list of atomic absolute file names.
% @param Latest An atomic absolute file name.

latest_file([First | Files], Latest):-
  time_file(First, FirstTime),
  latest_file(Files, FirstTime-First, Latest).

latest_file([], _Time-Latest, Latest).
latest_file([File | Files], TopTime/TopFile, Latest):-
  time_file(File, Time),
  (
    Time > TopTime
  ->
    NewTopTime = Time,
    NewTopFile = File
  ;
    NewTopTime = TopTime,
    NewTopFile = TopFile
  ),
  latest_file(Files, NewTopTime-NewTopFile, Latest).

%! posix_date(-Date:atom) is det.
% Returns the current date in POSIX format.
%
% @compat POSIX strfdate()
% @param Date A compound term of the form =Year/Month/Day=,
%        where =Year= consists of 4, =Month= consists of 2,
%        and =Day= consists of 2 digits.

posix_date(Date):-
  get_time(TimeStamp),
  format_time(atom(Date), '%F', TimeStamp).

%! posix_time(Time) is det.
% Returns the current time in POSIX format.
%
% @compat POSIX strftime()
% @param Time The atomic default textual representation of a time in PraSem,
%        i.e. =Hour:Minute:Second=.

posix_time(Time):-
  get_time(TimeStamp),
  format_time(atom(Time), '%T', TimeStamp).

%! posix_timestamp_to_xsd_dateTime(
%!   +POSIX_TimeStemp:float,
%!   -XSD_DT:compound
%! ) is det.
% Converts a POSIX timestamp to an XSD dateTime compound term.
%
% @param POSIX_TimeStamp A floating point number expressing the time
%        in seconds since the Epoch at 1970-1-1.
% @param XSD_DateTime A compound term representing a data-time value,
%        as defined by XML schema 1.1 Part 2: Datatypes.
%
% @see http://en.wikipedia.org/wiki/Unix_time
% @see http://www.w3.org/TR/xmlschema11-2/#dt-dt-7PropMod

posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT):-
  stamp_date_time(POSIX_TS, SWIPL_D, local),
  prolog_date_to_xsd_dateTime(SWIPL_D, XSD_DT).

%! prolog_date_to_xsd_dateTime(
%!   +SWI_Prolog_Date:compound,
%!   -XSD_DateTime:compound
%! ) is det.
% In the SWI-Prolog representation the timezone is an atom (e.g. `CEST`)
% and the offset is an integer representing the offset relative to UTC
% in _seconds_.
%
% In the XSD representation the timezone is the offset relative to UTC
% in _minutes_.
%
% @param SWI_Prolog_Date A compound term representing a date-time value.
%        date-time representations.
% @param XSD_DateTime A compound term representing a data-time value,
%        as defined by XML schema 1.1 Part 2: Datatypes.
%
% @see http://www.swi-prolog.org/pldoc/man?section=timedate
% @see http://www.w3.org/TR/xmlschema11-2/#dt-dt-7PropMod

prolog_date_to_xsd_dateTime(
  date(Y,M,D,H,MM,S,Offset,_TZ,_DST),
  dateTime(Y,M,D,H,MM,S,TZ)
):-
  TZ is Offset / 60.

%! seconds(?Hours:integer, ?Minutes:integer, ?Seconds:integer) is det.
% Converts hours and minutes into seconds and vice versa.
%
% @param Hours An integer
% @param Minutes An integer
% @param Seconds An integer

seconds(Hours, Minutes, Seconds):-
  nonvar(Seconds), !,
  Minutes is Seconds mod 60,
  Hours is Seconds / 60.
seconds(Hours, Minutes, Seconds):-
  default(Hours, 0, SetHours),
  default(Minutes, 0, SetMinutes),
  Seconds is (SetMinutes + (SetHours * 60)) * 60.

%! xsd_dateTime(-DateTime) is det.
% Similar to get_time/1, but returns the date and time in
% the canonical lexical format of the =dateTime= datatype
% from XML Schema 2: Datatypes W3C standard.

xsd_dateTime(DT):-
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  dateTimeCanonicalMap(XSD_DT, DT_Codes),
  atom_codes(DT, DT_Codes).

