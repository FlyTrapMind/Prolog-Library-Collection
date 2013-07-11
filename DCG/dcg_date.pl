:- module(
  dcg_date,
  [
    date//2, % ?Lang:atom
             % ?Date:compound
    day//2, % ?Lang:atom
            % ?Day:integer
    month//2 % ?Lang:atom
             % ?Month:integer
  ]
).

/** <module> DCG_DATE

DCG rules for parsing/generating dates.

@author Wouter Beek
@version 2013/06
*/



date(Lang, date(Year,Month,Day)) -->
  % DCG_YEAR cannot be used for consecutive YYYYMMDD representations,
  % since we need the unwarrented asseumption that a year lies between
  % 0000 and 9999.
  year(Lang, Year),
  ("-" ; ""),
  month(Lang, Month),
  ("-" ; ""),
  day(Lang, Day).

day(_Lang, Day) -->
  [D1,D2],
  {
    number_codes(Day, [D1,D2]),
    between(1, 31, Day)
  }.

month(_Lang, Month) -->
  [D1,D2],
  {
    number_codes(Month, [D1,D2]),
    between(1, 12, Month)
  }.

year(_Lang, Year) -->
  [Y1,Y2,Y3,Y4],
  {number_codes(Year, [Y1,Y2,Y3,Y4])}.
