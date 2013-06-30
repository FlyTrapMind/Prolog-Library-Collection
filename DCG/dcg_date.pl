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

/** <module>

DCG rules for parsing/generating dates.

@author Wouter Beek
@version 2013/06
*/

:- use_module(dcg(dcg_year)).



date(Lang, date(Year,Month,Day)) -->
  year(Lang, Year),
  % Exclude intervals.
  {integer(Year)},
  ("-" ; ""),
  Month(Lang, Month),
  ("-" ; ""),
  day(Lang, Day).

day(_Lang, Day) -->
  digit(X),
  digit(Y),
  {
    Day is X * 10 + Y,
    between(1, 31, Day)
  }.

month(_Lang, Month) -->
  digit(X),
  digit(Y),
  {
    Month is X * 10 + Y,
    between(1, 12, Month)
  }.
