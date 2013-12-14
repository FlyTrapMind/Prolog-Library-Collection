:- module(
  http_date,
  [
    'delta-seconds'//1, % ?Seconds:nonneg
    'HTTP-date'//6 % ?Year
                   % ?Month
                   % ?Day
                   % ?Hour
                   % ?Minute
                   % ?Seconds
  ]
).

/** <module> HTTP date

~~~{.pl}
HTTP-date    = rfc1123-date | rfc850-date | asctime-date
rfc1123-date = wkday "," SP date1 SP time SP "GMT"
rfc850-date  = weekday "," SP date2 SP time SP "GMT"
asctime-date = wkday SP date3 SP time SP 4DIGIT
date1        = 2DIGIT SP month SP 4DIGIT
               ; day month year (e.g., 02 Jun 1982)
date2        = 2DIGIT "-" month "-" 2DIGIT
               ; day-month-year (e.g., 02-Jun-82)
date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
               ; month day (e.g., Jun  2)
time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
               ; 00:00:00 - 23:59:59
wkday        = "Mon" | "Tue" | "Wed"
             | "Thu" | "Fri" | "Sat" | "Sun"
weekday      = "Monday" | "Tuesday" | "Wednesday"
             | "Thursday" | "Friday" | "Saturday" | "Sunday"
month        = "Jan" | "Feb" | "Mar" | "Apr"
             | "May" | "Jun" | "Jul" | "Aug"
             | "Sep" | "Oct" | "Nov" | "Dec"
~~~

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(http(http_abnf)).
:- use_module(math(radix)).



%! 'asctime-date'(Y, M, D, H, MM, S)//
% ~~~{.abnf}
% asctime-date = wkday SP date3 SP time SP 4DIGIT
% ~~~

% @tbd Conversion between `D1` and `D2`.
'asctime-date'(Y, M, D, H, MM, S) -->
  wkday(D1),
  'SP',
  date3(M, D2),
  'SP',
  time(H, MM, S),
  'SP',
  dcg_multi('DIGIT', 4-4).

%! date1(?Year:between(0,9999), ?Month:between(1,12), ?Day:between(0,99))//
% ~~~{.abnf}
% date1 = 2DIGIT SP month SP 4DIGIT   ; day month year (e.g., 02 Jun 1982)
% ~~~

date1(Y, M, D) -->
  % Day
  dcg_multi2('DIGIT', 2-2, [D1,D2]),
  {digits_to_decimal([D1,D2], D)},
  'SP',
  
  % Month
  month(M),
  'SP',
  
  % Year
  dcg_multi2('DIGIT', 4-4, _, [Y1,Y2,Y3,Y4]),
  {digits_to_decimal([Y1,Y2,Y3,Y4], Y)}.

%! date2(?Year:between(0,99), ?Month:between(1,12), ?Day:between(0,99))//
% ~~~{.abnf}
% date2 = 2DIGIT "-" month "-" 2DIGIT   ; day-month-year (e.g., 02-Jun-82)
% ~~~

date2(Y, M, D) -->
  % Day
  dcg_multi2('DIGIT', 2-2, _, [Y1,Y2]),
  {digits_to_decimal([Y1,Y2], Y)},
  "-",
  
  % Month
  month(M),
  "-",
  
  % Year
  dcg_multi2('DIGIT', 2-2, _, [D1,D2]),
  {digits_to_decimal([D1,D2], D)}.

%! date3(?Month:between(1,12), ?Day:between(0,99))//
% ~~~{.abnf}
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))   ; month day (e.g., Jun  2)
% ~~~

date3(M, D) -->
  % Month
  month(M),
  'SP',
  
  % Day
  (
    dcg_multi2('DIGIT', 2-2, _, [D1,D2]),
    {digits_to_decimal([D1,D2], D)}
  ;
    'SP',
    'DIGIT'(_, D)
  ).

%! 'delta-seconds'(?Seconds:nonneg)//
% Some HTTP header fields allow a time value to be specified as an
% integer number of seconds, represented in decimal, after the time
% that the message was received.

'delta-seconds'(S) -->
  dcg_multi('DIGIT', 1-_, Ss),
  {digits_to_decimal(Ss, S)}.

%! 'HTTP-date'(Y, M, D, H, MM, S)//
% ~~~{.abnf}
% HTTP-date  = rfc1123-date | rfc850-date | asctime-date
% ~~~

'HTTP-date'(Y, M, D, H, MM, S) --> 'rfc1123-date'(Y, M, D, H, MM, S).
'HTTP-date'(Y, M, D, H, MM, S) --> 'rfc850-date'(Y, M, D, H, MM, S).
'HTTP-date'(Y, M, D, H, MM, S) --> 'asctime-date'(Y, M, D, H, MM, S).

%! month(?Month:between(1,12))//
% ~~~{.abnf}
% month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug"
%       | "Sep" | "Oct" | "Nov" | "Dec"
% ~~~

month(1) --> "Jan".
month(2) --> "Feb".
month(3) --> "Mar".
month(4) --> "Apr".
month(5) --> "May".
month(6) --> "Jun".
month(7) --> "Jul".
month(8) --> "Aug".
month(9) --> "Sep".
month(10) --> "Oct".
month(11) --> "Nov".
month(12) --> "Dec".

%! 'rfc1123-date'(Y, M, D, H, MM, S)//
% ~~~{.abnf}
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ~~~

% @tbd Conversion between `D1` and `D2`.
'rfc1123-date'(Y, M, D, H, MM, S) -->
  wkday(D1),
  ",",
  'SP',
  date1(Y, M, D2),
  'SP',
  time(H, MM, S),
  'SP',
  "GMT".

%! 'rfc850-date'(Y, M, D, H, MM, S)//
% ~~~{.abnf}
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ~~~

% @tbd Conversion between `D1` and `D2`.
'rfc850-date'(Y, M, D, H, MM, S) -->
  weekday(D1),
  ",",
  'SP',
  date2(Y, M, D2),
  'SP',
  time(H, MM, S),
  'SP',
  "GMT".

%! time(?Hour:between(0,99), ?Minute:between(0,99), ?Second:between(0,99))//
% ~~~{.abnf}
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT   ; 00:00:00 - 23:59:59
% ~~~

time(H, MM, S) -->
  dcg_multi2('DIGIT', 2-2, _, [H1,H2]),
  {digits_to_decimal([H1,H2], H)},
  ":",
  dcg_multi2('DIGIT', 2-2, _, [MM1,MM2]),
  {digits_to_decimal([MM1,MM2], MM)},
  ":",
  dcg_multi2('DIGIT', 2-2, _, [S1,S2]),
  {digits_to_decimal([S1,S2], S)}.

%! weekday(?Day:between(1,7))//
% ~~~{.abnf}
% weekday = "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday"
%         | "Saturday" | "Sunday"
% ~~~

weekday(1) --> "Monday".
weekday(2) --> "Tuesday".
weekday(3) --> "Wednesday".
weekday(4) --> "Thursday".
weekday(5) --> "Friday".
weekday(6) --> "Saturday".
weekday(7) --> "Sunday".

%! wkday(?Day:between(1,7))//
% ~~~{.abnf}
% wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
% ~~~

wkday(1) --> "Mon".
wkday(2) --> "Tue".
wkday(3) --> "Wed".
wkday(4) --> "Thu".
wkday(5) --> "Fri".
wkday(6) --> "Sat".
wkday(7) --> "Sun".

