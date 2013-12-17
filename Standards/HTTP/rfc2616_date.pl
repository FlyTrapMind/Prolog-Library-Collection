:- module(
  rfc2616_date,
  [
    'delta-seconds'//1, % ?Seconds:nonneg
    'HTTP-date'//6 % ?Year:between(0,9999)
                   % ?Month:between(1,12)
                   % ?Day:between(0,99)
                   % ?Hour:between(0,99)
                   % ?Minute:between(0,99)
                   % ?Seconds:between(0,99)
  ]
).

/** <module> RFC 2616 date-time

Date-time values for RFC 2616 (HTTP 1.1).

# RFC 2616

## Syntax

~~~{.abnf}
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

## Formats

HTTP applications have historically allowed three different formats
 for the representation of date/time stamps:
~~~{.txt}
[1]   Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
[2]   Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
[3]   Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
~~~

The first format is preferred as an Internet standard and represents
 a fixed-length subset of that defined by RFC 1123
 (an update to RFC 822).
The second format is in common use,
 but is based on the obsolete RFC 850 date format
 and lacks a four-digit year.

HTTP-date is case sensitive and MUST NOT include additional `LWS`
 beyond that specifically included as `SP` in the grammar.

@see RFC 822
@see RFC 850
@see RFC 1123

## Conformance

### Parsing

HTTP/1.1 clients and servers that parse the date value MUST accept
 all three formats (for compatibility with HTTP/1.0).

Note: Recipients of date values are encouraged to be robust in
 accepting date values that may have been sent by non-HTTP
 applications, as is sometimes the case when retrieving or posting
 messages via proxies/gateways to SMTP or NNTP.

### Generation

HTTP/1.1 clients and servers MUST only generate the RFC 1123 format
 for representing HTTP-date values in header fields.

## Pragmatics

All HTTP date/time stamps MUST be represented in Greenwich Mean Time (GMT),
 without exception.
For the purposes of HTTP, GMT is exactly equal to
 UTC (Coordinated Universal Time).
This is indicated in the first two formats by the inclusion of `GMT`
 as the three-letter abbreviation for time zone,
 and MUST be assumed when reading the `asctime` format.

Note: HTTP requirements for the date/time stamp format apply only
 to their usage within the protocol stream. Clients and servers are not
 required to use these formats for user presentation, request logging, etc.

--

@author Wouter Beek
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(http(rfc2616_basic)).
:- use_module(math(radix)).



%! 'asctime-date'(Y, M, D, H, MM, S)//
% Date-time for ANSI C.
%
% ~~~{.abnf}
% asctime-date = wkday SP date3 SP time SP 4DIGIT
% ~~~
%
% @see ANSI C
% @see RFC 2616

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
% Date for RFC 1123.
%
% ~~~{.abnf}
% date1 = 2DIGIT SP month SP 4DIGIT   ; day month year (e.g., 02 Jun 1982)
% ~~~
%
% @see RFC 1123
% @see RFC 2616

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
% Date for RFC 850.
%
% ~~~{.abnf}
% date2 = 2DIGIT "-" month "-" 2DIGIT   ; day-month-year (e.g., 02-Jun-82)
% ~~~
%
% @see RFC 850
% @see RFC 2616

date2(Y, M, D) -->
  % Day
  dcg_multi1('DIGIT', 2-2, [Y1,Y2]),
  {digits_to_decimal([Y1,Y2], Y)},
  "-",
  
  % Month
  month(M),
  "-",
  
  % Year
  dcg_multi1('DIGIT', 2-2, [D1,D2]),
  {digits_to_decimal([D1,D2], D)}.



%! date3(?Month:between(1,12), ?Day:between(0,99))//
% Date for ANSI C.
%
% ~~~{.abnf}
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))   ; month day (e.g., Jun  2)
% ~~~
%
% @see ANSI C
% @see RFC 2616

date3(M, D) -->
  % Month
  month(M),
  'SP',
  
  % Day
  (
    dcg_multi1('DIGIT', 2-2, [D1,D2]),
    {digits_to_decimal([D1,D2], D)}
  ;
    'SP',
    'DIGIT'(D)
  ).



%! 'delta-seconds'(?Seconds:nonneg)//
% Some HTTP header fields allow a time value to be specified as an
%  integer number of seconds, represented in decimal,
%  after the time that the message was received.
%
% ~~~{.abnf}
% delta-seconds = 1*DIGIT
% ~~~
%
% @see RFC 2616

'delta-seconds'(S) -->
  dcg_multi1('DIGIT', 1-_, Ss),
  {digits_to_decimal(Ss, S)}.



%! 'HTTP-date'(
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(0,99),
%!   ?Hour:between(0,99),
%!   ?Minute:between(0,99),
%!   ?Second:between(0,99)
%! )//
% Date-time for HTTP.
%
% ~~~{.abnf}
% HTTP-date  = rfc1123-date | rfc850-date | asctime-date
% ~~~
%
% @see ANSI C
% @see RFC 850
% @see RFC 1123
% @see RFC 2616

'HTTP-date'(Y, M, D, H, MM, S) --> 'rfc1123-date'(Y, M, D, H, MM, S).
'HTTP-date'(Y, M, D, H, MM, S) --> 'rfc850-date'(Y, M, D, H, MM, S).
'HTTP-date'(Y, M, D, H, MM, S) --> 'asctime-date'(Y, M, D, H, MM, S).



%! month(?Month:between(1,12))//
% Month names used in HTTP dates.
%
% ~~~{.abnf}
% month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug"
%       | "Sep" | "Oct" | "Nov" | "Dec"
% ~~~
%
% @see RFC 2616

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



%! 'rfc1123-date'(
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(0,99),
%!   ?Hour:between(0,99),
%!   ?Minute:between(0,99),
%!   ?Second:between(0,99)
%! )//
% Date-time for RFC 1123.
%
% ~~~{.abnf}
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ~~~
%
% @see RFC 1123
% @see RFC 2616

'rfc1123-date'(Y, Mo, D, H, Mi, S) -->
  wkday(D),
  ",",
  'SP',
  date1(Y, Mo, D),
  'SP',
  time(H, Mi, S),
  'SP',
  "GMT".



%! 'rfc850-date'(
%!   ?Year:between(0,99),
%!   ?Month:between(1,12),
%!   ?Day:between(0,99),
%!   ?Hour:between(0,99),
%!   ?Minute:between(0,99),
%!   ?Second:between(0,99)
%! )//
% Date-time for RFC 850.
%
% ~~~{.abnf}
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ~~~
%
% @see RFC 850
% @see RFC 2616

'rfc850-date'(Y, Mo, D, H, Mi, S) -->
  weekday(D),
  ",",
  'SP',
  date2(Y, Mo, D),
  'SP',
  time(H, Mi, S),
  'SP',
  "GMT".



%! time(?Hour:between(0,99), ?Minute:between(0,99), ?Second:between(0,99))//
% Time for HTTP (for ANSI C, RFC 850, and RFC 1123).
%
% ~~~{.abnf}
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT   ; 00:00:00 - 23:59:59
% ~~~
%
% @see RFC 2616

time(H, M, S) -->
  dcg_multi1('DIGIT', 2-2, [H1,H2]),
  {digits_to_decimal([H1,H2], H)},
  ":",
  dcg_multi1('DIGIT', 2-2, [M1,M2]),
  {digits_to_decimal([M1,M2], M)},
  ":",
  dcg_multi1('DIGIT', 2-2, [S1,S2]),
  {digits_to_decimal([S1,S2], S)}.



%! weekday(?Day:between(1,7))//
% Full weekday names.
%
% ~~~{.abnf}
% weekday = "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday"
%         | "Saturday" | "Sunday"
% ~~~
%
% @RFC 2616

weekday(1) --> "Monday".
weekday(2) --> "Tuesday".
weekday(3) --> "Wednesday".
weekday(4) --> "Thursday".
weekday(5) --> "Friday".
weekday(6) --> "Saturday".
weekday(7) --> "Sunday".



%! wkday(?Day:between(1,7))//
% Abbreviated weekday names.
%
% ~~~{.abnf}
% wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
% ~~~
%
% @see RFC 2616

wkday(1) --> "Mon".
wkday(2) --> "Tue".
wkday(3) --> "Wed".
wkday(4) --> "Thu".
wkday(5) --> "Fri".
wkday(6) --> "Sat".
wkday(7) --> "Sun".

