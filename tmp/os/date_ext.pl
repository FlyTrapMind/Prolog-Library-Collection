:- module(
  date_ext,
  [
    hash_date/1, % -Hash:atom
    iso8601_date/2, % +Date:compound
                    % -Representation:atom
    posix_date/2, % -Date:compound
                  % -Representation:atom
    seconds/3 % ?Hours:integer
              % ?Minutes:integer
              % ?Second:integer
  ]
).

%! hash_date(-Hash:atom) is det.
% Returns the hash of the current timestamp.
%
% @arg Hash An atomic hash.

hash_date(Hash):-
  get_time(TimeStamp),
  variant_sha1(TimeStamp, Hash).



%! iso8601_date(+Date:compound, -Representation:atom) is det.

iso8601_date(Date, Representation):-
  format_time(atom(Representation), '%FT%T%z', Date).



%! posix_date(+Date:compound, -Representation:atom) is det.
% Returns the given date in POSIX format.
%
% @compat POSIX strfdate()
% @compat POSIX strftime()

posix_date(date(Y,Mo,D,H,Mi,S,_,_,_), Representation):-
  maplist(ground, [Y,Mo,D,H,Mi,S]), !,
  format_time(atom(Representation), '%FT%T', date(Y,Mo,D,H,Mi,S,_,_,_)).
posix_date(date(Y,Mo,D,_,_,_,_,_,_), Representation):-
  maplist(ground, [Y,Mo,D]), !,
  format_time(atom(Representation), '%F', date(Y,Mo,D)).



%! seconds(?Hours:integer, ?Minutes:integer, ?Seconds:integer) is det.
% Converts hours and minutes into seconds and vice versa.
%
% @arg Hours An integer
% @arg Minutes An integer
% @arg Seconds An integer

seconds(H, Mi, S):-
  nonvar(S), !,
  Mi is S mod 60,
  H is S / 60.
seconds(H, Mi, S):-
  defval(0, H),
  defval(0, Mi),
  S is (Mi + (H * 60)) * 60.
