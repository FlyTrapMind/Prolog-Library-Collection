:- module(
  swipl_ext,
  [
    check_prolog_version/0
  ]
).

/** <module> SWIPL_EXT

Predicate for SWI-Prolog.

@author Wouter Beek
@version 2013/06
*/



%! check_prolog_version is semidet.
% Checks whether a correct version of SWI-Prolog is installed.

check_prolog_version:-
  current_prolog_flag(version, CurrentVersion),
  minimum_prolog_version(MinimumMajor/MinimumMinor/MinimumPatch),
  MinimumVersion is
    (MinimumMajor * 10000) + (MinimumMinor * 100) + (MinimumPatch),
  (
    CurrentVersion >= MinimumVersion
  ->
    true
  ;
    print_message(
      error,
      outdated_version(swipl, CurrentVersion, MinimumVersion)
    ),
    fail
  ).

%! minimum_prolog_version(-Version:compound) is det.
% The minimal SWI-Prolog version that is needed for the features the
% application uses.
%
% During active development, i.e. now, I pay little attention to
% compatibility with older SWI-Prolog versions. I try to run the
% latest development release on Linux systems (currently:
% Arch and Fedora) and Windows systems (currently: 7 and 8)
% (all my systems are 64-bit). I always try to use new SWI-Prolog
% features immediately in order to keep up with recent advances in
% logic programming. I do not try to keep the codebase compatible
% with other Prologs (e.g., Yap), which is a nontrivial chore in the
% absense of broad standards. All this means that the required version
% number is probably set higher than it need be for most functionality.
% Thus, feel free to lower the number and try the PGC out on an older
% SWI-Prolog version to your liking.
%
% This is currently set to 6.3.15.
% 6.2.6 is the latest stable release.
% 6.3.15 is the latest development release.
%
% @arg The version indicator is of the form =|Major/Minor/Paths|=,
%      with three integers.

minimum_prolog_version(6/3/15).

prolog:message(outdated_version(Component, Current, Minimum)) -->
  [
    ansi([fg(red), intensity(normal)], 'Your version of ', []),
    ansi([bold, fg(red)], '~w', [Component]),
    ansi(
      [fg(red), intensity(normal)],
      ' is outdated. You are using version ',
      []
    )
  ],
  prolog:message(version(Current)),
  [
    ansi(
      [fg(red), intensity(normal)],
      ' whereas the minimum required version is ',
      []
    )
  ],
  prolog:message(version(Minimum)).
prolog:message(version(Version)) -->
  {
    Major is Version // 10000,
    Minor is (Version rem Major) // 100,
    Patch is Version rem 100
  },
  ['~w.~w.~w'-[Major, Minor, Patch]].

