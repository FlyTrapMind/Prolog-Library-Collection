:- module(
  swipl_ext,
  [
    check_prolog_version/0,
    check_prolog_version/1, % +MinimumVersion:nonneg
    check_prolog_version/3 % ?MinimumMajor:nonneg
                           % ?MinimumMinor:nonneg
                           % ?MinimumPatch:nonneg
  ]
).

/** <module> SWIPL_EXT

Predicates that are specific to the operation of SWI-Prolog.

@author Wouter Beek
@version 2013/06, 2013/08
*/

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_multi)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(ansi_term)). % Used in markup.



%! check_prolog_version is semidet.
% Checks whether a sufficient version of SWI-Prolog is installed for the PGC.

check_prolog_version:-
  minimum_prolog_version(MinimumMajor, MinimumMinor, MinimumPatch),
  check_prolog_version(MinimumMajor, MinimumMinor, MinimumPatch).

%! check_prolog_version(+MinimumVersion:nonneg) is semidet.
% Checks whether at least the given version of SWI-Prolog is installed.
% (to be used for non-PGC project requirements that exceed
% the PGC requirement).
%
% @param MinimumVersion:

check_prolog_version(MinimumVersion):-
  % Type check.
  nonneg(MinimumVersion), !,

  % Retrieve the current version as a single integer.
  current_prolog_flag(version, CurrentVersion1),
  (
    % The representation used since SWI-Prolog 2.7.10.
    integer(CurrentVersion1)
  ->
    CurrentVersion2 = CurrentVersion1
  ;
    atom_codes(CurrentVersion1, Codes),
    % The representation used before SWI-Prolog 2.7.10.
    phrase(prolog_version(CurrentVersion2), Codes)
  ),

  (
    CurrentVersion2 >= MinimumVersion, !
  ;
    print_message(
      error,
      outdated_version(swipl, CurrentVersion2, MinimumVersion)
    )
  ).

%! check_prolog_version(
%!   ?MinimumMajor:nonneg,
%!   ?MinimumMinor:nonneg,
%!   ?MinimumPatch:nonneg
%! ) is semidet.
% Checks whether at least the given version of SWI-Prolog is installed
% (to be used for non-PGC project requirements that exceed
% the PGC requirement).

check_prolog_version(MinimumMajor, MinimumMinor, MinimumPatch):-
  % Calculate the minimum version as an integer.
  major_minor_patch_to_integer(
    MinimumMajor,
    MinimumMinor,
    MinimumPatch,
    MinimumVersion
  ),
  check_prolog_version(MinimumVersion).

%! major_minor_patch_to_integer(
%!   ?Major:nonneg,
%!   ?Minor:nonneg,
%!   ?Patch:nonneg,
%!   -Version:integer
%! ) is det.

major_minor_patch_to_integer(Major1, Minor1, Patch1, Version):-
  default(Major1, 0, Major2),
  default(Minor1, 0, Minor2),
  default(Patch1, 0, Patch2),
  Version is (Major2 * 10000) + (Minor2 * 100) + Patch2.

%! minimum_prolog_version(
%!   ?Major:nonneg,
%!   ?Minor:nonneg,
%!   ?Patch:nonneg
%! ) is nondet.
% The minimum SWI-Prolog version that is needed for the features the
% PGC project uses.
%
% During active development, i.e. now, I pay little attention to
% compatibility with older SWI-Prolog versions.
% I usually run a SWI-Prolog version that is compiled off of the
% development Git branch.
% As OS I mostly use Fedora (currently 19) and Windows (currently 8.1).
%
% I usually try to use new SWI-Prolog features as soon as they come out
% in order to keep up with recent advances in logic programming.
% I do not try to keep the codebase compatible
% with other Prologs (e.g., Yap), which is a nontrivial chore in the
% absense of broad standards.
%
% All this means that the version that is required for running the PGC
% is probably higher than it need be for most functionality.
% Thefore, one should feel free to lower the number and try the PGC out on
% older SWI-Prolog versions as well.
%
% 6.4.1 is the latest stable release.
% 6.5.1 is the latest development release.

minimum_prolog_version(6, 5, 2).

prolog:message(outdated_version(Component, Current, Minimum)) -->
  [
    ansi([fg(red),intensity(normal)], 'Your version of ', []),
    ansi([bold,fg(red)], '~w', [Component]),
    ansi(
      [fg(red),intensity(normal)],
      ' is outdated. You are using version ',
      []
    )
  ],
  prolog:message(version(Current)),
  [
    ansi(
      [fg(red),intensity(normal)],
      ' whereas the minimum required version is ',
      []
    )
  ],
  prolog:message(version(Minimum)).
prolog:message(version(Version)) -->
  {
    Major is Version // 10000,
    Minor is (Version rem 10000) // 100,
    Patch is Version rem 100
  },
  ['~w.~w.~w'-[Major,Minor,Patch]].

%! prolog_version(-Version:nonneg)//
% Before SWI-Prolog 2.7.10 the version was stored in a dot-separated atom.

prolog_version(Version) -->
  dcg_multi1(decimal_number, 3, [Major,Minor,Patch], [separator(comma)]),
  {major_minor_patch_to_integer(Major, Minor, Patch, Version)}.

