:- module(
  dcg_replace,
  [
    dcg_replace//1, % :Replacements:list(pair(dcg))
    dcg_replace//2 % :FromDCG
                   % :ToDCG
  ]
).

/** <module> DCG replace

DCG rules for replacing content.

@author Wouter Beek
@see http://stackoverflow.com/users/1613573/mat
@see http://stackoverflow.com/questions/6392725/using-a-prolog-dcg-to-find-replace-code-review
@version 2013/05-2013/09, 2013/11-2014/01
*/

:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).

:- meta_predicate(dcg_replace(//,?,?)).
:- meta_predicate(dcg_replace(//,//,?,?)).



%! dcg_replace(+Replacements:list)// is det.
% Generic DCG rule-based replacements.
%
% @arg Replacements A list of pairs of DCG rules.
%        Whenever the former rule is read, the latter rule is written.
%        Notice that the order of the replacement pairs does matter.

dcg_replace(Repl1) -->
  {strip_module(Repl1, Mod, Repl2)},
  '_dcg_replace'(Mod, Repl2).

'_dcg_replace'(_Mod, _Repl) -->
  dcg_end, !.
'_dcg_replace'(Mod, Repl), Mod:To -->
  {member(From-To, Repl)},
  Mod:From, !,
  '_dcg_replace'(Mod, Repl).
'_dcg_replace'(Mod, Repl), [X] -->
  [X],
  '_dcg_replace'(Mod, Repl).


%! dcg_replace(:FromDCG, :ToDCG)// is det.
% @see Wrapper around dcg_replace//1.

dcg_replace(From, To) -->
  dcg_replace([From-To]).

