:- module(
  dcg_replace,
  [
    dcg_replace//2 % +From:list(code)
                   % +To:list(code)
  ]
).

/** <module> DCG replace

DCG rules for replacing content.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- use_module(dcg(dcg_generic)).

:- meta_predicate(dcg_replace(//,//,?,?)).



%! dcg_replace(:From:dcg, :To:dcg)// is det.
% @author http://stackoverflow.com/users/1613573/mat
% @see http://stackoverflow.com/questions/6392725/using-a-prolog-dcg-to-find-replace-code-review

dcg_replace(_From, _To) -->
  dcg_end, !.
dcg_replace(From, To), To -->
  From, !,
  dcg_replace(From, To).
dcg_replace(From, To), [X] -->
  [X],
  dcg_replace(From, To).
