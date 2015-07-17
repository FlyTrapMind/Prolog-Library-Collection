:- module(
  dcg_generics,
  [
    dcg_copy//0,
    dcg_done//0,
    dcg_end//0,
    dcg_sep_list//2, % :Separator
                     % ?Codess:list(list(code))
    dcg_rest//1, % -Rest:list(code)
    dcg_void//0,
    dcg_with_output_to/2, % +Output:compound
                          % :Dcg
    parsing//0
  ]
).

:- meta_predicate(dcg_sep_list(//,?,?,?)).
:- meta_predicate(dcg_with_output_to(+,//)).



dcg_copy(X, X).



dcg_done(_, _).



dcg_end([], []).



%! dcg_rest(-Rest:list(code))// is det.

dcg_rest(X, X, []).



%! dcg_sep_list(:Separator, ?Codess:list(list(code)))// .
% Succeeds when the code lists in Codess are processed
% and each is interspersed with Separators.

dcg_sep_list(Sep, [H|T]) -->
  dcg_sep_item(Sep, H), !,
  dcg_sep_list(Sep, T).
dcg_sep_list(_, []) --> "".

dcg_sep_item(Sep, []) -->
  Sep, !.
dcg_sep_item(Sep, [H|T]) -->
  [H],
  dcg_sep_item(Sep, T).
dcg_void --> [].



%! dcg_with_output_to(+Output:compound, :Dcg) is nondet.

dcg_with_output_to(Out, Dcg):-
  phrase(Dcg, Cs),
  with_output_to(Out, put_codes(Cs)).



%! parsing// is semidet.
% Succeeds if the DCG is in parse mode.

parsing(H, H):-
   nonvar(H).
