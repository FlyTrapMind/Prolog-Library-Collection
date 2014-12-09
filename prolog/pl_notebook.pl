:- module(
  pl_notebook,
  [
    variant2/2 % @Term1
               % @Term2
  ]
).

/** <module> Prolog notebook

Scattered ideas about programming in Prolog that I learned mostly
 from other, more experience Prolog programmers, either on IRC or on the Web.


# Resources

  - [Production Logic](http://youtu.be/G_eYTctGZw8), Michael Hendricks,
    Presentation at Strangeloop.


# Answer, Solution, Success

"Success is not what it used to be."
@author Ulrich Neumerkel

An answer need not contain a solution (e.g., `X = t(_)`).


# Answers as executable queries

Beginning with Prolog IV, answer can be returned as executable queries
E.g., `yes` -> `true`, `no` -> `false`.
This allows answers to be inserted into the next query.


# Constraints

The following built-ins are known to be incompatible with constraints:

  - forall/2 (uses non-monotonic constructs)
  - foreach/2
  - include/3
  - setof/3

```sicstus
?- setof(t, (I in 1..3 ; I in 3..5 ), _).
yes
```

```swipl
?- setof(t, (I in 1..3 ; I in 3..5 ),_).
I = 3.
```


# `dif/2`

```prolog
union([], X, X).
union([X|Y], L, S):-
  memberchk(X, L),
  union(Y, L, S).
union([X|Y], L, [X|S]):-
  maplist(dif(X), L),
  union(Y, L, S).
```

```prolog
?- union([A],[B],[C,D]).
A = C,
B = D,
dif(C, D).
```

@author Ulrich Neumerkel



# First-argument indexing

```prolog
[1]   predicate([]):- something1.
[2]   predicate([H|T]):- condition, !, someting2.
[3]   predicate([H|T]):- something3.
```

In the following rewrite, first argument-indexing is used to avoid trying
 clauses during a proof that cannot be used to resolve the current goal.
Specifically, the choice-point for choosing [2] rather than [3]
 is no longer created.

```prolog
predicate([]).
predicate([N|L]):- (condition -> something2 ; something3).
```

@author Paulo Moura



# Prolog flags

## Double quotes

```prolog
[1]   set_prolog_flag(double_quotes, atom).
[2]   set_prolog_flag(double_quotes, chars).
[3]   set_prolog_flag(double_quotes, codes).
```


## Unknown

```prolog
[1]   set_prolog_flag(unknown, error).
[2]   set_prolog_flag(unknown, fail).
```

[1] throws an error if a predicate name is use but not defined
 or declared dynamic.



# Steadfastness

```prolog
remove_even_integers([], []).
remove_even_integers([H|T1], [H|T2]):-
  odd(H), !,
  remove_even_integers(T1, T2).
remove_even_integers([_|T1], T2):-
  remove_even_integers(T1, T2).
```



# Variant

```yap
?- [A,B] =@= [B,A].
false.
```

```swipl
?- [A,B] =@= [B,A]
true.
```

@author Paulo Moura



---

@author Wouter Beek
@version 2014/11-2014/12
*/


%! variant2(@Term1, @Term) is semidet.
% The use of double negation avoids instantiating any variables
%  in the arguments.
%
% @author Paulo Moura
% @see http://stackoverflow.com/questions/20711893/prolog-compare-list-structure/20713406#comment31025646_20711893

variant2(Term1, Term2):-
  \+ \+ subsumes_term(Term1, Term2),
  \+ \+ subsumes_term(Term2, Term1).
