:- module(
  dcg_char,
  [
    char//1, % ?Char:char
    char_ci//1, % ?Char:char
    char_lower//1, % ?Char:char
    char_upper//1 % ?Char:char
  ]
).

/** <module> DCG character

Grammar rules for processing characters.

# Parsing

Read letters in either case, prefering lowercase
 (i.e., the first solution is all-lowercase).

```prolog
?- dcg_phrase(char_upper(Char), 'Q').
Char = 'q';
Char = 'Q'.
```

# Generating

Write letters according to the predicate's semantics,
 e.g., code_upper//1 writes uppercase letters.
It does not matter in which case the letters are supplied
 in the instantiated argument.

```prolog
?- dcg_phrase(char_upper(q), Atom).
Atom = 'Q'.
```

--

@author Wouter Beek
@version 2014/11
*/

:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_meta)).

:- meta_predicate(char_code_metacall(3,?,?,?)).



%! char(?Char:char)// .

char(Char) -->
  char_code_metacall(code, Char).



%! char_ci(?Char:char)// .

char_ci(Char) -->
  char_code_metacall(code_ci, Char).



%! char_lower(?Char:char)// .

char_lower(Char) -->
  char_code_metacall(code_lower, Char).



%! char_upper(?Char:char)// .

char_upper(Char) -->
  char_code_metacall(code_upper, Char).



% HELPERS

char_code_metacall(Goal, Char) -->
  {var(Char)}, !,
  dcg_call(Goal, Code),
  {char_code(Char, Code)}.
char_code_metacall(Goal, Char) -->
  {char_code(Char, Code)},
  dcg_call(Goal, Code).
