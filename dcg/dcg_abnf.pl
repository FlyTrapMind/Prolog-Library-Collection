:- module(
  dcg_abnf,
  [
    '#'//2, '#'//3, '#'//4, '#'//5, '#'//6, '#'//7,
    '#_s'//2, '#_s'//3, '#_s'//4, '#_s'//5, '#_s'//6, '#_s'//7,
    '*'//1, '*'//2, '*'//3, '*'//4, '*'//5, '*'//6,
    '*_c'//2, '*_c'//3, '*_c'//4, '*_c'//5, '*_c'//6, '*_c'//7,
    '*_s'//1, '*_s'//2, '*_s'//3, '*_s'//4, '*_s'//5, '*_s'//6,
    '*_s_c'//2, '*_s_c'//3, '*_s_c'//4, '*_s_c'//5, '*_s_c'//6, '*_s_c'//7,
    '*n'//2, '*n'//3, '*n'//4, '*n'//5, '*n'//6, '*n'//7,
    '*n_c'//3, '*n_c'//4, '*n_c'//5, '*n_c'//6, '*n_c'//7, '*n_c'//8,
    '*n_s'//2, '*n_s'//3, '*n_s'//4, '*n_s'//5, '*n_s'//6, '*n_s'//7,
    '*n_s_c'//3, '*n_s_c'//4, '*n_s_c'//5, '*n_s_c'//6, '*n_s_c'//7, '*n_s_c'//8,
    '+'//1, '+'//2, '+'//3, '+'//4, '+'//5, '+'//6,
    '+_c'//2, '+_c'//3, '+_c'//4, '+_c'//5, '+_c'//6, '+_c'//7,
    '+_s'//1, '+_s'//2, '+_s'//3, '+_s'//4, '+_s'//5, '+_s'//6,
    '+_s_c'//2, '+_s_c'//3, '+_s_c'//4, '+_s_c'//5, '+_s_c'//6, '+_s_c'//7,
    '?'//1, '?'//2, '?'//3, '?'//4, '?'//5, '?'//6,
    '?_c'//2, '?_c'//3, '?_c'//4, '?_c'//5, '?_c'//6, '?_c'//7,
    'm*'//2, 'm*'//3, 'm*'//4, 'm*'//5, 'm*'//6, 'm*'//7,
    'm*_c'//3, 'm*_c'//4, 'm*_c'//5, 'm*_c'//6, 'm*_c'//7, 'm*_c'//8,
    'm*_s'//2, 'm*_s'//3, 'm*_s'//4, 'm*_s'//5, 'm*_s'//6, 'm*_s'//7,
    'm*_s_c'//3, 'm*_s_c'//4, 'm*_s_c'//5, 'm*_s_c'//6, 'm*_s_c'//7, 'm*_s_c'//8,
    'm*n'//3, 'm*n'//4, 'm*n'//5, 'm*n'//6, 'm*n'//7, 'm*n'//8,
    'm*n_c'//4, 'm*n_c'//5, 'm*n_c'//6, 'm*n_c'//7, 'm*n_c'//8, 'm*n_c'//9,
    'm*n_s'//3, 'm*n_s'//4, 'm*n_s'//5, 'm*n_s'//6, 'm*n_s'//7, 'm*n_s'//8,
    'm*n_s_c'//4, 'm*n_s_c'//5, 'm*n_s_c'//6, 'm*n_s_c'//7, 'm*n_s_c'//8, 'm*n_s_c'//9
  ]
).

/** <module> Descriptive Clause Grammar: Augmented Backus-Naur Form

DCG emulation of useful ABNF constructs.

### Counting the number of DCG calls

The predicates defined in this module have a variant that returns
the exact number of DCG calls.
These variants are indicated by the `_c` suffix, which stands for 'count'.

Example of counting the number of DCG calls:
~~~{.pl}
?- phrase('*_c'(Count, arrow(Head, Length)), `<--->`).
Count = 1 ;   % `<--->`
Count = 2 ;   % `<---` and `>`
Count = 2 ;   % `<--` and `->`
Count = 2 ;   % `<-` and `-->`
Count = 2 ;   % `<` and `--->`
false.
~~~

### Uninstantiated variables: shared or not?

The DCG rules defined in this module (except for '?'//1 and '?_c'//2)
have a `_s`-variant, where the `s` stands for 'shared'.

For the variant without `_s`, a new copy of the DCG rule is called each time.
For the variant with `_s`, the uninstantiated variables are shared
between all DCG calls.
We illustrate this distinction with an example.

The following generates all sequences of at most 2 arrows
surrounded by triple quotes, with uninstantiated variables shared
between successive calls of `Dcg`.
The output shows that the single and double quote characters
do not occur in the same string.
~~~{.pl}
?- phrase('*n_s'(2, quoted(triple_quote(_), arrow(right, 8))), Codes),
   atom_codes(Atom, Codes).
Codes = [],
Atom = '' ;
Codes = """"------->"""",
Atom = '"""------->"""' ;
Codes = """"------->""""""------->"""",
Atom = '"""------->""""""------->"""' ;
Codes = "'''------->'''",
Atom = '\'\'\'------->\'\'\'' ;
Codes = "'''------->''''''------->'''",
Atom = '\'\'\'------->\'\'\'\'\'\'------->\'\'\'' ;
false.
~~~

The following generates all sequences of at most 2 arrows
surrounded by triple quotes, without sharing variables
between successive calls of `Dcg`.
The output shows that the single and double quote characters
do not occur in the same string.
~~~{.pl}
?- phrase('*n'(2, quoted(triple_quote(_), arrow(right, 8))), Codes),
   atom_codes(Atom, Codes).
Codes = [],
Atom = '' ;
Codes = """"------->"""",
Atom = '"""------->"""' ;
Codes = """"------->""""""------->"""",
Atom = '"""------->""""""------->"""' ;
Codes = """"------->"""'''------->'''",
Atom = '"""------->"""\'\'\'------->\'\'\'' ;
Codes = "'''------->'''",
Atom = '\'\'\'------->\'\'\'' ;
Codes = "'''------->'''"""------->"""",
Atom = '\'\'\'------->\'\'\'"""------->"""' ;
Codes = "'''------->''''''------->'''",
Atom = '\'\'\'------->\'\'\'\'\'\'------->\'\'\'' ;
false.
~~~

### Determinism

@tbd Oftentimes we want '#'//[2-7] and '#_c'//[2-7] to be deterministic,
but this module does not detect whether a DCG rule is deterministic or not.

--

@author Wouter Beek
@version 2014/05-2014/06
*/

:- use_module(library(apply)).
:- use_module(library(error)).

:- use_module(dcg(dcg_meta)).
:- use_module(generics(meta_ext)).

:- meta_predicate('#'(?,//,?,?)).
:- meta_predicate('#'(?,3,?,?,?)).
:- meta_predicate('#'(?,4,?,?,?,?)).
:- meta_predicate('#'(?,5,?,?,?,?,?)).
:- meta_predicate('#'(?,6,?,?,?,?,?,?)).
:- meta_predicate('#'(?,7,?,?,?,?,?,?,?)).
:- meta_predicate('#_s'(?,//,?,?)).
:- meta_predicate('#_s'(?,3,?,?,?)).
:- meta_predicate('#_s'(?,4,?,?,?,?)).
:- meta_predicate('#_s'(?,5,?,?,?,?,?)).
:- meta_predicate('#_s'(?,6,?,?,?,?,?,?)).
:- meta_predicate('#_s'(?,7,?,?,?,?,?,?,?)).
:- meta_predicate('*'(//,?,?)).
:- meta_predicate('*'(3,?,?,?)).
:- meta_predicate('*'(4,?,?,?,?)).
:- meta_predicate('*'(5,?,?,?,?,?)).
:- meta_predicate('*'(6,?,?,?,?,?,?)).
:- meta_predicate('*'(7,?,?,?,?,?,?,?)).
:- meta_predicate('*_c'(-,//,?,?)).
:- meta_predicate('*_c'(-,3,?,?,?)).
:- meta_predicate('*_c'(-,4,?,?,?,?)).
:- meta_predicate('*_c'(-,5,?,?,?,?,?)).
:- meta_predicate('*_c'(-,6,?,?,?,?,?,?)).
:- meta_predicate('*_c'(-,7,?,?,?,?,?,?,?)).
:- meta_predicate('*_s'(//,?,?)).
:- meta_predicate('*_s'(3,?,?,?)).
:- meta_predicate('*_s'(4,?,?,?,?)).
:- meta_predicate('*_s'(5,?,?,?,?,?)).
:- meta_predicate('*_s'(6,?,?,?,?,?,?)).
:- meta_predicate('*_s'(7,?,?,?,?,?,?,?)).
:- meta_predicate('*_s_c'(-,//,?,?)).
:- meta_predicate('*_s_c'(-,3,?,?,?)).
:- meta_predicate('*_s_c'(-,4,?,?,?,?)).
:- meta_predicate('*_s_c'(-,5,?,?,?,?,?)).
:- meta_predicate('*_s_c'(-,6,?,?,?,?,?,?)).
:- meta_predicate('*_s_c'(-,7,?,?,?,?,?,?,?)).
:- meta_predicate('*n'(?,//,?,?)).
:- meta_predicate('*n'(?,3,?,?,?)).
:- meta_predicate('*n'(?,4,?,?,?,?)).
:- meta_predicate('*n'(?,5,?,?,?,?,?)).
:- meta_predicate('*n'(?,6,?,?,?,?,?,?)).
:- meta_predicate('*n'(?,7,?,?,?,?,?,?,?)).
:- meta_predicate('*n_c'(?,-,//,?,?)).
:- meta_predicate('*n_c'(?,-,3,?,?,?)).
:- meta_predicate('*n_c'(?,-,4,?,?,?,?)).
:- meta_predicate('*n_c'(?,-,5,?,?,?,?,?)).
:- meta_predicate('*n_c'(?,-,6,?,?,?,?,?,?)).
:- meta_predicate('*n_c'(?,-,7,?,?,?,?,?,?,?)).
:- meta_predicate('*n_s'(?,//,?,?)).
:- meta_predicate('*n_s'(?,3,?,?,?)).
:- meta_predicate('*n_s'(?,4,?,?,?,?)).
:- meta_predicate('*n_s'(?,5,?,?,?,?,?)).
:- meta_predicate('*n_s'(?,6,?,?,?,?,?,?)).
:- meta_predicate('*n_s'(?,7,?,?,?,?,?,?,?)).
:- meta_predicate('*n_s_c'(?,-,//,?,?)).
:- meta_predicate('*n_s_c'(?,-,3,?,?,?)).
:- meta_predicate('*n_s_c'(?,-,4,?,?,?,?)).
:- meta_predicate('*n_s_c'(?,-,5,?,?,?,?,?)).
:- meta_predicate('*n_s_c'(?,-,6,?,?,?,?,?,?)).
:- meta_predicate('*n_s_c'(?,-,7,?,?,?,?,?,?,?)).
:- meta_predicate('+'(//,?,?)).
:- meta_predicate('+'(3,?,?,?)).
:- meta_predicate('+'(4,?,?,?,?)).
:- meta_predicate('+'(5,?,?,?,?,?)).
:- meta_predicate('+'(6,?,?,?,?,?,?)).
:- meta_predicate('+'(7,?,?,?,?,?,?,?)).
:- meta_predicate('+_c'(-,//,?,?)).
:- meta_predicate('+_c'(-,3,?,?,?)).
:- meta_predicate('+_c'(-,4,?,?,?,?)).
:- meta_predicate('+_c'(-,5,?,?,?,?,?)).
:- meta_predicate('+_c'(-,6,?,?,?,?,?,?)).
:- meta_predicate('+_c'(-,7,?,?,?,?,?,?,?)).
:- meta_predicate('+_s'(//,?,?)).
:- meta_predicate('+_s'(3,?,?,?)).
:- meta_predicate('+_s'(4,?,?,?,?)).
:- meta_predicate('+_s'(5,?,?,?,?,?)).
:- meta_predicate('+_s'(6,?,?,?,?,?,?)).
:- meta_predicate('+_s'(7,?,?,?,?,?,?,?)).
:- meta_predicate('+_s_c'(-,//,?,?)).
:- meta_predicate('+_s_c'(-,3,?,?,?)).
:- meta_predicate('+_s_c'(-,4,?,?,?,?)).
:- meta_predicate('+_s_c'(-,5,?,?,?,?,?)).
:- meta_predicate('+_s_c'(-,6,?,?,?,?,?,?)).
:- meta_predicate('+_s_c'(-,7,?,?,?,?,?,?,?)).
:- meta_predicate('?'(//,?,?)).
:- meta_predicate('?'(3,?,?,?)).
:- meta_predicate('?'(4,?,?,?,?)).
:- meta_predicate('?'(5,?,?,?,?,?)).
:- meta_predicate('?'(6,?,?,?,?,?,?)).
:- meta_predicate('?'(7,?,?,?,?,?,?,?)).
:- meta_predicate('?_c'(-,//,?,?)).
:- meta_predicate('?_c'(-,3,?,?,?)).
:- meta_predicate('?_c'(-,4,?,?,?,?)).
:- meta_predicate('?_c'(-,5,?,?,?,?,?)).
:- meta_predicate('?_c'(-,6,?,?,?,?,?,?)).
:- meta_predicate('?_c'(-,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*'(?,//,?,?)).
:- meta_predicate('m*'(?,3,?,?,?)).
:- meta_predicate('m*'(?,4,?,?,?,?)).
:- meta_predicate('m*'(?,5,?,?,?,?,?)).
:- meta_predicate('m*'(?,6,?,?,?,?,?,?)).
:- meta_predicate('m*'(?,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*_c'(?,-,//,?,?)).
:- meta_predicate('m*_c'(?,-,3,?,?,?)).
:- meta_predicate('m*_c'(?,-,4,?,?,?,?)).
:- meta_predicate('m*_c'(?,-,5,?,?,?,?,?)).
:- meta_predicate('m*_c'(?,-,6,?,?,?,?,?,?)).
:- meta_predicate('m*_c'(?,-,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*_s'(?,//,?,?)).
:- meta_predicate('m*_s'(?,3,?,?,?)).
:- meta_predicate('m*_s'(?,4,?,?,?,?)).
:- meta_predicate('m*_s'(?,5,?,?,?,?,?)).
:- meta_predicate('m*_s'(?,6,?,?,?,?,?,?)).
:- meta_predicate('m*_s'(?,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*_s_c'(?,-,//,?,?)).
:- meta_predicate('m*_s_c'(?,-,3,?,?,?)).
:- meta_predicate('m*_s_c'(?,-,4,?,?,?,?)).
:- meta_predicate('m*_s_c'(?,-,5,?,?,?,?,?)).
:- meta_predicate('m*_s_c'(?,-,6,?,?,?,?,?,?)).
:- meta_predicate('m*_s_c'(?,-,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*n'(?,?,//,?,?)).
:- meta_predicate('m*n'(?,?,3,?,?,?)).
:- meta_predicate('m*n'(?,?,4,?,?,?,?)).
:- meta_predicate('m*n'(?,?,5,?,?,?,?,?)).
:- meta_predicate('m*n'(?,?,6,?,?,?,?,?,?)).
:- meta_predicate('m*n'(?,?,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*n_c'(?,?,-,//,?,?)).
:- meta_predicate('m*n_c'(?,?,-,3,?,?,?)).
:- meta_predicate('m*n_c'(?,?,-,4,?,?,?,?)).
:- meta_predicate('m*n_c'(?,?,-,5,?,?,?,?,?)).
:- meta_predicate('m*n_c'(?,?,-,6,?,?,?,?,?,?)).
:- meta_predicate('m*n_c'(?,?,-,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*n_s'(?,?,//,?,?)).
:- meta_predicate('m*n_s'(?,?,3,?,?,?)).
:- meta_predicate('m*n_s'(?,?,4,?,?,?,?)).
:- meta_predicate('m*n_s'(?,?,5,?,?,?,?,?)).
:- meta_predicate('m*n_s'(?,?,6,?,?,?,?,?,?)).
:- meta_predicate('m*n_s'(?,?,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*n_s_c'(?,?,-,//,?,?)).
:- meta_predicate('m*n_s_c'(?,?,-,3,?,?,?)).
:- meta_predicate('m*n_s_c'(?,?,-,4,?,?,?,?)).
:- meta_predicate('m*n_s_c'(?,?,-,5,?,?,?,?,?)).
:- meta_predicate('m*n_s_c'(?,?,-,6,?,?,?,?,?,?)).
:- meta_predicate('m*n_s_c'(?,?,-,7,?,?,?,?,?,?,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,//,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,3,?,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,4,?,?,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,5,?,?,?,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,6,?,?,?,?,-,?)).
:- meta_predicate('m*n_generate'(?,?,+,-,7,?,?,?,?,?,-,?)).
:- meta_predicate('m*n_s_generate'(?,?,+,-,//,-,?)).
:- meta_predicate('m*n_s_generate'(?,?,+,-,3,?,-,?)).
:- meta_predicate('m*n_s_generate'(?,?,+,-,4,?,?,-,?)).
:- meta_predicate('m*n_s_generate'(?,?,+,-,5,?,?,?,-,?)).
:- meta_predicate('m*n_s_generate'(?,?,+,-,6,?,?,?,?,-,?)).
:- meta_predicate('m*n_s_generate'(?,?,+,-,7,?,?,?,?,?,-,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,//,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,3,?,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,4,?,?,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,5,?,?,?,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,6,?,?,?,?,+,?)).
:- meta_predicate('m*n_parse'(?,?,+,-,7,?,?,?,?,?,+,?)).
:- meta_predicate('m*n_s_parse'(?,?,+,-,//,+,?)).
:- meta_predicate('m*n_s_parse'(?,?,+,-,3,?,+,?)).
:- meta_predicate('m*n_s_parse'(?,?,+,-,4,?,?,+,?)).
:- meta_predicate('m*n_s_parse'(?,?,+,-,5,?,?,?,+,?)).
:- meta_predicate('m*n_s_parse'(?,?,+,-,6,?,?,?,?,+,?)).
:- meta_predicate('m*n_s_parse'(?,?,+,-,7,?,?,?,?,?,+,?)).



%! '#'(+N:nonneg, :Dcg)// .
%! '#'(+N:nonneg, :Dcg, ?Args1:list, ...)// .
% @see Wrappers around 'm*n'//[3-8], using the same value for `M` and `N`.

'#'(N, Dcg) -->
  'm*n_c'(N, N, N, Dcg).

'#'(N, Dcg, L1) -->
  'm*n_c'(N, N, N, Dcg, L1).

'#'(N, Dcg, L1, L2) -->
  'm*n_c'(N, N, N, Dcg, L1, L2).

'#'(N, Dcg, L1, L2, L3) -->
  'm*n_c'(N, N, N, Dcg, L1, L2, L3).

'#'(N, Dcg, L1, L2, L3, L4) -->
  'm*n_c'(N, N, N, Dcg, L1, L2, L3, L4).

'#'(N, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_c'(N, N, N, Dcg, L1, L2, L3, L4, L5).


%! '#_s'(+N:nonneg, :Dcg)// .
%! '#_s'(+N:nonneg, :Dcg, ?Args1:list, ...)// .
% @see Wrappers around 'm*n'//[3-8], using the default value for `M`.

'#_s'(N, Dcg) -->
  'm*n_s_c'(N, N, N, Dcg).

'#_s'(N, Dcg, L1) -->
  'm*n_s_c'(N, N, N, Dcg, L1).

'#_s'(N, Dcg, L1, L2) -->
  'm*n_s_c'(N, N, N, Dcg, L1, L2).

'#_s'(N, Dcg, L1, L2, L3) -->
  'm*n_s_c'(N, N, N, Dcg, L1, L2, L3).

'#_s'(N, Dcg, L1, L2, L3, L4) -->
  'm*n_s_c'(N, N, N, Dcg, L1, L2, L3, L4).

'#_s'(N, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s_c'(N, N, N, Dcg, L1, L2, L3, L4, L5).


%! '*'(:Dcg)// .
%! '*'(:Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `*` in a nondeterministic way.

'*'(Dcg) -->
  'm*n'(_, _, Dcg).

'*'(Dcg, L1) -->
  'm*n'(_, _, Dcg, L1).

'*'(Dcg, L1, L2) -->
  'm*n'(_, _, Dcg, L1, L2).

'*'(Dcg, L1, L2, L3) -->
  'm*n'(_, _, Dcg, L1, L2, L3).

'*'(Dcg, L1, L2, L3, L4) -->
  'm*n'(_, _, Dcg, L1, L2, L3, L4).

'*'(Dcg, L1, L2, L3, L4, L5) -->
  'm*n'(_, _, Dcg, L1, L2, L3, L4, L5).


%! '*_c'(-Count:nonneg, :Dcg)// .
%! '*_c'(-Count:nonneg, :Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `*` in a nondeterministic way.

'*_c'(C, Dcg) -->
  'm*n_c'(_, _, C, Dcg).

'*_c'(C, Dcg, L1) -->
  'm*n_c'(_, _, C, Dcg, L1).

'*_c'(C, Dcg, L1, L2) -->
  'm*n_c'(_, _, C, Dcg, L1, L2).

'*_c'(C, Dcg, L1, L2, L3) -->
  'm*n_c'(_, _, C, Dcg, L1, L2, L3).

'*_c'(C, Dcg, L1, L2, L3, L4) -->
  'm*n_c'(_, _, C, Dcg, L1, L2, L3, L4).

'*_c'(C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_c'(_, _, C, Dcg, L1, L2, L3, L4, L5).


%! '*_s'(:Dcg)// .
%! '*_s'(:Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `*` in a nondeterministic way.

'*_s'(Dcg) -->
  'm*n_s'(_, _, Dcg).

'*_s'(Dcg, L1) -->
  'm*n_s'(_, _, Dcg, L1).

'*_s'(Dcg, L1, L2) -->
  'm*n_s'(_, _, Dcg, L1, L2).

'*_s'(Dcg, L1, L2, L3) -->
  'm*n_s'(_, _, Dcg, L1, L2, L3).

'*_s'(Dcg, L1, L2, L3, L4) -->
  'm*n_s'(_, _, Dcg, L1, L2, L3, L4).

'*_s'(Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s'(_, _, Dcg, L1, L2, L3, L4, L5).


%! '*_s_c'(-Count:nonneg, :Dcg)// .
%! '*_s_c'(-Count:nonneg, :Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `*` in a nondeterministic way.

'*_s_c'(C, Dcg) -->
  'm*n_s_c'(_, _, C, Dcg).

'*_s_c'(C, Dcg, L1) -->
  'm*n_s_c'(_, _, C, Dcg, L1).

'*_s_c'(C, Dcg, L1, L2) -->
  'm*n_s_c'(_, _, C, Dcg, L1, L2).

'*_s_c'(C, Dcg, L1, L2, L3) -->
  'm*n_s_c'(_, _, C, Dcg, L1, L2, L3).

'*_s_c'(C, Dcg, L1, L2, L3, L4) -->
  'm*n_s_c'(_, _, C, Dcg, L1, L2, L3, L4).

'*_s_c'(C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s_c'(_, _, C, Dcg, L1, L2, L3, L4, L5).


%! '*n'(+N:nonneg, :Dcg)// .
%! '*n'(+N:nonneg, :Dcg, ?Args1:list, ...)// .
% @see Wrappers around 'm*n'//[3-8], using the default value for `M`.

'*n'(N, Dcg) -->
  'm*n'(_, N, Dcg).

'*n'(N, Dcg, L1) -->
  'm*n'(_, N, Dcg, L1).

'*n'(N, Dcg, L1, L2) -->
  'm*n'(_, N, Dcg, L1, L2).

'*n'(N, Dcg, L1, L2, L3) -->
  'm*n'(_, N, Dcg, L1, L2, L3).

'*n'(N, Dcg, L1, L2, L3, L4) -->
  'm*n'(_, N, Dcg, L1, L2, L3, L4).

'*n'(N, Dcg, L1, L2, L3, L4, L5) -->
  'm*n'(_, N, Dcg, L1, L2, L3, L4, L5).


%! '*n_c'(+N:nonneg, -Count:nonneg, :Dcg)// .
%! '*n_c'(+N:nonneg, -Count:nonneg, :Dcg, ?Args1:list, ...)// .
% @see Wrappers around 'm*n'//[3-8], using the default value for `M`.

'*n_c'(N, C, Dcg) -->
  'm*n_c'(_, N, C, Dcg).

'*n_c'(N, C, Dcg, L1) -->
  'm*n_c'(_, N, C, Dcg, L1).

'*n_c'(N, C, Dcg, L1, L2) -->
  'm*n_c'(_, N, C, Dcg, L1, L2).

'*n_c'(N, C, Dcg, L1, L2, L3) -->
  'm*n_c'(_, N, C, Dcg, L1, L2, L3).

'*n_c'(N, C, Dcg, L1, L2, L3, L4) -->
  'm*n_c'(_, N, C, Dcg, L1, L2, L3, L4).

'*n_c'(N, C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_c'(_, N, C, Dcg, L1, L2, L3, L4, L5).


%! '*n_s'(+N:nonneg, :Dcg)// .
%! '*n_s'(+N:nonneg, :Dcg, ?Args1:list, ...)// .
% @see Wrappers around 'm*n'//[3-8], using the default value for `M`.

'*n_s'(N, Dcg) -->
  'm*n_s'(_, N, Dcg).

'*n_s'(N, Dcg, L1) -->
  'm*n_s'(_, N, Dcg, L1).

'*n_s'(N, Dcg, L1, L2) -->
  'm*n_s'(_, N, Dcg, L1, L2).

'*n_s'(N, Dcg, L1, L2, L3) -->
  'm*n_s'(_, N, Dcg, L1, L2, L3).

'*n_s'(N, Dcg, L1, L2, L3, L4) -->
  'm*n_s'(_, N, Dcg, L1, L2, L3, L4).

'*n_s'(N, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s'(_, N, Dcg, L1, L2, L3, L4, L5).


%! '*n_s_c'(+N:nonneg, -Count:nonneg, :Dcg)// .
%! '*n_s_c'(+N:nonneg, -Count:nonneg, :Dcg, ?Args1:list, ...)// .
% @see Wrappers around 'm*n'//[3-8], using the default value for `M`.

'*n_s_c'(N, C, Dcg) -->
  'm*n_s_c'(_, N, C, Dcg).

'*n_s_c'(N, C, Dcg, L1) -->
  'm*n_s_c'(_, N, C, Dcg, L1).

'*n_s_c'(N, C, Dcg, L1, L2) -->
  'm*n_s_c'(_, N, C, Dcg, L1, L2).

'*n_s_c'(N, C, Dcg, L1, L2, L3) -->
  'm*n_s_c'(_, N, C, Dcg, L1, L2, L3).

'*n_s_c'(N, C, Dcg, L1, L2, L3, L4) -->
  'm*n_s_c'(_, N, C, Dcg, L1, L2, L3, L4).

'*n_s_c'(N, C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s_c'(_, N, C, Dcg, L1, L2, L3, L4, L5).


%! '+'(:Dcg)// .
%! '+'(:Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `+` in a nondeterministic way.
%
% @see Implementation-wise these are wrappers around 'm*n'//[3-8].

'+'(Dcg) -->
  'm*n'(1, _, Dcg).

'+'(Dcg, L1) -->
  'm*n'(1, _, Dcg, L1).

'+'(Dcg, L1, L2) -->
  'm*n'(1, _, Dcg, L1, L2).

'+'(Dcg, L1, L2, L3) -->
  'm*n'(1, _, Dcg, L1, L2, L3).

'+'(Dcg, L1, L2, L3, L4) -->
  'm*n'(1, _, Dcg, L1, L2, L3, L4).

'+'(Dcg, L1, L2, L3, L4, L5) -->
  'm*n'(1, _, Dcg, L1, L2, L3, L4, L5).


%! '+_c'(-Count:nonneg, :Dcg)// .
%! '+_c'(-Count:nonneg, :Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `+` in a nondeterministic way.
%
% @see Implementation-wise these are wrappers around 'm*n'//[3-8].

'+_c'(C, Dcg) -->
  'm*n_c'(1, _, C, Dcg).

'+_c'(C, Dcg, L1) -->
  'm*n_c'(1, _, C, Dcg, L1).

'+_c'(C, Dcg, L1, L2) -->
  'm*n_c'(1, _, C, Dcg, L1, L2).

'+_c'(C, Dcg, L1, L2, L3) -->
  'm*n_c'(1, _, C, Dcg, L1, L2, L3).

'+_c'(C, Dcg, L1, L2, L3, L4) -->
  'm*n_c'(1, _, C, Dcg, L1, L2, L3, L4).

'+_c'(C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_c'(1, _, C, Dcg, L1, L2, L3, L4, L5).


%! '+_s'(:Dcg)// .
%! '+_s'(:Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `+` in a nondeterministic way.
%
% @see Implementation-wise these are wrappers around 'm*n'//[3-8].

'+_s'(Dcg) -->
  'm*n_s'(1, _, Dcg).

'+_s'(Dcg, L1) -->
  'm*n_s'(1, _, Dcg, L1).

'+_s'(Dcg, L1, L2) -->
  'm*n_s'(1, _, Dcg, L1, L2).

'+_s'(Dcg, L1, L2, L3) -->
  'm*n_s'(1, _, Dcg, L1, L2, L3).

'+_s'(Dcg, L1, L2, L3, L4) -->
  'm*n_s'(1, _, Dcg, L1, L2, L3, L4).

'+_s'(Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s'(1, _, Dcg, L1, L2, L3, L4, L5).


%! '+_s'(-Count:nonneg, :Dcg)// .
%! '+_s'(-Count:nonneg, :Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `+` in a nondeterministic way.
%
% @see Implementation-wise these are wrappers around 'm*n'//[3-8].

'+_s_c'(C, Dcg) -->
  'm*n_s_c'(1, _, C, Dcg).

'+_s_c'(C, Dcg, L1) -->
  'm*n_s_c'(1, _, C, Dcg, L1).

'+_s_c'(C, Dcg, L1, L2) -->
  'm*n_s_c'(1, _, C, Dcg, L1, L2).

'+_s_c'(C, Dcg, L1, L2, L3) -->
  'm*n_s_c'(1, _, C, Dcg, L1, L2, L3).

'+_s_c'(C, Dcg, L1, L2, L3, L4) -->
  'm*n_s_c'(1, _, C, Dcg, L1, L2, L3, L4).

'+_s_c'(C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s_c'(1, _, C, Dcg, L1, L2, L3, L4, L5).


%! '?'(:Dcg)// .
%! '?'(:Dcg, ?Args1:list, ...)// .
% Implements the Regular Expression operator `?`,
% generating *both* the case of 0 occurrences *and* the case of 1 occurrence.

'?'(Dcg) -->
  'm*n'(0, 1, Dcg).

'?'(Dcg, A1) -->
  'm*n'(0, 1, Dcg, L1),
  {maplist('?_arg', [L1], [A1])}.

'?'(Dcg, A1, A2) -->
  'm*n'(0, 1, Dcg, L1, L2),
  {maplist('?_arg', [L1,L2], [A1,A2])}.

'?'(Dcg, A1, A2, A3) -->
  'm*n'(0, 1, Dcg, L1, L2, L3),
  {maplist('?_arg', [L1,L2,L3], [A1,A2,A3])}.

'?'(Dcg, A1, A2, A3, A4) -->
  'm*n'(0, 1, Dcg, L1, L2, L3, L4),
  {maplist('?_arg', [L1,L2,L3,L4], [A1,A2,A3,A4])}.

'?'(Dcg, A1, A2, A3, A4, A5) -->
  'm*n'(0, 1, Dcg, L1, L2, L3, L4, L5),
  {maplist('?_arg', [L1,L2,L3,L4,L5], [A1,A2,A3,A4,A5])}.

'?_arg'([], _).
'?_arg'([A], A).


%! '?_c'(-Count:nonneg, :Dcg)// .
%! '?_c'(-Count:nonneg, :Dcg, ?Args1:list, ...)// .

'?_c'(C, Dcg) -->
  'm*n_c'(0, 1, C, Dcg).

'?_c'(C, Dcg, A1) -->
  'm*n_c'(0, 1, C, Dcg, L1),
  {maplist('?_arg', [L1], [A1])}.

'?_c'(C, Dcg, A1, A2) -->
  'm*n_c'(0, 1, C, Dcg, L1, L2),
  {maplist('?_arg', [L1,L2], [A1,A2])}.

'?_c'(C, Dcg, A1, A2, A3) -->
  'm*n_c'(0, 1, C, Dcg, L1, L2, L3),
  {maplist('?_arg', [L1,L2,L3], [A1,A2,A3])}.

'?_c'(C, Dcg, A1, A2, A3, A4) -->
  'm*n_c'(0, 1, C, Dcg, L1, L2, L3, L4),
  {maplist('?_arg', [L1,L2,L3,L4], [A1,A2,A3,A4])}.

'?_c'(C, Dcg, A1, A2, A3, A4, A5) -->
  'm*n_c'(0, 1, C, Dcg, L1, L2, L3, L4, L5),
  {maplist('?_arg', [L1,L2,L3,L4,L5], [A1,A2,A3,A4,A5])}.


%! 'm*'(?M:nonneg, :Dcg)// .
%! 'm*'(?M:nonneg, :Dcg, ?Args1:list, ...)// .

'm*'(M, Dcg) -->
  'm*n'(M, _, Dcg).

'm*'(M, Dcg, L1) -->
  'm*n'(M, _, Dcg, L1).

'm*'(M, Dcg, L1, L2) -->
  'm*n'(M, _, Dcg, L1, L2).

'm*'(M, Dcg, L1, L2, L3) -->
  'm*n'(M, _, Dcg, L1, L2, L3).

'm*'(M, Dcg, L1, L2, L3, L4) -->
  'm*n'(M, _, Dcg, L1, L2, L3, L4).

'm*'(M, Dcg, L1, L2, L3, L4, L5) -->
  'm*n'(M, _, Dcg, L1, L2, L3, L4, L5).


%! 'm*_c'(?M:nonneg, -Count:nonneg, :Dcg)// .
%! 'm*_c'(?M:nonneg, -Count:nonneg, :Dcg, ?Args1:list, ...)// .

'm*_c'(M, C, Dcg) -->
  'm*n_c'(M, _, C, Dcg).

'm*_c'(M, C, Dcg, L1) -->
  'm*n_c'(M, _, C, Dcg, L1).

'm*_c'(M, C, Dcg, L1, L2) -->
  'm*n_c'(M, _, C, Dcg, L1, L2).

'm*_c'(M, C, Dcg, L1, L2, L3) -->
  'm*n_c'(M, _, C, Dcg, L1, L2, L3).

'm*_c'(M, C, Dcg, L1, L2, L3, L4) -->
  'm*n_c'(M, _, C, Dcg, L1, L2, L3, L4).

'm*_c'(M, C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_c'(M, _, C, Dcg, L1, L2, L3, L4, L5).


%! 'm*_s'(?M:nonneg, :Dcg)// .
%! 'm*_s'(?M:nonneg, :Dcg, ?Args1:list, ...)// .

'm*_s'(M, Dcg) -->
  'm*n_s'(M, _, Dcg).

'm*_s'(M, Dcg, L1) -->
  'm*n_s'(M, _, Dcg, L1).

'm*_s'(M, Dcg, L1, L2) -->
  'm*n_s'(M, _, Dcg, L1, L2).

'm*_s'(M, Dcg, L1, L2, L3) -->
  'm*n_s'(M, _, Dcg, L1, L2, L3).

'm*_s'(M, Dcg, L1, L2, L3, L4) -->
  'm*n_s'(M, _, Dcg, L1, L2, L3, L4).

'm*_s'(M, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s'(M, _, Dcg, L1, L2, L3, L4, L5).


%! 'm*_s_c'(?M:nonneg, -Count:nonneg, :Dcg)// .
%! 'm*_s_c'(?M:nonneg, -Count:nonneg, :Dcg, ?Args1:list, ...)// .

'm*_s_c'(M, C, Dcg) -->
  'm*n_s_c'(M, _, C, Dcg).

'm*_s_c'(M, C, Dcg, L1) -->
  'm*n_s_c'(M, _, C, Dcg, L1).

'm*_s_c'(M, C, Dcg, L1, L2) -->
  'm*n_s_c'(M, _, C, Dcg, L1, L2).

'm*_s_c'(M, C, Dcg, L1, L2, L3) -->
  'm*n_s_c'(M, _, C, Dcg, L1, L2, L3).

'm*_s_c'(M, C, Dcg, L1, L2, L3, L4) -->
  'm*n_s_c'(M, _, C, Dcg, L1, L2, L3, L4).

'm*_s_c'(M, C, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s_c'(M, _, C, Dcg, L1, L2, L3, L4, L5).


%! 'm*n'(?M:nonneg, ?N:nonneg,:Dcg)// .
%! 'm*n'(?M:nonneg, ?N:nonneg, :Dcg, ?Args1:list, ...)// .

'm*n'(M, N, Dcg) -->
  'm*n_c'(M, N, _, Dcg).

'm*n'(M, N, Dcg, L1) -->
  'm*n_c'(M, N, _, Dcg, L1).

'm*n'(M, N, Dcg, L1, L2) -->
  'm*n_c'(M, N, _, Dcg, L1, L2).

'm*n'(M, N, Dcg, L1, L2, L3) -->
  'm*n_c'(M, N, _, Dcg, L1, L2, L3).

'm*n'(M, N, Dcg, L1, L2, L3, L4) -->
  'm*n_c'(M, N, _, Dcg, L1, L2, L3, L4).

'm*n'(M, N, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_c'(M, N, _, Dcg, L1, L2, L3, L4, L5).


%! 'm*n_c'(?M:nonneg, ?N:nonneg, -Count:nonneg,:Dcg)// .
%! 'm*n_c'(?M:nonneg, ?N:nonneg, -Count:nonneg, :Dcg, ?Args1:list, ...)// .
% This predicate lies at the bases of all the other public predicates
% in this module.
%
% The uninstantiated variables in `Dcg` are *not* shared between calls
% (implemented by calling a copy of `Dcg`, using copy_term/2).
%
% Typechecking is performed on `M` and `N`,
% throwing a `type_error` if they are not integers,
% throwing a `domain_error` if they are negative integers,
% and failing silently when `N < M`.
%
% @compat Semi-compatible with the specification of
%         Augmented Backus-Naur Form in RFC 2616 (HTTP 1.1).
%
% @throws type_error when `M` or N` is not an integer.
% @throws domain_error when `M` or `N` is a negative integer.

'm*n_c'(M, N, C, Dcg, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_generate'(M, N, 0, C, Dcg, X, Y)
  ;  'm*n_parse'(M, N, 0, C, Dcg, X, Y)
  ).

'm*n_c'(M, N, C, Dcg, L1, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_generate'(M, N, 0, C, Dcg, L1, X, Y)
  ;  'm*n_parse'(M, N, 0, C, Dcg, L1, X, Y)
  ).

'm*n_c'(M, N, C, Dcg, L1, L2, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_generate'(M, N, 0, C, Dcg, L1, L2, X, Y)
  ;  'm*n_parse'(M, N, 0, C, Dcg, L1, L2, X, Y)
  ).

'm*n_c'(M, N, C, Dcg, L1, L2, L3, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_generate'(M, N, 0, C, Dcg, L1, L2, L3, X, Y)
  ;  'm*n_parse'(M, N, 0, C, Dcg, L1, L2, L3, X, Y)
  ).

'm*n_c'(M, N, C, Dcg, L1, L2, L3, L4, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_generate'(M, N, 0, C, Dcg, L1, L2, L3, L4, X, Y)
  ;  'm*n_parse'(M, N, 0, C, Dcg, L1, L2, L3, L4, X, Y)
  ).

'm*n_c'(M, N, C, Dcg, L1, L2, L3, L4, L5, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_generate'(M, N, 0, C, Dcg, L1, L2, L3, L4, L5, X, Y)
  ;  'm*n_parse'(M, N, 0, C, Dcg, L1, L2, L3, L4, L5, X, Y)
  ).


%! 'm*n_s'(?M:nonneg, ?N:nonneg, :Dcg)// .
%! 'm*n_s'(?M:nonneg, ?N:nonneg, :Dcg, ?Args1:list, ...)// .

'm*n_s'(M, N, Dcg) -->
  'm*n_s_c'(M, N, _, Dcg).

'm*n_s'(M, N, Dcg, L1) -->
  'm*n_s_c'(M, N, _, Dcg, L1).

'm*n_s'(M, N, Dcg, L1, L2) -->
  'm*n_s_c'(M, N, _, Dcg, L1, L2).

'm*n_s'(M, N, Dcg, L1, L2, L3) -->
  'm*n_s_c'(M, N, _, Dcg, L1, L2, L3).

'm*n_s'(M, N, Dcg, L1, L2, L3, L4) -->
  'm*n_s_c'(M, N, _, Dcg, L1, L2, L3, L4).

'm*n_s'(M, N, Dcg, L1, L2, L3, L4, L5) -->
  'm*n_s_c'(M, N, _, Dcg, L1, L2, L3, L4, L5).


%! 'm*n_s_c'(?M:nonneg, ?N:nonneg, -Count:nonneg, :Dcg)// .
%! 'm*n_s_c'(?M:nonneg, ?N:nonneg, -Count:nonneg, :Dcg, ?Args1:list, ...)// .

'm*n_s_c'(M, N, C, Dcg, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_s_generate'(M, N, 0, C, Dcg, X, Y)
  ;  'm*n_s_parse'(M, N, 0, C, Dcg, X, Y)
  ).

'm*n_s_c'(M, N, C, Dcg, L1, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_s_generate'(M, N, 0, C, Dcg, L1, X, Y)
  ;  'm*n_s_parse'(M, N, 0, C, Dcg, L1, X, Y)
  ).

'm*n_s_c'(M, N, C, Dcg, L1, L2, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_s_generate'(M, N, 0, C, Dcg, L1, L2, X, Y)
  ;  'm*n_s_parse'(M, N, 0, C, Dcg, L1, L2, X, Y)
  ).

'm*n_s_c'(M, N, C, Dcg, L1, L2, L3, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_s_generate'(M, N, 0, C, Dcg, L1, L2, L3, X, Y)
  ;  'm*n_s_parse'(M, N, 0, C, Dcg, L1, L2, L3, X, Y)
  ).

'm*n_s_c'(M, N, C, Dcg, L1, L2, L3, L4, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_s_generate'(M, N, 0, C, Dcg, L1, L2, L3, L4, X, Y)
  ;  'm*n_s_parse'(M, N, 0, C, Dcg, L1, L2, L3, L4, X, Y)
  ).

'm*n_s_c'(M, N, C, Dcg, L1, L2, L3, L4, L5, X, Y):-
  'm*n_typecheck'(M, N),
  (  var(X)
  -> 'm*n_s_generate'(M, N, 0, C, Dcg, L1, L2, L3, L4, L5, X, Y)
  ;  'm*n_s_parse'(M, N, 0, C, Dcg, L1, L2, L3, L4, L5, X, Y)
  ).


%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg,
%!     :Dcg)// .
%! 'm*n_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg,
%!     :Dcg, -Args1:list, ...)// .
% Since generating is meager, we try to stop generating
% instances of `Dcg` as soon as possible.

'm*n_generate'(M, _, C, C, _) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_generate'(M, N, C1, C, Dcg) -->
  {'m*n_higher'(N, C1)},
  Dcg,
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg).

'm*n_generate'(M, _, C, C, _, []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_generate'(M, N, C1, C, Dcg, [H1|T1]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, T1).

'm*n_generate'(M, _, C, C, _, [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, T1, T2).

'm*n_generate'(M, _, C, C, _, [], [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2, H3),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, T1, T2, T3).

'm*n_generate'(M, _, C, C, _, [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2, H3, H4),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, T1, T2, T3, T4).

'm*n_generate'(M, _, C, C, _, [], [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2, H3, H4, H5),
  {succ(C1, C2)},
  'm*n_generate'(M, N, C2, C, Dcg, T1, T2, T3, T4, T5).


%! 'm*n_s_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg,
%!     :Dcg)// .
%! 'm*n_s_generate'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg,
%!     :Dcg, -Args1:list, ...)// .

'm*n_s_generate'(M, _, C, C, _) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_s_generate'(M, N, C, C1, Dcg) -->
  {'m*n_higher'(N, C1)},
  Dcg,
  {succ(C1, C2)},
  'm*n_s_generate'(M, N, C, C2, Dcg).

'm*n_s_generate'(M, _, C, C, _, []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_s_generate'(M, N, C, C1, Dcg, [H1|T1]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1),
  {succ(C1, C2)},
  'm*n_s_generate'(M, N, C, C2, Dcg, T1).

'm*n_s_generate'(M, _, C, C, _, [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_s_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2),
  {succ(C1, C2)},
  'm*n_s_generate'(M, N, C2, C, Dcg, T1, T2).

'm*n_s_generate'(M, _, C, C, _, [], [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_s_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2, H3),
  {succ(C1, C2)},
  'm*n_s_generate'(M, N, C2, C, Dcg, T1, T2, T3).

'm*n_s_generate'(M, _, C, C, _, [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_s_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2, H3, H4),
  {succ(C1, C2)},
  'm*n_s_generate'(M, N, C2, C, Dcg, T1, T2, T3, T4).

'm*n_s_generate'(M, _, C, C, _, [], [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].
'm*n_s_generate'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2, H3, H4, H5),
  {succ(C1, C2)},
  'm*n_s_generate'(M, N, C2, C, Dcg, T1, T2, T3, T4, T5).


%! 'm*n_higher'(+N:nonneg, +Counter:nonneg) is semidet.
% Succeeds whenever the higher bound in 'm*n'//[3-8] is respected.

'm*n_higher'(N, C):-
  nonvar(N),
  N =< C, !,
  fail.
'm*n_higher'(_, _).


%! 'm*n_lower'(+M:nonneg, +Counter:nonneg) is semidet.
% Succeeds whenever the lower bound in 'm*n'//[3-8] is respected.

'm*n_lower'(M, C):-
  default(0, M),
  M > C, !,
  fail.
'm*n_lower'(_, _).


%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg)// .
%! 'm*n_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg, :Dcg,
%!     -Args1:list, ...)// .
% Since parsing is eager, we try to process
% as many instances of `Dcg` as possible.

'm*n_parse'(M, N, C1, C, Dcg) -->
  {'m*n_higher'(N, C1)},
  Dcg,
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg).
'm*n_parse'(M, _, C, C, _) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_parse'(M, N, C1, C, Dcg, [H1|T1]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, T1).
'm*n_parse'(M, _, C, C, _, []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, T1, T2).
'm*n_parse'(M, _, C, C, _, [], []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2, H3),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, T1, T2, T3).
'm*n_parse'(M, _, C, C, _, [], [], []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2, H3, H4),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, T1, T2, T3, T4).
'm*n_parse'(M, _, C, C, _, [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) -->
  {'m*n_higher'(N, C1)},
  dcg_call(Dcg, H1, H2, H3, H4, H5),
  {succ(C1, C2)},
  'm*n_parse'(M, N, C2, C, Dcg, T1, T2, T3, T4, T5).
'm*n_parse'(M, _, C, C, _, [], [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].


%! 'm*n_s_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg,
%!     :Dcg)// .
%! 'm*n_s_parse'(?M:nonneg, ?N:nonneg, +Counter:nonneg, -Count:nonneg,
%!     :Dcg, -Args1:list, ...)// .

'm*n_s_parse'(M, N, C1, C, Dcg) -->
  % We may not surpass the upper bound.
  {'m*n_higher'(N, C1)},
  Dcg,
  {succ(C1, C2)},
  'm*n_s_parse'(M, N, C2, C, Dcg).
'm*n_s_parse'(M, _, C, C, _) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_s_parse'(M, N, C1, C, Dcg, [H1|T1]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1),
  {succ(C1, C2)},
  'm*n_s_parse'(M, N, C2, C, Dcg, T1).
'm*n_s_parse'(M, _, C, C, _, []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_s_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2),
  {succ(C1, C2)},
  'm*n_s_parse'(M, N, C2, C, Dcg, T1, T2).
'm*n_s_parse'(M, _, C, C, _, [], []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_s_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2, H3),
  {succ(C1, C2)},
  'm*n_s_parse'(M, N, C2, C, Dcg, T1, T2, T3).
'm*n_s_parse'(M, _, C, C, _, [], [], []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_s_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2, H3, H4),
  {succ(C1, C2)},
  'm*n_s_parse'(M, N, C2, C, Dcg, T1, T2, T3, T4).
'm*n_s_parse'(M, _, C, C, _, [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].

'm*n_s_parse'(M, N, C1, C, Dcg, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) -->
  {'m*n_higher'(N, C1)},
  dcg_call_s(Dcg, H1, H2, H3, H4, H5),
  {succ(C1, C2)},
  'm*n_s_parse'(M, N, C2, C, Dcg, T1, T2, T3, T4, T5).
'm*n_s_parse'(M, _, C, C, _, [], [], [], [], []) -->
  {'m*n_lower'(M, C)},
  [].


% Type error for M.
'm*n_typecheck'(M, _):-
  nonvar(M),
  \+ integer(M), !,
  type_error(integer, M).
% Domain error for M.
'm*n_typecheck'(M, _):-
  nonvar(M),
  M < 0, !,
  domain_error(nonneg, M).
% Type error for N.
'm*n_typecheck'(_, N):-
  nonvar(N),
  \+ integer(N), !,
  type_error(integer, N).
% Domain error for N.
'm*n_typecheck'(_, N):-
  nonvar(N),
  N < 0, !,
  domain_error(nonvar, N).
% N below M: fail silently.
'm*n_typecheck'(M, N):-
  nonvar(M),
  nonvar(N),
  N < M, !,
  fail.
% Everything else succeeds.
'm*n_typecheck'(_, _).

