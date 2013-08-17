:- module(
  typecheck,
  [
    boolean/1, % @Term
    char/1, % @Term
    chars/1, % @Term
    code/1, % @Term
    codes/1, % @Term
    list/2, % +Type
            % @Term
    list_or_partial_list/1, % @Term
    negative_integer/1,
    nonneg/1, % @Term
    oneof/2, % +Literals:list(atom)
             % @Term
    positive_integer/1, % @Term
    text/1, % @Term
    typecheck/2, % +Type:compound
                 % +Value
    is_uri/1
  ]
).

/** <module> Type checking

Predicates used for parsing and checking value-type conformance.

Type checks defined by SWI-Prolog:

| *Type*    | *Predicate* |
| atom      | atom/1      |
| atomic    | atomic/1    |
| between/2 | between/3   |
| callable  | callable/1  |
| compound  | compound/1  |
| encoding  | encoding/1  |
| float     | float/1     |
| ground    | ground/1    |
| integer   | integer/1   |
| list      | is_list/1   |
| nonvar    | nonvar/1    |
| number    | number/1    |
| rational  | rational/1  |
| string    | string/1    |
| var       | var/1       |

Type checks defined by this module:

| *Type*               | *Predicate*            |
| boolean              | boolean/1              |
| char                 | char/1                 |
| chars                | chars/1                |
| code                 | code/1                 |
| codes                | codes/1                |
| constant             | @tbd                   |
| list/1               | list/2                 |
| list_or_partial_list | list_or_partial_list/1 |
| negative_integer/1   | negative_integer/1     |
| nonneg               | nonneg/1               |
| oneof/1              | oneof/2                |
| positive_integer/1   | positive_integer/1     |
| symbol               | @tbd                   |
| text                 | text/1                 |

@author Wouter Beek
@version 2013/01, 2013/08
*/

:- use_module(library(apply)).

:- meta_predicate(list(1,+)).



boolean(Term):-
  oneof([true,false], Term).

char(Term):-
  atom_length(Term, 1).

chars(Terrm):-
  maplist(char, Terrm).

code(Term):-
  once(code_type(Term, _)).

codes(Terrm):-
  maplist(code, Terrm).

cons(Term, SubTerm1, SubTerm2):-
  functor(Term, ., 2),
  Term =.. [.,SubTerm1,SubTerm2].

%! list(@Term) is semidet.
% A compound term with the binary cons functor `.` (dot)
% that ends with the empty list symbol (`[]`).
%
% Example: =|.(a,.(b,[]))|= for =|[a,b]|=.

list(Term):-
  is_list(Term).

list(Type, Term):-
  maplist(Type, Term).

list_or_partial_list(Term):-
  list(Term), !.
list_or_partial_list(Term):-
  partial_list(Term), !.

negative_integer(Term):-
  integer(Term),
  Term < 0.

nonneg(Term):-
  integer(Term),
  Term >= 0.

oneof(Literals, Term):-
  ground(Term),
  memberchk(Term, Literals).

%! partial_list(@Term) is semidet.
% A compound term with the binary cons functor `.` (dot)
% that does not end in the empty list symbol (`[]`).
%
% Example: =|.(a,b)|=

partial_list(Term):-
  cons(Term, SubTerm1, SubTerm2),
  maplist(partial_list_, [SubTerm1,SubTerm2]).

partial_list_(Term):-
  \+ compound(Term), !.
partial_list_(Term):-
  cons(Term, SubTerm1, SubTerm2),
  maplist(partial_list_, [SubTerm1,SubTerm2]).

positive_integer(Term):-
  integer(Term),
  Term > 0.

text(Term):-
  atom(Term), !.
text(Term):-
  chars(Term), !.
text(Term):-
  codes(Term), !.
text(Term):-
  string(Term), !.

%! typecheck(+Type:compound, +Value) is semidet.
% Succeeds if the given value is of the given type.
%
% @param Type A compound term representing a type.
% @param Value

typecheck(or(Types), Value):-
  member(Type, Types),
  typecheck(Type, Value), !.
% Open numeric interval: open to the right.
typecheck(between(Min, Var), Value):-
  var(Var), !,
  (
    float(Min)
  ->
    float(Value)
  ;
    integer(Min)
  ->
    integer(Value)
  ),
  Min =< Value.
% Open numeric interval: open to the left.
typecheck(between(Var, Max), Value):-
  var(Var), !,
  (
    float(Max)
  ->
    float(Value)
  ;
    integer(Max)
  ->
    integer(Value)
  ),
  Max >= Value.
typecheck(Type, Value):-
  must_be(Type, Value), !.
% DCG defined types.
typecheck(Type, Value):-
  atom_chars(Value, ValueChars),
  Call =.. [Type, ValueChars, []],
  call(Call).

%! is_uri(?Resource:uri) is semidet.

is_uri(Resource):-
  uri_components(
    Resource,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  nonvar(Scheme),
  nonvar(Authority),
  nonvar(Path).
