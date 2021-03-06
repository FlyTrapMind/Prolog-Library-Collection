:- module(
  typecheck,
  [
    boolean/1,           % @Term
    char/1,              % @Term
    chars/1,             % @Term
    code/1,              % @Term
    codes/1,             % @Term
    is_file_iri/1,       % @Term
    is_http_iri/1,       % @Term
    is_iri/1,            % @Term
    is_uri/1,            % @Term
    must_be_directory/1, % +Dir
    must_be_file/2,      % +Mode:oneof([append,read,write]), +File
    negative_float/1,    % @Term
    negative_integer/1,  % @Term
    nonneg/1,            % @Term
    nonpos/1,            % @Term
    positive_float/1,    % @Term
    positive_integer/1,  % @Term
    text/1               % @Term
  ]
).
:- reexport(
  library(error),
  [
    is_of_type/2, % +Type, @Term
    must_be/2     % +Type, @Term
  ]
).

/** <module> Typecheck

Predicates used for parsing and checking value-type conformance.

| *Type*               | *|Defined here|* | *|Atom-conversion|* |
| atom                 |                  | Yes                 |
| atomic               |                  |                     |
| between/2            |                  | Yes                 |
| boolean              |                  | Yes                 |
| callable             |                  |                     |
| char                 |                  | Yes                 |
| chars                |                  | Yes                 |
| code                 |                  | Yes                 |
| codes                |                  | Yes                 |
| compound             |                  |                     |
| constant             |                  |                     |
| email                |                  |                     |
| encoding             |                  |                     |
| float                |                  | Yes                 |
| ground               |                  |                     |
| integer              |                  | Yes                 |
| list                 |                  |                     |
| list/1               | Yes              |                     |
| list_or_partial_list |                  |                     |
| negative_integer     |                  | Yes                 |
| nonneg               |                  | Yes                 |
| nonpos               | Yes              | Yes                 |
| nonvar               |                  |                     |
| number               |                  | Yes                 |
| oneof/1              |                  | Yes                 |
| or/1                 | Yes              |                     |
| positive_integer     |                  | Yes                 |
| rational             |                  |                     |
| string               |                  | Yes                 |
| symbol               |                  |                     |
| term                 | Yes              |                     |
| text                 |                  |                     |
| iri                  | Yes              |                     |
| var                  |                  |                     |

---

@author Wouter Beek
@version 2015/07, 2015/09-2015/11, 2016/03
*/

:- use_module(library(error)).
:- use_module(library(uri)).

:- multifile
    error:has_type/2.

% Character
error:has_type(char, T) :-
  is_char(T).
% Code
error:has_type(code, T) :-
  once(code_type(T, _)).
% between_float/2, extension of between/2 for floats
% allowing uninstiated upper and lower bounds.
error:has_type(between_float(L,U), X) :-
  number(X),
  (number(L) -> X >= L ; true),
  (number(U) -> X =< L ; true).
% IRI
error:has_type(iri, T) :-
  atom(T),
  uri_is_global(T).
% or/1
error:has_type(or(Types), T) :-
  member(Type, Types),
  error:has_type(Type, T), !.
% term
error:has_type(term, _).



%! boolean(@Term) is semidet.

boolean(T) :-
  error:has_type(boolean, T).



%! char(@Term) is semidet.

char(T) :-
  error:has_type(char, T).



%! chars(@Term) is semidet

chars(T) :-
  error:has_type(chars, T).



%! code(@Term) is semidet.

code(T) :-
  error:has_type(code, T).



%! codes(@Term) is semidet

codes(T) :-
  error:has_type(codes, T).



%! is_file_iri(@Term) is semidet.

is_file_iri(Iri) :-
  atom(Iri),
  uri_file_name(Iri, _).



%! is_http_iri(@Term) is semidet.

is_http_iri(Iri) :-
  is_iri(Iri),
  uri_components(Iri, uri_components(Scheme,_,_,_,_)),
  memberchk(Scheme, [http,https]).



%! is_iri(@Term) is semidet.

is_iri(T) :-
  error:has_type(iri, T).



%! is_uri(@Term) is semidet.

is_uri(T) :-
  is_iri(T).



%! must_be_directory(+Dir) is det.

must_be_directory(Dir) :-
  var(Dir), !,
  instantiation_error(Dir).
must_be_directory(Dir) :-
  \+ exists_directory(Dir), !,
  existence_error(directory, Dir).
must_be_directory(_).



%! must_be_file(+Mode:oneof([append,read,write]), +File) is det.
% @throws existence_error If File does not exist.
% @throws instantiation_error If File is uninstantiated.
% @throws permission_error If File cannot be accessed under Mode.

must_be_file(_, File) :-
  var(File), !,
  instantiation_error(File).
must_be_file(Mode, File) :-
  memberchk(Mode, [access,read]),
  \+ exists_file(File), !,
  existence_error(file, File).
must_be_file(Mode, File) :-
  \+ access_file(File, Mode), !,
  permission_error(Mode, file, File).
must_be_file(_, _).



%! negative_float(@Term) is semidet.
% Fails silently when no negative integer.

negative_float(T) :-
  float(T),
  T > 0.



%! negative_integer(@Term) is semidet.
% Fails silently when no negative integer.

negative_integer(T) :-
  integer(T),
  T < 0.



%! nonneg(@Term) is semidet.
% Fails silently when no positive integer or zero.

nonneg(T) :-
  integer(T),
  T >= 0.



%! nonpos(@Term) is semidet.
% Fails silently when no positive integer or zero.

nonpos(T) :-
  integer(T),
  T =< 0.



%! positive_float(@Term) is semidet.
% Fails silently when no negative integer.

positive_float(T) :-
  float(T),
  T > 0.0.



%! positive_integer(@Term) is semidet.
% Fails silently when no negative integer.

positive_integer(T) :-
  integer(T),
  T > 0.



%! text(@Term) is semidet.
% Text is one of atom, string, chars or codes.

text(T) :-
  error:has_type(text, T).
