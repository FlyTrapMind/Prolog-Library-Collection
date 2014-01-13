:- module(typecheck, []).
:- reexport(library(error), [
  is_of_type/2, % +Type
		% @Term
  must_be/2 % +Type
            % @Term
]).

/** <module> Type checking

Predicates used for parsing and checking value-type conformance.

| *Type*               | *|Defined here|* |
| atom                 |                  |
| atomic               |                  |
| between/2            |                  |
| boolean              |                  |
| callable             |                  |
| char                 |                  |
| chars                |                  |
| code                 |                  |
| codes                |                  |
| compound             |                  |
| constant             |                  |
| encoding             |                  |
| float                |                  |
| ground               |                  |
| integer              |                  |
| list                 |                  |
| list/1               | Yes              |
| list_or_partial_list |                  |
| negative_integer     |                  |
| nonneg               |                  |
| nonvar               |                  |
| number               |                  |
| oneof/1              |                  |
| or/1                 | Yes              |
| positive_integer     |                  |
| rational             |                  |
| string               |                  |
| symbol               |                  |
| text                 |                  |
| iri                  | Yes              |
| var                  |                  |

--

@author Wouter Beek
@version 2013/01, 2013/08, 2014/01
*/

:- use_module(dcg(dcg_generic)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(uri(rfc3987_dcg)).



% char/0
error:has_type(char, Term):-
  once(char_type(Term, _)).
% code/0
error:has_type(code, Term):-
  once(code_type(Term, _)).
% or/1
error:has_type(or(Types), Term):-
  member(Type, Types),
  error:has_type(Type, Term), !.
% list/1
error:has_type(list(Type), Term):-
  must_be(list, Term),
  maplist(must_be(Type), Term).
% iri/0
error:has_type(iri, Term):-
  uri_components(
    Term,
    uri_component(Scheme,Authority,Path,_Search,_Fragment)
  ),
  maplist(nonvar, [Scheme,Authority,Path]).
  % @tbd
  %%%%once(dcg_phrase('IRI'(_), Term)),

