:- module(
  gv_hash,
  [
    clear_indexed_sha_hash/0,
    indexed_sha_hash/2, % +Input:or(atom,list(atom))
                        % -Hash:atom
    sha_hash_atom/2 % +Input:or(atom,list(atom))
                    % -Hash:atom
  ]
).

/** <module> GV_HASH

Hashing identifiers of GV objects.
Used for interactive callback functions.

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(library(sha)).

:- dynamic(indexed_sha_hash_(_Input, _Hash)).



clear_indexed_sha_hash:-
  retractall(indexed_sha_hash_(_Key, _Hash)).

indexed_sha_hash(Input, Hash):-
  indexed_sha_hash_(Input, Hash), !.
indexed_sha_hash(Input, Hash):-
  nonvar(Input), !,
  sha_hash_atom(Input, Hash),
  assert(indexed_sha_hash_(Input, Hash)).

sha_hash_atom(Atom, Hash):-
  atomic(Atom), !,
  sha_hash(Atom, HashCodes, []),
  hash_atom(HashCodes, Hash).
sha_hash_atom(List, Hash):-
  is_list(List), !,
  atomic_list_concat(List, Atom),
  sha_hash_atom(Atom, Hash).

