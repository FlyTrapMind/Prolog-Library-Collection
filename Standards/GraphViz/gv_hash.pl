:- module(
  gv_hash,
  [
    clear_indexed_sha_hash/0,
    indexed_sha_hash/2, % +Input:oneof([atom,list])
                        % -Hash:atom
    sha_hash_atom/2 % +Input:oneof([atom,list])
                    % -Hash:atom
  ]
).

/** <module> GV_HASH

@tbd What is this?

@author Wouter Beek
@version 2011-2013/07
*/

:- use_module(library(sha)).

:- dynamic(indexed_sha_hash0(_Input, _Hash)).



clear_indexed_sha_hash:-
  retractall(indexed_sha_hash0(_Key, _Hash)).

indexed_sha_hash(Input, Hash):-
  indexed_sha_hash0(Input, Hash), !.
indexed_sha_hash(Input, Hash):-
  sha_hash_atom(Input, Hash),
  assert(indexed_sha_hash0(Input, Hash)).

sha_hash_atom(Atom, Hash):-
  atom(Atom), !,
  sha_hash(Atom, HashCodes, []),
  hash_atom(HashCodes, Hash).
sha_hash_atom(List, Hash):-
  is_list(List), !,
  atomic_list_concat(List, Atom),
  sha_hash_atom(Atom, Hash).

