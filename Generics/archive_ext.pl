:- module(
  archive_ext,
  [
    archive_extract/2 % +FromFile:atom
                      % +ToDirectory:atom
  ]
).

/** <module> Archive extensions

Extensions to the support for archived files.

@author Wouter Beek
@version 2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(library(archive)).

:- db_add_novel(user:prolog_file_type('tar.gz', archive)).



%! archive_extract(+FromFile:atom, +ToDir:atom) is det.
% @see Wrapper around archive_extract/3.

archive_extract(FromFile, ToDir):-
  archive_extract(FromFile, ToDir, []).

