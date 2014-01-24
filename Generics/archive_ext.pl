:- module(
  archive_ext,
  [
    extract_archive/3, % +FromFile:atom
                       % +ToDirectory:atom
                       % -Message:atom
    list_archive/1 % +File:atom
  ]
).

/** <module> Archive extensions

Extensions to the support for archived files.

@author Wouter Beek
@version 2013/12-2014/01
*/

:- use_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(process)).

:- db_add_novel(user:prolog_file_type(gz, archive)).
:- db_add_novel(user:prolog_file_type(tar, archive)).
:- db_add_novel(user:prolog_file_type(zip, archive)).

:- debug(archive_ext).



%! extract_archive(+FromFile:atom, +ToDir:atom) is det.

extract_archive(File, Dir, Msg):-
  file_name_extension(Base, Ext, File),
  prolog_file_type(Ext, archive), !,
  extract_archive(Ext, File, Base, Msg1),
  extract_archive(Base, Dir, Msg2),
  atomic_list_concat([Msg1,Msg2], ' and ', Msg).
extract_archive(File, Dir, copied):-
  copy_file(File, Dir).


extract_archive(gz, File, _, gunzipped):- !,
  process_create(path(gunzip), [file(File)], []).
extract_archive(tgz, File, _, untarred):- !,
  process_create(path(tar), [zxvf,file(File)], []).
extract_archive(zip, File, Base, unzipped):- !,
  process_create(path(unzip), [file(File),'-o',file(Base)], []).

list_archive(File):-
  archive_open(File, Archive, []),
  repeat,
  (
    archive_next_header(Archive, Path)
  ->
    format('~w~n', [Path]),
    fail
  ;
    !,
    archive_close(Archive)
  ).

