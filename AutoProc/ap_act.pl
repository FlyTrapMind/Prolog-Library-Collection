:- module(
  ap_act,
  [
    ap_copy_file/3, % +PS
                    % +FromFile:atom
                    % +ToFile:atom
    ap_extract_archive/3 % +PS
                         % +FromFile:atom
                         % +ToFile:atom
  ]
).

/** <module> Auto-process actions

Actions that can be used from within an automated process.

@author Wouter Beek
@version 2013/10-2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(library(archive)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- db_add_novel(user:prolog_file_type('tar.gz', archive)).
:- db_add_novel(user:prolog_file_type(txt,      text   )).



ap_copy_file(_PS, FromFile, ToFile):-
  safe_copy_file(FromFile, ToFile).

ap_extract_archive(_PS, FromFile, ToFile):-
  directory_file_path(ToDir, _, ToFile),
  archive_extract(FromFile, ToDir, []).

