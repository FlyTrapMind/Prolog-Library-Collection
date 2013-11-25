:- module(
  ap_act,
  [
    ap_copy_file/3, % +PS
                    % +FromFile:atom
                    % +ToFile:atom
    ap_extract_archive/3, % +PS
                          % +FromFile:atom
                          % +ToFile:atom
    ap_merge_into_one_file/3 % +Id:pair(atom,nonneg)
                             % +FromDir:atom
                             % +ToFile:atom
  ]
).

/** <module> Auto-process actions

Actions that can be used from within an automated process.

@author Wouter Beek
@version 2013/10-2013/11
*/

:- use_module(ap(ap)).
:- use_module(ap(ap_stat)).
:- use_module(generics(db_ext)).
:- use_module(library(apply)).
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

ap_merge_into_one_file(PS, FromDir, ToFile):-
  directory_files(
    [
      file_types([text]),
      include_directories(false),
      % It may for instance be reasonable to assume that
      % files with names ending in numbers are merged in the order
      % that is indicated by those numbers.
      order(lexicographic),
      recursive(true)
    ],
    FromDir,
    FromFiles
  ),
  setup_call_cleanup(
    open(ToFile, write, Out, [type(binary)]),
    maplist(ap_merge_into_one_file_(PS, Out), FromFiles),
    close(Out)
  ).
ap_merge_into_one_file_(PS, Out, FromFile):-
  setup_call_cleanup(
    open(FromFile, read, In, [type(binary)]),
    copy_stream_data(In, Out),
    close(In)
  ),
  ap_stage_tick(PS).

