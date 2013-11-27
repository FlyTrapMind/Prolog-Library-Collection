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

:- use_module(ap(ap_stat)).
:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).



ap_copy_file(_StageAlias, FromFile, ToFile):-
  safe_copy_file(FromFile, ToFile).

ap_extract_archive(_StageAlias, FromFile, ToFile):-
  file_to_directory(ToFile, ToDir),
  archive_extract(FromFile, ToDir, []).

ap_merge_into_one_file(StageAlias, FromDir, ToFile):-
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
  
  % Stats
  length(FromFiles, NumberOfFromFiles),
  ap_stage_init(StageAlias, NumberOfFromFiles),
  
  setup_call_cleanup(
    open(ToFile, write, Out, [type(binary)]),
    maplist(ap_merge_into_one_file_(StageAlias, Out), FromFiles),
    close(Out)
  ).
ap_merge_into_one_file_(StageAlias, Out, FromFile):-
  setup_call_cleanup(
    open(FromFile, read, In, [type(binary)]),
    copy_stream_data(In, Out),
    close(In)
  ),
  
  % Stats
  ap_stage_tick(StageAlias).

