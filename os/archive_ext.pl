:- module(
  archive_ext,
  [
    extract_file/1, % +File:atom
    extract_file/2, % +File:atom
                    % +Directory:atom
    info_file/1, % +File:atom
    is_archive_file/1 % +File:atom
  ]
).

/** <module> Archive extraction

Based on library `archive`.

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(archive)).
:- use_module(library(error)).
:- use_module(library(filesex)).

:- use_module(os(file_ext)).
:- use_module(os(io_ext)).

:- meta_predicate(archive_file(:,+,+)).



%! archive_file(:Goal, +File:atom, +Arguments:list) is det.

archive_file(Goal, File, Args):-
  setup_call_cleanup(
    archive_open(
      File,
      Archive,
      [close_parent(true),filter(all),format(all),format(raw)]
    ),
    apply(Goal, [File|Args]),
    archive_close(Archive)
  ),
  delete_file(File).


%! extract_archive(+Archive:archive, +Directory:atom) is det.

extract_archive(Archive, Dir):-
  repeat,
  (
    archive_next_header(Archive, RelativeFile)
  ->
    archive_open_entry(Archive, Stream),
    relative_file_path(File, Dir, RelativeFile),
    create_file_directory(File),
    file_from_stream(File, Stream),
    print_message(informational, archive_extracted(File)),
    fail
  ;
    !
  ).


%! extract_file(+File:atom) is semidet.
% Extracts the given file into its own directory.

extract_file(File):-
  file_directory_name(File, Dir),
  archive_file(extract_archive, File, Dir).


%! extract_file(+File:atom, +Directory:atom) is semidet.
% Extracts the given file into the given directory.

extract_file(File, Dir):-
  archive_file(extract_archive, File, [Dir]).


%! info_archive(+Archive:archive) is det.

info_archive(Archive):-
  repeat,
  (
    archive_next_header(Archive, Path)
  ->
    print_message(information, archive_header(Archive,Path)),
    forall(
      archive_header_property(Archive, Property),
      print_message(information, archive_property(Archive,Property))
    ),
    fail
  ;
    !
  ).


%! info_file(+File:atom) is det.
% Writes archive information for the given file to current input.

info_file(File):-
  archive_file(info_archive, File, []).


%! is_archive_file(+File:atom) is semidet.

is_archive_file(File):-
  archive_file(exists_file, File, []).



% MESSAGES

:- multifile(prolog:message//1).

prolog:message(archive_extracted(File)) -->
  ['Extracted ~w'-[File]].

prolog:message(archive_header(Archive,Path)) -->
  ['~w~t~w'-[Archive,Path]].

prolog:message(archive_property(Archive,Property)) -->
  ['~t~w~t~w'-[Archive,Property]].

