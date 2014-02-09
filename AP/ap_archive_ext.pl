:- module(
  ap_archive_ext,
  [
    extract_archives/3 % +FromDirectory:atom
                       % +ToDirectory:atom
                       % +AP_Stage:iri
  ]
).

/** <module> AP: extract archive

Archive extraction process for the AP architecture.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(ap(ap_db)).
:- use_module(generics(archive_ext)).
:- use_module(os(dir_ext)).
:- use_module(library(apply)).
:- use_module(library(lists)).



%! extact_archives(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   +AP_Stage:iri
%! ) is det.

extract_archives(FromDir, ToDir, AP_Stage):-
  Operation = 'archive extraction',
  directory_files([recursive(false)], FromDir, FromFiles),
  forall(
    member(FromFile, FromFiles),
    (
      extract_archive(FromFile, ToDir, Conversions),
      add_operation_on_file(AP_Stage, FromFile, Operation, Conversions)
    )
  ).

