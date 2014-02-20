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
:- use_module(library(lists)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).



%! extact_archives(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   +AP_Stage:iri
%! ) is det.

extract_archives(FromDir, ToDir, AP_Stage):-
  directory_files([recursive(false)], FromDir, FromFiles),
  extract_archives_files(FromFiles, ToDir, AP_Stage).

extract_archives_files([], _ToDir, AP_Stage):- !,
  rdf_assert_individual(AP_Stage, ap:'Skip', ap),
  rdf_assert_datatype(AP_Stage, ap:status, xsd:string, skip, ap).
extract_archives_files(FromFiles, ToDir, AP_Stage):-
  findall(
    FromFile-Conversions,
    (
      member(FromFile, FromFiles),
      extract_archive(FromFile, ToDir, Conversions)
    ),
    Pairs
  ),
  assert_extract_archives_files(Pairs, AP_Stage).

assert_extract_archives_files(Pairs, AP_Stage):-
  pairs_values(Pairs, Conversionss),
  append(Conversionss, Conversions),
  Conversions == [], !,
  rdf_assert_individual(AP_Stage, ap:'Skip', ap).
assert_extract_archives_files(Pairs, AP_Stage):-
  maplist(assert_extract_archives_file(AP_Stage), Pairs).

assert_extract_archives_file(AP_Stage, File-Conversions):-
  add_operation_on_file(AP_Stage, File, 'archive extraction', Conversions).

