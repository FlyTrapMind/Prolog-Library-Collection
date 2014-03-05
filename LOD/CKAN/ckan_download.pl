:- module(ckan_download, []).

/** <module> CKAN Download

Downloads CKAN datasets to the given directory.

@author Wouter Beek
@version 2014/03
*/

:- use_module(ap(ap_rdf_serial)).
:- use_module(ap(ap_table)). % Debug tool.
:- use_module(ap(ap_void_stat)).
:- use_module(ckan(ckan_ap)).
:- use_module(ckan(ckan_table)). % Debug tool.
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).

:- initialization(ckan_download).



ckan_download:-
  current_prolog_flag(argv, [File1]),
  argument_to_absolute_file_name(File1, File2),
  file_to_directory(File2, Dir),
  ckan_download(File2, Dir).
ckan_download:-
  current_prolog_flag(argv, [File1,Directory1]),
  argument_to_absolute_file_name(File1, File2),
  argument_to_absolute_file_name(Directory1, Directory2),
  make_directory_path(Directory2),
  ckan_download(File2, Directory2).

ckan_download(File, Dir):-
  ckan_ap(
    File,
    [
      ckan_download:ap_stage(
        [name('toNTriples'),args(['application/n-triples'])],
        ap_rdf_convert_directory
      ),
      ckan_download:ap_stage([name('VoID')], void_statistics),
      ckan_download:ap_stage([name('Stash'),args([Dir])], stash_output)
    ]
  ).


argument_to_absolute_file_name(File, File):-
  is_absolute_file_name(File), !.
argument_to_absolute_file_name(File1, File2):-
  absolute_file_name(File1, File2, [access(write)]).


stash_output(FromDir, _, _, Dir):-
  directory_files(
    [include_directories(false),include_self(false),recursive(true)],
    FromDir,
    FromFiles
  ),
  forall(
    member(FromFile, FromFiles),
    copy_file(FromFile, Dir)
  ).

