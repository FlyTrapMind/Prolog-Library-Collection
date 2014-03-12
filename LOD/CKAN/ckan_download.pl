:- module(ckan_download, []).

/** <module> CKAN Download

Downloads CKAN datasets to the given directory.

@author Wouter Beek
@version 2014/03
*/

:- use_module(ap(ap_db)).
:- use_module(ap(ap_table)). % Debug tool.
:- use_module(ckan(ckan_ap)).
:- use_module(ckan(ckan_table)). % Debug tool.
:- use_module(generics(uri_ext)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_datatype)).

:- initialization(ckan_download).



ckan_download:-
  (
    absolute_file_name(
      data(datahub_io),
      File,
      [access(read),file_errors(fail),file_type(turtle)]
    ), !
  ;
    absolute_file_name(
      data(datahub_io),
      File,
      [access(write),file_type(turtle)]
    ),
    download_to_file(
      [],
      'https://dl.dropboxusercontent.com/s/brxpfdwn4n72c2z/datahub_io.ttl?dl=1&token_hash=AAEd9UWXY3SsIBAILVE-yIH7fuRq-_s8RYFgdEAePb0oSQ',
      File
    )
  ),
  absolute_file_name(data(.), DataDir, [access(read),file_type(directory)]),
  directory_file_path(DataDir, 'Output', OutputDir),
  make_directory_path(OutputDir),
  ckan_ap(
    File,
    [ckan_download:ap_stage([name('Stash'),args([OutputDir])], stash_output)]
  ).


stash_output(FromDir, _, AP_Stage, Dir):-
  directory_files(
    [include_directories(false),include_self(false),recursive(true)],
    FromDir,
    FromFiles
  ),
  ap_stage_resource(AP_Stage, Resource, Graph),
  rdf_datatype(Resource, ckan:url, xsd:string, URL, Graph),
  forall(
    member(FromFile, FromFiles),
    stash_output_file(Dir, URL, FromFile)
  ).


stash_output_file(Dir1, URL, FromFile):-
  rdf_atom_md5(URL, 1, Hash),
  directory_file_path(Dir1, Hash, Dir2),
  make_directory_path(Dir2),
  drop_url_name(URL, Dir2),
  file_alternative(FromFile, Dir2, input, nt, ToFile),
  copy_file(FromFile, ToFile).


drop_url_name(URL, Dir):-
  absolute_file_name(basename, File, [access(write),relative_to(Dir)]),
  setup_call_cleanup(
    open(File, write, Out),
    with_output_to(Out, writeln(URL)),
    close(Out)
  ).

