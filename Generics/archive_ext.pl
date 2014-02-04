:- module(
  archive_ext,
  [
    extract_archive/3, % +FromFile:atom
                       % +ToDirectory:atom
                       % -Conversions:list(oneof([gunzipped,untarred,unzipped]))
% AP
    extract_archives/3 % +FromDirectory:atom
                       % +ToDirectory:atom
                       % -AP_Status:compound
  ]
).

/** <module> Archive extensions

Extensions to the support for archived files.

@author Wouter Beek
@version 2013/12-2014/02
*/

:- use_module(generics(db_ext)).
:- use_module(library(filesex)).
:- use_module(library(process)).
:- use_module(os(dir_ext)).
:- use_module(os(mime_type)).

:- mime_register_type(application, 'x-bzip2', bz2).
:- db_add_novel(user:prolog_file_type(bz2, archive)).
:- mime_register_type(application, 'x-gzip', gz).
:- db_add_novel(user:prolog_file_type(gz, archive)).
:- mime_register_type(application, 'x-tar', tar).
:- db_add_novel(user:prolog_file_type(tar, archive)).
:- mime_register_type(application, 'zip', zip).
:- db_add_novel(user:prolog_file_type(zip, archive)).



%! extract_archive(
%!   +FromFile:atom,
%!   +ToDirectory:atom,
%!   -Conversions:list(oneof([gunzipped,untarred,unzipped]))
%! ) is det.

extract_archive(File, Dir, [Conversion|Conversions]):-
  file_name_extension(Base, Ext, File),
  prolog_file_type(Ext, archive), !,
  extract_archive(Ext, File, Base, Conversion),
  extract_archive(Base, Dir, Conversions).
extract_archive(File, Dir, []):-
  copy_file(File, Dir).


%! extract_archive(
%!   +Extension:oneof([bz2,gz,tgz,zip]),
%!   +FromFile:atom,
%!   +ToName:atom,
%!   -Conversion:oneof([gunzipped,untarred,unzipped])
%! ) is semidet.

extract_archive(bz2, File, _, gunzipped):- !,
  process_create(path(bunzip2), ['-f',file(File)], []).
extract_archive(gz, File, _, gunzipped):- !,
  process_create(path(gunzip), ['-f',file(File)], []).
extract_archive(tgz, File, _, untarred):- !,
  process_create(path(tar), [zxvf,file(File)], []).
extract_archive(zip, File, Base, unzipped):- !,
  process_create(path(unzip), [file(File),'-o',file(Base)], []).


%! extact_archives(
%!   +FromDirectory:atom,
%!   +ToDirectory:atom,
%!   -AP_Status:compound
%! ) is det.

extract_archives(
  FromDir,
  ToDir,
  ap(status(succeed),extract_archive(OnFiles))
):-
  directory_files([recursive(false)], FromDir, FromFiles),
  findall(
    on_file(FromFile,conversions(Conversions)),
    (
      member(FromFile, FromFiles),
      extract_archive(FromFile, ToDir, Conversions)
    ),
    OnFiles
  ).

