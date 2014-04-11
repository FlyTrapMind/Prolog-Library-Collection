/* Use remote modules

Allows remote Prolog modules to be imported in the same way in which
use_module/1 imports local Prolog modules.

@author Wouter Beek
@tbd Allow the file argument to be instantiated to a **list** of files.
@version 2014/04
*/

:- use_module(library(filesex)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(option)).
:- use_module(library(optparse)).
:- use_module(library(uri)).

:- meta_predicate(use_remote_module(:)).
:- meta_predicate(use_remote_module(+,:)).
:- meta_predicate(use_remote_module(+,:,+)).
:- meta_predicate(reexport_remote_module(:)).
:- meta_predicate(reexport_remote_module(+,:)).
:- meta_predicate(reexport_remote_module(+,:,+)).

:- meta_predicate(print_message_start_end(+,+,:)).

%! github_repository(
%!   ?RepositoryId:atom,
%!   ?User:atom,
%!   ?RepositoryId:atom,
%!   ?ProjectDirectory:atom
%! ) is nondet.

:- multifile(user:github_repository/4).

:- multifile(prolog:message//1).

:- dynamic(imports/2).

% default_repository(?Repository:atom) is semidet.
% There is at most one default repository.

:- dynamic(default_repository/1).

:- initialization(init_use_remote_module).

init_use_remote_module:-
  flag(number_of_downloaded_files, _, 1),
  source_file(init_use_remote_module, ThisFile),
  file_directory_name(ThisFile, ThisDir),
  opt_arguments(
    [
      [
        default(false),
        help('Force all files to be redownloaded.'),
        longflags([redownload]),
        opt(redownload),
        shortflags([r]),
        type(boolean)
      ]
    ],
    O1,
    _,
    []
  ),
  call_remote_goal(
    github,
    [repository('Prolog-Library-Collection'),user(wouterbeek)|O1],
    remotes,
    register_remotes(ThisDir, O1)
  ).


%! call_remote_goal(
%!   +Type:oneof([github]),
%!   +Options:list(nvpair),
%!   +Base:atom,
%!   :Goal
%! ) is det.
% The following options are supported for `Type=github`:
%    * `repository(+RepositoryName:atom)`
%    * `user(+User:atom)`

call_remote_goal(github, O1, Base, Goal):-
  option(user(User), O1),
  option(repository(RepositoryName), O1),
  absolute_file_name(Base, LocalPath, [access(write),file_type(prolog)]),
  file_name_extension(Base, pl, File),
  print_message_start_end(
    informational,
    load(Base),
    setup_call_cleanup(
      (
        fetch_remote_file(github, User, RepositoryName, [File], LocalPath),
        ensure_loaded(LocalPath)
      ),
      call(Goal),
      (
        unload_file(LocalPath),
        delete_file(LocalPath)
      )
    )
  ).


%! ensure_remote_loaded(+FileSpec:compound) is det.
%! ensure_remote_loaded(+RepositoryId:atom, +FileSpec:compound) is det.
% Remote variant of ensure_loaded/1

ensure_remote_loaded(FileSpec):-
  default_repository(DefaultRepository),
  ensure_remote_loaded(DefaultRepository, FileSpec).

ensure_remote_loaded(RepositoryId, FileSpec):-
  fetch_remote_file(RepositoryId, FileSpec, LocalFile),
  ensure_loaded(LocalFile).


%! fetch_remote_file(
%!   +RepositoryId:atom,
%!   +ModuleSpec:compund,
%!   -LocalPath:atom
%! ) is det.

fetch_remote_file(RepositoryId, ModuleSpec, LocalPath):-
  github_repository(RepositoryId, User, RepositoryName, LocalRelativeTo1),
  atomic_list_concat([LocalRelativeTo1,RepositoryId], '/', LocalRelativeTo2),
  absolute_file_name(ModuleSpec, LocalPath, [file_type(prolog)]),
  relative_file_name(LocalPath, LocalRelativeTo2, RelativePath),
  atomic_list_concat(Components, '/', RelativePath),
  fetch_remote_file(github, User, RepositoryName, Components, LocalPath).


%! fetch_remote_file(
%!   +Type:oneof([github]),
%!   +User:atom,
%!   +RepositoryId:atom,
%!   +Components:list,
%!   +LocalPath:atom
%! ) is det.

fetch_remote_file(github, _, _, _, LocalPath):-
  exists_file(LocalPath), !.
fetch_remote_file(github, User, RepositoryName, Components, LocalPath):-
  atomic_list_concat(
    ['',User,RepositoryName,raw,master|Components],
    '/',
    Path
  ),
  uri_components(Url, uri_components(htts,'github.com',Path,_,_)),
  file_directory_name(LocalPath, LocalDirectory),
  make_directory_path(LocalDirectory),
  guarantee_download(Url, LocalPath),
  print_message(information, downloaded(Components)).


%! guarantee_download(+Url:atom, +Path:atom) is det.

guarantee_download(Url, Path):-
  catch(
    setup_call_cleanup(
      http_open(
        Url,
        HttpStream,
        [cert_verify_hook(cert_verify),status_code(Status),timeout(1)]
      ),
      http_process(Status, HttpStream, Url, Path),
      close(HttpStream)
    ),
    Error,
    (
      print_message(warning, http_error(Error)),
      guarantee_download(Url, Path)
    )
  ).


%! http_process(
%!   +Status:between(100,999),
%!   +HttpStream:stream,
%!   +Url:atom,
%!   +Path:atom
%! ) is det.

http_process(Status, HttpStream, _, File):-
  between(200, 299, Status), !,
  setup_call_cleanup(
    open(File, write, FileStream, [type(binary)]),
    copy_stream_data(HttpStream, FileStream),
    close(FileStream)
  ).
http_process(Status, _, Url, Path):-
  print_message(warning, http_status(Status)),
  guarantee_download(Url, Path).


%! reexport_remote_module(+ModuleSpec:compound) is det.
%! reexport_remote_module(+RepositoryId:atom, +ModuleSpec:compound) is det.
%! reexport_remote_module(
%!   +RepositoryId:atom,
%!   +ModuleSpec:compound,
%!   +Import:list(compound)
%! ) is det.

reexport_remote_module(ModuleSpec):-
  default_repository(DefaultRepository),
  reexport_remote_module(DefaultRepository, ModuleSpec).

reexport_remote_module(RepositoryId, CallingModule:CalledModuleSpec):-
  flag(level_of_nesting, M, M + 1),
  fetch_remote_file(RepositoryId, CalledModuleSpec, LocalFile),
  CallingModule:reexport_remote_module(LocalFile),
  flag(level_of_nesting, N, N - 1),
  store_import_relation(CallingModule, CalledModuleSpec).

reexport_remote_module(RepositoryId, CallingModule:CalledModuleSpec, Import):-
  flag(level_of_nesting, M, M + 1),
  fetch_remote_file(RepositoryId, CalledModuleSpec, LocalFile),
  CallingModule:reexport(LocalFile, Import),
  flag(level_of_nesting, N, N - 1),
  store_import_relation(CallingModule, CalledModuleSpec).


%! register_remote(
%!   +Type:oneof([github]),
%!   +Name:atom,
%!   +LocalDirectory:atom,
%!   +Options:list(nvpair)
%! ) is det.
% We lump generic options and repository-specific parameters together.
%
% The following options are supported for all
%    * `default(+DefaultRepository:boolean)`
%      Set this repository as the default.
%      When multiple repositories are registered with this option,
%      only the last one will be the default.
%
% The following repository-specific parameters are supported
% for `Type=github`:
%    * `repository(+Name:atom)`
%      The name of the Github repository.
%    * `user(+Name:atom)`
%      The name of the Github user who manages the reposository.

register_remote(github, RepositoryId, Dir, O1):-
  option(user(User), O1),
  option(repository(RepositoryName), O1),
  assert(user:github_repository(RepositoryId, User, RepositoryName, Dir)),

  % Set as the (new) default repository.
  (
    option(default(true), O1)
  ->
    retractall(default_repository(_)),
    assert(default_repository(RepositoryId))
  ;
    true
  ),

  % Delete previously downloaded files.
  (
    option(redownload(true), O1),
    directory_file_path(Dir, RepositoryId, OldDir),
    absolute_file_name(
      OldDir,
      _,
      [access(write),file_errors(fail),file_type(directory)]
    )
  ->
    delete_directory_and_contents(OldDir)
  ;
    true
  ),
  
  % Load the remote index.
  directory_file_path(Dir, RepositoryId, ParentDir),
  make_directory_path(ParentDir),
  call_remote_goal(github, O1, index, index(ParentDir)).


%! store_import_relation(
%!   +CallingModule:atom,
%!   +CalledModuleSpec:compound
%! ) is det.

store_import_relation(CallingModule, CalledModuleSpec):-
  % Retrieve either the file or the class name of the calling module.
  (
    module_property(CallingModule, file(CallingModuleName))
  ->
    true
  ;
    module_property(CallingModule, class(CallingModuleName))
  ),
  
  % The absolute file name of the called module.
  absolute_file_name(
    CalledModuleSpec,
    CalledModuleName,
    [access(read),file_type(prolog)]
  ),
  
  assert(imports(CallingModuleName, CalledModuleName)).


%! use_remote_module(:ModuleSpec:compound) is det.
%! use_remote_module(+RepositoryId:atom, :ModuleSpec:compound) is det.
%! use_remote_module(
%!   +RepositoryId:atom,
%!   :ModuleSpec:compound,
%!   +ImportList:list
%! ) is det.
% Remote variant of use_module/1, loading a Prolog module from the Web.
%
% Example URL:
% ~~~{.url}
% https://github.com/wouterbeek/PGC/raw/master/OS/file_ext.pl
% ~~~

use_remote_module(ModuleSpec):-
  default_repository(DefaultRepository),
  use_remote_module(DefaultRepository, ModuleSpec).

use_remote_module(RepositoryId, CallingModule:CalledModuleSpec):-
  flag(level_of_nesting, M, M + 1),
  fetch_remote_file(RepositoryId, CalledModuleSpec, LocalFile),
  CallingModule:use_module(LocalFile),
  flag(level_of_nesting, N, N - 1),
  store_import_relation(CallingModule, CalledModuleSpec).

use_remote_module(RepositoryId, CallingModule:CalledModuleSpec, ImportList):-
  flag(level_of_nesting, M, M + 1),
  fetch_remote_file(RepositoryId, CalledModuleSpec, LocalFile),
  CallingModule:use_module(LocalFile, ImportList),
  flag(level_of_nesting, N, N - 1),
  store_import_relation(CallingModule, CalledModuleSpec).



% Informational messages.

print_message_start_end(Type, Msg, Goal):-
  print_message(Type, start(Msg)),
  Goal,
  print_message(Type, end(Msg)).

prolog:message(start(Action)) -->
  ['Start '],
  action(Action),
  ['...'].

prolog:message(end(Action)) -->
  ['End '],
  action(Action),
  ['.'].

action(load(Object)) -->
  ['loading '],
  object(Object).
action(reexport(Object)) -->
  ['re-exporting '],
  object(Object).

object(index) -->
  ['repository index'].
object(file(File)) -->
  ['file ',File].
object(module(Module)) -->
  ['module ~w'-[Module]].

prolog:message(downloaded(L)) -->
  indentation,
  {
    flag(number_of_downloaded_files, N, N + 1),
    atomic_list_concat(L, '/', Name)
  },
  [N,':',Name].

indentation -->
  {flag(level_of_nesting, M, M)},
  indentation(M).

indentation(0) --> [].
indentation(M1) -->
  {M2 is M1 - 1},
  ['  '],
  indentation(M2).

prolog:message(http_error(Error)) -->
  {term_to_atom(Error, Atom)},
  ['HTTP error: ',Atom,'. '],
  retrying.

prolog:message(http_status(Status)) -->
  ['HTTP status code: ',Status,'. '],
  retrying.

retrying -->
  ['Retrying...'].



% Support predicates.

opt_arguments(OptSpecs, Opts, PositionalArgs, ParseOptions):-
  current_prolog_flag(argv, Argv),
  ignore(
    catch(
      opt_parse(OptSpecs, Argv, Opts, PositionalArgs, ParseOptions),
      E,
      print_message(error, E)
    )
  ).

