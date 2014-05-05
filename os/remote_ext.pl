:- module(
  remote_ext,
  [
    clear_remote_directory/1, % +RemoteDirectory:or([atom,compound])
    exists_remote_file/1, % +RemoteFile:or([atom,compound])
    make_remote_directory/1, % +RemoteDirectory:or([atom,compound])
    make_remote_directory_path/1, % +RemoteDirectory:or([atom,compound])
    remote_open/3, % +RemoteFile:or([atom,compound]),
                   % +Mode:oneof([append,read,write]),
                   % -Stream:stream,
    remote_open/4 % +RemoteFile:or([atom,compound]),
                  % +Mode:oneof([append,read,write]),
                  % -Stream:stream,
                  % +Options:list(nvpair)
  ]
).

/** <module> Remote extensions

Support for files residing on remote machines.

@author Wouter Beek
@version 2014/05
*/

:- use_module(library(filesex)).
:- use_module(library(process)).

:- use_module(os(dir_ext)).



%! clear_remote_directory(+RemoteDirectory:or([atom,compound])) is det.

clear_remote_directory(remote(User,Machine,Dir)):- !,
  atomic_list_concat([User,Machine], '@', UserMachine),
  append_directories(Dir, '*', Regex),
  atomic_list_concat([ssh,UserMachine,rm,Regex], ' ', Command),
  ignore(
    catch(
      process_create(path(sh), ['-c',Command], [stderr(null),stdout(null)]),
      _,
      fail
    )
  ).
clear_remote_directory(Dir):-
  delete_directory_contents(Dir).


%! exists_remote_file(+RemoteFile:or([atom,compound])) is semidet.

exists_remote_file(remote(User,Machine,File)):- !,
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_list_concat([ssh,UserMachine,ls,File], ' ', Command),
  catch(
    process_create(path(sh), ['-c',Command], []),
    _,
    fail
  ).
exists_remote_file(File):-
  exists_file(File).


%! make_remote_directory(+RemoteDirectory:or([atom,compound])) is det.

make_remote_directory(remote(User,Machine,Dir)):- !,
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_list_concat([ssh,UserMachine,mkdir,Dir], ' ', Command),
  process_create(path(sh), ['-c',Command], []).
make_remote_directory(Dir):-
  make_directory(Dir).


%! make_remote_directory_path(+RemoteDirectory:or([atom,compound])) is det.

make_remote_directory_path(remote(User,Machine,Dir)):- !,
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_list_concat([ssh,UserMachine,mkdir,'-p',Dir], ' ', Command),
  process_create(path(sh), ['-c',Command], []).
make_remote_directory_path(Dir):-
  make_directory_path(Dir).


%! remote_open(
%!   +RemoteFile:or([atom,compound]),
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream
%! ) is det.

remote_open(RemotePath, Mode, Stream):-
  remote_open(RemotePath, Mode, Stream, []).

%! remote_open(
%!   +RemoteFile:or([atom,compound]),
%!   +Mode:oneof([append,read,write]),
%!   -Stream:stream,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * `filter(+Filter:oneof([gzip]))`
%   * Other options are passed to open/4.

remote_open(remote(User,Machine,Path), Mode, Stream, Options):- !,
  atomic_list_concat([User,Machine], '@', UserMachine),
  atomic_concat(Path, '"', Suffix),

  % CAT append uses a double greater than sign.
  (
    Mode == append
  ->
    CatSign = '>>'
  ;
    CatSign = '>'
  ),

  atomic_list_concat([ssh,UserMachine,'"cat',CatSign,Suffix], ' ', Command),

  % Gzip in stream.
  (
    option(filter(gzip), Options)
  ->
    gzopen(pipe(Command), write, Stream, Options)
  ;
    open(pipe(Command), write, Stream, Options)
  ).
remote_open(File, Mode, Stream, Options):-
  (
    option(filter(gzip), Options)
  ->
    gzopen(File, Mode, Stream, Options)
  ;
    open(File, Mode, Stream, Options)
  ).

