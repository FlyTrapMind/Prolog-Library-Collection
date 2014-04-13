% This file allows remotely stored file to be locally copied.
% It presupposes that the file search path named `project` is set.
%
% This file allows a file to be downloaded
% over unreliable internet connections,
% since it simply retries until it succeeds.

:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).

:- multifile(prolog:message//1).



base_url(Url):-
  uri_components(
    Url,
    uri_components(
      https,
      'github.com',
      'wouterbeek/Prolog-Communities/raw/master'
    )
  ).


cert_verify(_, _, _, _, _):- !.


%! guarantee_download(+Url:atom, +Path:atom) is det.
% Make sure the given URL is downloaded to a local file.
% We keep retrying this, until a local copy is obtained.

% The remote file was already fetched previously.
guarantee_download(_, Path):-
  exists_file(Path), !.
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
      print_message(warning, http_error(Error,Url)),
      guarantee_download(Url, Path)
    )
  ).


%! http_process(
%!   +Status:between(100,999),
%!   +HttpStream:stream,
%!   +Url:atom,
%!   +Path:atom
%! ) is det.

% The HTTP status code indicates success,
% so we make a local copy of the remote data.
http_process(Status, HttpStream, _, File):-
  between(200, 299, Status), !,
  setup_call_cleanup(
    open(File, write, FileStream, [type(binary)]),
    copy_stream_data(HttpStream, FileStream),
    close(FileStream)
  ).
% The HTTP status code indicates failure,
% so we retry indifinately.
http_process(Status, _, Url, Path):-
  print_message(warning, http_status(Status,Url)),
  guarantee_download(Url, Path).


load_remote_file(Base):-
  base_url(Url),
  absolute_file_name(project(Base), File, [access(write),file_type(prolog)]),
  guarantee_download(Url, File),
  ensure_loaded(File).


prolog_repository(Mode):-
  load_remote_file(optparse2),
  (
    Mode == remote
  ->
    load_remote_file(prolog_repository_remote)
  ;
    Mode == local
  ->
    load_remote_file(prolog_repository_local)
  ).



% MESSAGES

prolog:message(http_error(Error,Url)) -->
  {term_to_atom(Error, Atom)},
  ['HTTP error: ',Atom,' for URL ',Url,' '],
  retrying.

prolog:message(http_status(Status,Url)) -->
  ['HTTP status code: ',Status,' for URL ',Url,' '],
  retrying.

retrying -->
  ['Retrying...'].

