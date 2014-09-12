:- module(
  http_download,
  [
    download_to_file/2, % +Url:url
                        % ?File:atom
    download_to_file/3 % +Url:url
                       % ?File:atom
                       % +Options:list(nvpair)
  ]
).

/** <module> HTTP download

Support for downloading files over HTTP(S).

@author Wouter Beek
@version 2013/05, 2013/09, 2013/11-2014/05
*/

:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_module(library(uri)).

:- use_module(generics(uri_ext)).
:- use_module(http(http_goal)).
:- use_module(os(file_ext)).
:- use_module(os(io_ext)).



%! download_to_file(+Url:url, ?File:atom) is det.
% @see Wrapper for download_to_file/2, which empty options list.

download_to_file(Url, File):-
  download_to_file(Url, File, []).

%! download_to_file(+Url:url, ?File:atom, +Options:list(nvpair)) is det.
% Downloads files from a URL to either the given file (when instantiated)
% of to the a file name that is created based on the URL.
%
% The following options are supported:
%   * =|freshness_lifetime(+or([between(0.0,inf),oneof([inf])]))|=
%     Sets whether -- and if so, when -- files that were downloaded
%     in the past are redownloaded and overwritten.
%     Default is `inf`.
%   * Other options are passed to http_goal/3 and http_open/3.
%
% @see url_nested_file/3 for how the file name is created based on the URL.
%      This requires the file search path `data` to be set to a directory
%      with write access.

% The file was already downloaded in the past.
download_to_file(Url, File, Options):-
  nonvar(File),
  exists_file(File), !,
  (
    option(freshness_lifetime(FreshnessLifetime), Options, inf),
    is_fresh_file(File, FreshnessLifetime)
  ->
    access_file(File, read)
  ;
    delete_file(File),
    download_to_file(Url, File, Options)
  ).
% An absolute file name is specified.
download_to_file(Url, File, Options):-
  nonvar(File),
  is_absolute_file_name(File), !,
  file_directory_name(File, Dir),
  make_directory_path(Dir),

  % Check write access to the file.
  access_file(File, write),

  % Check the URL.
  uri_is_global(Url),

  % Multiple threads could be downloading the same file,
  % so we cannot download to the file's systematic name.
  % Instead we save to a thread-specific name.
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', ThreadName),
  file_name_extension(File, ThreadName, TmpFile),

  % The actual downloading part.
  http_goal(Url, file_from_stream(TmpFile), Options),

  % Give the file its original name.
  rename_file(TmpFile, File).
% No file name is given; create a file name in a standardized way,
% based on the URL.
download_to_file(Url, File, Options):-
  url_nested_file(data(.), Url, File),
  download_to_file(Url, File, Options).

