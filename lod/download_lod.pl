:- module(
  download_lod,
  [
    download_lod/3 % +Directory:or([atom,compound])
                   % +URLs:or([iri,list(iri)])
                   % +NumberOfThreads:nonneg
  ]
).

/** <module> Download LOD

@author Wouter Beek
@version 2014/03-2014/05
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(thread)).
:- use_module(library(url)).

:- use_module(generics(uri_ext)).
:- use_module(os(remote_ext)).
:- use_module(os(unpack)).
:- use_module(pl(pl_clas)).
:- use_module(pl(pl_log)).
:- use_module(rdf_file(rdf_ntriples_write)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(void(void_db)). % XML namespace.

:- dynamic(data_directory/1).

:- rdf_meta(store_triple(r,r,o)).

%! seen_dataset(?Url:url) is nondet.

:- thread_local(seen_dataset/1).

%! todo_dataset(?Url:url) is nondet.

:- thread_local(todo_dataset/1).

%! finished_lod_url(?Dataset:atom) is nondet.

:- dynamic(finished_lod_url/1).



%! download_lod(
%!   +DataDirectory:atom,
%!   +Urls:or([iri,list(iri)]),
%!   +NumberOfThreads:nonneg
%! ) is det.

download_lod(DataDir, Iris1, N):-
  is_list(Iris1), !,
  flag(number_of_processed_files, _, 0),
  flag(number_of_triples_written, _, 0),
  read_finished_lod_urls(DataDir),

  % Assert the data directory.
  retractall(data_directory(_)),
  assert(data_directory(DataDir)),

  % Process the resources by authority.
  % This avoids being blocked by servers that do not allow
  % multiple simultaneous requests.
  exclude(finished_lod_url, Iris1, Iris2),
  findall(
    Authority-Uri,
    (
      member(Iri, Iris2),
      uri_iri(Uri, Iri),
      uri_components(Uri, Components),
      uri_data(authority, Components, Authority)
    ),
    Pairs1
  ),
  group_pairs_by_key(Pairs1, Pairs2),

  % Construct the set of goals.
  findall(
    download_lod_authority(DataDir, Pair),
    member(Pair, Pairs2),
    Goals
  ),

  % Run the goals in threads.
  % The number of threads can be given as an option.
  rdf_retractall(_, _, _),
  thread_create(run_goals_in_threads(N, Goals), _, []).
download_lod(DataDir, Iri, N):-
  download_lod(DataDir, [Iri], N).


%! download_lod_authority(
%!   +DataDirectory:compound,
%!   +AuthorityPair:pair(atom,list(pair(atom)))
%! ) is det.
% Downloads the datasets at the given authority.
%
% An authority is represented as a pair of an authority name
% and a list of CKAN resources that -- according to the metadata --
% reside at that authority.

% Skip the first N authorities.
download_lod_authority(DataDir, _-Urls):-
  forall(
    member(Url, Urls),
    (
      % Add another LOD input to the pool.
      register_input(Url),
      process_lod_files(DataDir)
    )
  ).


%! process_lod_files(+DataDirectory:or([atom,compound])) is det.

process_lod_files(DataDir):-
  % Take another LOD input from the pool.
  pick_input(Url), !,
  store_triple(Url2, rdf:type, ap:'LOD-URL'),
  
  % Start message.
  print_message(informational, lod_download_start(Url)),

  % Make sure the remote directory exists.
  url_flat_directory(DataDir, Url, UrlDir),
  make_remote_directory_path(UrlDir),

  % Clear any previous, incomplete results.
  clear_remote_directory(UrlDir),

  % We log the status, all warnings, and all informational messages
  % that are emitted while processing a file.
  run_collect_messages(
    process_lod_file(Url, UrlDir),
    Status,
    Messages
  ),
  % Store the status and all messages.
  log_status(Url, Status),
  maplist(log_message(Url), Messages),
  print_message(informational, lod_downloaded_file(Status,Messages)),

  write_finished_lod_url(Url),

  process_lod_files(DataDir).
% No more inputs to pick.
process_lod_files(_).


%! process_lod_file(+Url:url, +UrlDirectory:or([atom,compound])) is det.

process_lod_file(Url1, UrlDir):-
  % Non-deterministic for multiple entries in one archive stream.
  unpack(Url1, Read, Location),
  store_location_properties(Url1, Location, Url2),

  % Process individual RDF files in a separate RDF transaction and snapshot.
  call_cleanup(
    rdf_transaction(
      process_rdf_file(Url2, Read, UrlDir, Location),
      _,
      [snapshot(true)]
    ),
    close(Read)
  ),

  % Unpack the next entry by backtracking.
  fail.
process_lod_file(_, _).


%! process_rdf_file(
%!   +Url:url,
%!   +Read:stream,
%!   +UrlDirectory:atom,
%!   +Location:dict
%! ) is det.

process_rdf_file(Url, Read, UrlDir, Location):-
  % Guess the serialization format that is used in the given stream.
  rdf_guess_format([], Read, Location, Base, Format),
  store_triple(Url, ap:rdf_serialization_format,
      literal(type(xsd:string,Format))),
  store_triple(Url, ap:base_iri, Base),
  set_stream(Read, file_name(Base)),

  % Load triples in any serialization format.
  rdf_load(
    stream(Read),
    [base_uri(Base),format(Format),register_namespaces(false)]
  ),

  % Asssert some statistics for inclusion in the messages file.
  assert_number_of_triples(Url, TIn),
  store_void_triples,

  % Save triples using the N-Triples serialization format.
  directory_file_path(UrlDir, 'input.nt.gz', Path),
  setup_call_cleanup(
    remote_open(Path, append, Write, [filter(gzip)]),
    rdf_ntriples_write(Write, [bnode_base(Base),number_of_triples(TOut)]),
    close(Write)
  ),

  % Log the number of triples after deduplication.
  store_triple(Url, ap:triples_without_dups, literal(type(xsd:integer,TOut))),
  print_message(informational, rdf_ntriples_written(Path,TIn,TOut)),

  % Make sure any VoID datadumps are considered as well.
  register_void_datasets.



% HELPERS

%! assert_number_of_triples(+Url:url, -NumberOfTriples:nonneg) is det.

assert_number_of_triples(Url, N):-
  % Log the number of triples before deduplication.
  aggregate_all(
    count,
    rdf(_, _, _, _),
    N
  ),
  store_triple(Url, ap:triples_with_dups, literal(type(xsd:integer,N))).


%! lod_resource_path(
%!   +DataDirectory:compound,
%!   +Dataset:iri,
%!   +File:atom,
%!   -Path:atom
%! ) is det.

lod_resource_path(
  remote(User,Machine,DataDir),
  Dataset,
  File,
  remote(User,Machine,Path)
):- !,
  lod_resource_path(DataDir, Dataset, File, Path).
lod_resource_path(DataDir, Dataset, File, Path):-
  url_flat_directory(DataDir, Dataset, UrlDir),
  directory_file_path(UrlDir, File, Path).


%! log_message(+Dataset:iri, +Message:compound) is det.

log_message(Dataset, Message):-
  with_output_to(atom(String), write_canonical_blobs(Message)),
  store_triple(Dataset, ap:message, literal(type(xsd:string,String))).


%! log_status(+Dataset:iri, +Exception:compound) is det.

log_status(_, false):- !.
log_status(_, true):- !.
log_status(Dataset, exception(Error)):-
  with_output_to(atom(String), write_canonical_blobs(Error)),
  store_triple(Dataset, ap:status, literal(type(xsd:string,String))).


%! pick_input(-Url:url) is nondet.

pick_input(Url):-
  retract(todo_dataset(Url)).


%! read_finished_lod_urls(+DataDirectory:atom) is det.

read_finished_lod_urls(DataDir):-
  directory_file_path(DataDir, 'finished_lod_urls.log', File),
  (
    catch(read_file_to_terms(File, Terms, []), _, fail)
  ->
    maplist(assert, Terms)
  ;
    true
  ).


%! register_input(+Url:url) is det.

register_input(Url):-
  assert(todo_dataset(Url)),
  assert(seen_dataset(Url)).


%! register_void_datasets is det.

register_void_datasets:-
  % Add all VoID datadumps to the TODO list.
  aggregate_all(
    set(Url),
    (
      rdf(_, void:dataDump, Url),
      \+ seen_dataset(Url)
    ),
    Urls
  ),
  print_message(informational, found_void_lod_urls(Urls)),
  forall(
    member(Url, Urls),
    register_input(Url)
  ).


%! run_goals_in_threads(+NumberOfThreads:nonneg, :Goals) is det.

run_goals_in_threads(N, Goals):-
  N > 1, !,
  concurrent(N, Goals, []).
run_goals_in_threads(_, Goals):-
  maplist(call, Goals).


%! store_location_properties(+Url1:url, +Location:dict, -Url2:url) is det.

store_location_properties(Url1, Location, Url2):-
  (
    Data1 = Location.get(data),
    exclude(filter, Data1, Data2),
    Data2 = [ArchiveEntry]
  ->
    Name = ArchiveEntry.get(name),
    atomic_list_concat([Url1,Name], '/', Url2),
    store_triple(Url1, ap:archive_contains, Url2),
    store_triple(Url2, ap:format,
        literal(type(xsd:string,ArchiveEntry.get(format)))),
    store_triple(Url2, ap:size,
        literal(type(xsd:integer,ArchiveEntry.get(size))))
  ;
    Url2 = Url1
  ),
  store_triple(Url2, ap:content_type,
      literal(type(xsd:string,Location.get(content_type)))),
  store_triple(Url2, ap:content_length,
      literal(type(xsd:integer,Location.get(content_length)))),
  store_triple(Url2, ap:last_modified,
      literal(type(xsd:string,Location.get(last_modified)))),
  store_triple(Url2, ap:url, literal(type(xsd:string,Location.get(url)))).
filter(filter(_)).


%! store_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal])
%! ) is det.

store_triple(S, P, O):-
  with_mutex(
    store_triple,
    (
      data_directory(DataDir),
      absolute_file_name('messages.log', File, [relative_to(DataDir)]),
      setup_call_cleanup(
        remote_open(File, append, Stream),
        (
          writeq(Stream, rdf_triple(S, P, O)),
          write(Stream, '.'),
          nl(Stream)
        ),
        close(Stream)
      )
    )
  ).


%! store_void_triples is det.

store_void_triples:-
  aggregate_all(
    set(P),
    (
      rdf_current_predicate(P),
      rdf_global_id(void:_, P)
    ),
    Ps
  ),
  forall(
    (
      member(P, Ps),
      rdf(S, P, O)
    ),
    store_triple(S, P, O)
  ).


%! write_finished_lod_url(+Dataset:atom) is det.

write_finished_lod_url(Url):-
  with_mutex(
    finished,
    (
      data_directory(DataDir),
      directory_file_path(DataDir, 'finished_lod_urls.log', File),
      setup_call_cleanup(
        open(File, append, Stream),
        (
          writeq(Stream, finished_lod_url(Url)),
          write(Stream, '.'),
          nl(Stream)
        ),
        close(Stream)
      )
    )
  ).



% MESSAGES

:- multifile(prolog:message/1).

prolog:message(lod_download_start(Url)) -->
  {flag(number_of_processed_files, X, X + 1)},
  ['[~D] [~w]'-[X,Url]].
prolog:message(lod_downloaded_file(Status,Messages)) -->
  prolog_status(Status),
  prolog_messages(Messages),
  [nl].
prolog:message(found_void_lod_urls([])) --> !.
prolog:message(found_void_lod_urls([H|T])) -->
  ['A VoID dataset was found: ',H,nl],
  prolog:message(found_void_lod_urls(T)).

prolog:message(rdf_ntriples_written(File,TIn,TOut)) -->
  ['['],
    number_of_triples_written(TOut),
    number_of_duplicates_written(TIn, TOut),
    total_number_of_triples_written(TOut),
  ['] ['],
    remote_file(File),
  [']'].

number_of_duplicates_written(0) --> !, [].
number_of_duplicates_written(T) --> [' (~D dups)'-[T]].

number_of_duplicates_written(TIn, TOut) -->
  {T0 is TIn - TOut},
  number_of_duplicates_written(T0).

number_of_triples_written(0) --> !, [].
number_of_triples_written(T) --> ['+~D'-[T]].

prolog_status(false) --> !, [].
prolog_status(true) --> !, [].
prolog_status(exception(Error)) -->
  {print_message(error, Error)}.

prolog_messages([]) --> !, [].
prolog_messages([message(_,Kind,Lines)|T]) -->
  ['  [~w] '-[Kind]],
  prolog_lines(Lines),
  [nl],
  prolog_messages(T).

prolog_lines([]) --> [].
prolog_lines([H|T]) -->
  [H],
  prolog_lines(T).

remote_file(remote(User,Machine,Path)) --> !,
  [User,'@',Machine,':',Path].
remote_file(File) -->
  [File].

total_number_of_triples_written(0) --> !, [].
total_number_of_triples_written(T) -->
  {
    with_mutex(
      number_of_triples_written,
      (
        flag(number_of_triples_written, All1, All1 + T),
        All2 is All1 + T
      )
    )
  },
  [' (~D tot)'-[All2]].

