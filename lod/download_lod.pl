:- module(
  download_lod,
  [
    download_lod/2 % +Directory:or([atom,compound])
                   % +Input:or([atom,pair(atom)])
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

%! seen_dataset(?Dataset:atom, ?File:atom) is nondet.

:- thread_local(seen_dataset/2).

%! todo_dataset(?Dataset:atom, ?File:atom) is nondet.

:- thread_local(todo_dataset/2).

%! finished(?Dataset:atom) is nondet.

:- dynamic(finished/1).



%! download_lod(+DataDirectory:atom, +Input:or([atom,pair(atom)])) is det.

download_lod(DataDir, Pairs1):-
  is_list(Pairs1), !,
  flag(number_of_triples_written, _, 0),
  read_finished(DataDir),

  % Assert the data directory.
  retractall(data_directory(_)),
  assert(data_directory(DataDir)),

  % Process the resources by authority.
  % This avoids being blocked by servers that do not allow
  % multiple simultaneous requests.
  findall(
    UrlAuthority-(Dataset-Iri),
    (
      member(Dataset-Iri, Pairs1),
      uri_components(Iri, Components),
      uri_data(authority, Components, UrlAuthority)
    ),
    Pairs2
  ),
  exclude(finished_pair, Pairs2, Pairs3),
  group_pairs_by_key(Pairs3, Pairs4),

  length(Pairs4, Length), %DEB
  writeln(Length), %DEB

  % Construct the set of goals.
  findall(
    download_lod_authority(I, DataDir, Pair),
    nth0(I, Pairs4, Pair),
    Goals
  ),

  % Run the goals in threads.
  % The number of threads can be given as an option.
  rdf_retractall(_, _, _),
  run_goals_in_threads(Goals).
download_lod(DataDir, Dataset-Location):- !,
  download_lod(DataDir, [Dataset-Location]).
download_lod(DataDir, Location):-
  download_lod(DataDir, Location-Location).

finished_pair(Dataset-_):-
  finished(Dataset).


%! download_lod_authority(
%!   +Index:nonneg,
%!   +DataDirectory:compound,
%!   +UrlAuthority:pair(atom,list(pair(atom)))
%! ) is det.
% Downloads the datasets at the given authority.
%
% An authority is represented as a pair of an authority name
% and a list of CKAN resources that -- according to the metadata --
% reside at that authority.

% Skip the first N authorities.
download_lod_authority(I, _, _):-
  I < 0, !.
download_lod_authority(I, DataDir, _-Pairs):-
  forall(
    member(Dataset-Iri, Pairs),
    (
      % Add another LOD input to the pool.
      register_input(Dataset, Iri),
      process_lod_files(I, DataDir)
    )
  ).


%! process_lod_files(+Index:nonneg, +DataDirectory:or([atom,compound])) is det.

process_lod_files(I, DataDir):-
  % Take another LOD input from the pool.
  pick_input(Dataset, Iri), !,

  % Start message.
  print_message(informational, lod_download_start(I,Iri)),

  % CKAN URLs are sometimes non-URL IRIs.
  iri_to_url_conversion(Dataset, Iri, Url),

  % Make sure the remote directory exists.
  url_flat_directory(DataDir, Url, UrlDir),
  make_remote_directory_path(UrlDir),

  % Clear any previous, incomplete results.
  clear_remote_directory(UrlDir),

  % We log the status, all warnings, and all informational messages
  % that are emitted while processing a file.
  run_collect_messages(
    process_lod_file(Dataset, Url, UrlDir),
    Status,
    Messages
  ),
  % Store the status and all messages.
  log_status(Dataset, Status),
  maplist(log_message(Dataset), Messages),
  print_message(informational, lod_downloaded_file(Status,Messages)),

  write_finished(Dataset),

  process_lod_files(I, DataDir).
% No more inputs to pick.
process_lod_files(_, _).


%! process_lod_file(+Dataset:iri, +Url:url, +UrlDirectory:or([atom,compound])) is det.

process_lod_file(Dataset, Url, UrlDir):-
  % Non-deterministic for multiple entries in one archive stream.
  unpack(Url, Read, Location),

  % Process individual RDF files in a separate RDF transaction and snapshot.
  call_cleanup(
    rdf_transaction(
      process_rdf_file(Dataset, Read, UrlDir, Location),
      _,
      [snapshot(true)]
    ),
    close(Read)
  ),

  % Unpack the next entry by backtracking.
  fail.
process_lod_file(_, _, _).


%! process_rdf_file(
%!   +Dataset:iri,
%!   +Read:stream,
%!   +UrlDirectory:atom,
%!   +Location:dict
%! ) is det.

process_rdf_file(Dataset, Read, UrlDir, Location):-
  % Guess the serialization format that is used in the given stream.
  rdf_guess_format([], Read, Location, Base, Format),
  set_stream(Read, file_name(Base)),
  print_message(informational, rdf_load_any(rdf(Base, Format))),

  % Load triples in any serialization format.
  rdf_load(
    stream(Read),
    [base_uri(Base),format(Format),register_namespaces(false)]
  ),

  % Asssert some statistics for inclusion in the messages file.
  assert_number_of_triples(Dataset, T1),
  store_void_triples,

  % Save triples using the N-Triples serialization format.
  directory_file_path(UrlDir, 'input.nt.gz', Path),
  setup_call_cleanup(
    remote_open(Path, append, Write, [filter(gzip)]),
    rdf_ntriples_write(Write, [bnode_base(Base),number_of_triples(T2)]),
    close(Write)
  ),
  print_message(informational, rdf_ntriples_written(Path,T2)),

  % Log the number of triples after deduplication.
  store_triple(Dataset, ap:triples_without_dups, literal(type(xsd:integer,T2))),
  flag(number_of_triples_written, X, X),
  format(current_output, '~D --[deduplicate]--> ~D (All: ~D)~n', [T1,T2,X]),

  % Make sure any VoID datadumps are considered as well.
  register_void_datasets.



% HELPERS

%! assert_number_of_triples(+Dataset:iri, -NumberOfTriples:nonneg) is det.

assert_number_of_triples(Dataset, N):-
  % Log the number of triples before deduplication.
  aggregate_all(
    count,
    rdf(_, _, _, _),
    N
  ),
  store_triple(Dataset, ap:triples_with_dups, literal(type(xsd:integer,N))).


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


%! pick_input(-Dataset:atom, -File:atom) is nondet.

pick_input(Dataset, File):-
  retract(todo_dataset(Dataset, File)).


%! read_finished(+DataDirectory:atom) is det.

read_finished(DataDir):-
  directory_file_path(DataDir, 'finished.log', File),
  (
    catch(read_file_to_terms(File, Terms, []), _, fail)
  ->
    maplist(assert, Terms)
  ;
    true
  ).


%! register_input(+Dataset:iri, +Url:url) is det.

register_input(Dataset, Url):-
  assert(todo_dataset(Dataset, Url)),
  assert(seen_dataset(Dataset, Url)).


%! register_void_datasets is det.

register_void_datasets:-
  % Add all VoID datadumps to the TODO list.
  aggregate_all(
    set(Dataset-Url),
    (
      rdf(Dataset, void:dataDump, Url),
      \+ seen_dataset(Dataset, Url)
    ),
    VoidPairs
  ),
  print_message(informational, found_void_datadumps(VoidPairs)),
  forall(
    member(Dataset-Url, VoidPairs),
    register_input(Dataset, Url)
  ).


%! run_goals_in_threads(:Goals) is det.

run_goals_in_threads(Goals):-
  once(read_options(O1)),
  (
    option(threads(NumberOfThreads), O1),
    NumberOfThreads > 1
  ->
    concurrent(NumberOfThreads, Goals, [])
  ;
    maplist(call, Goals)
  ).


%! store_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal])
%! ) is det.

store_triple(S, P, O):-
  with_mutex(store_triple, store_triple_mutex(S, P, O)).

store_triple_mutex(S, P, O):-
  data_directory(DataDir),
  absolute_file_name('messages.log', File, [relative_to(DataDir)]),
  setup_call_cleanup(
    remote_open(File, append, Stream),
    (
      writeq(Stream, rdf(S, P, O)),
      write(Stream, '.'),
      nl(Stream)
    ),
    close(Stream)
  ).


%! iri_to_url_conversion(+Dataset:iri, +Iri:atom, -Url:atom) is det.

iri_to_url_conversion(Dataset, Iri, Url):-
  url_iri(Url, Iri),
  store_triple(Dataset, ap:url, Url).


%! write_finished(+Dataset:atom) is det.

write_finished(Dataset):-
  with_mutex(finished, write_finished_mutex(Dataset)).

write_finished_mutex(Dataset):-
  data_directory(DataDir),
  directory_file_path(DataDir, 'finished.log', File),
  setup_call_cleanup(
    open(File, append, Stream),
    (
      writeq(Stream, finished(Dataset)),
      write(Stream, '.'),
      nl(Stream)
    ),
    close(Stream)
  ).



% MESSAGES

:- multifile(prolog:message/1).

prolog:message(lod_download_start(I,Url)) -->
  ['[~D] [~w]'-[I,Url]].
prolog:message(lod_downloaded_file(Status,Messages)) -->
  prolog_status(Status),
  prolog_messages(Messages),
  [nl].
prolog:message(found_void_datadumps([])) --> !.
prolog:message(found_void_datadumps([Dataset-File|Pairs])) -->
  ['A VoID dataset was found: (~a,~a)'-[Dataset,File],nl],
  prolog:message(found_void_datadumps(Pairs)).
prolog:message(rdf_ntriples_written(File,N0)) -->
  {flag(number_of_triples_written, N1, N1 + N0)},
  ['~D triples written ('-[N0]],
  remote_file(File),
  [')'].


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

