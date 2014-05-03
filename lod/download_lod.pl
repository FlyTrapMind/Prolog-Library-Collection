:- module(
  download_lod,
  [
    download_lod/2 % +Directory:or([atom,compound])
                   % +Input
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

:- thread_local(seen/1).
:- thread_local(todo/1).

%! tmp(?Dataset:iri, ?Triple:compound) is nondet.

:- thread_local(tmp/2).



%! download_lod(+Directory:atom, +Input) is det.

download_lod(Dir, Pairs1):-
  is_list(Pairs1), !,
  flag(number_of_triples_written, _, 0),

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
  group_pairs_by_key(Pairs2, Pairs3),
  
  length(Pairs3, Length), %DEB
  writeln(Length), %DEB

  % Construct the set of goals.
  findall(
    lod_download_authority(I, Dir, Pair),
    nth0(I, Pairs3, Pair),
    Goals
  ),

  % Run the goals in threads.
  % The number of threads can be given as an option.
  rdf_retractall(_, _, _),
  run_goals_in_threads(Goals).
download_lod(Dir, Dataset-Location):- !,
  download_lod(Dir, [Dataset-Location]).
download_lod(Dir, Location):-
  download_lod(Dir, Location-Location).


%! lod_download_authority(
%!   +Index:nonneg,
%!   +DataDirectory:compound,
%!   +UrlAuthority:pair(atom,list(pair(atom)))
%! ) is det.
% Downloads the datasets at the given authority.
%
% An authority is represented as a pair of an authority name
% and a list of CKAN resources that -- according to the metadata --
% reside at that authority.

lod_download_authority(I, Dir, _-Pairs):-
  maplist(lod_download_resource(I, Dir), Pairs).


%! lod_download_resource(+Index:nonneg, +DataDirectory:compound, +Pair:pair(atom)) is det.
% Processes the given CKAN resource.

lod_download_resource(I, Dir, Dataset-Iri):-
  % Count the number of processed datasets.
  lod_download_url(I, Dir, Dataset, Iri).


%! lod_download_url(
%!   +Index:nonneg,
%!   +DataDirectory:compound,
%!   +Dataset:iri,
%!   +Iri:iri
%! ) is det.
% Processes the given CKAN IRI.

% This CKAN resource was already processed in the past.
lod_download_url(_, Dir, Dataset, _):-
  lod_resource_path(Dir, Dataset, 'messages.nt', RemotePath),
  exists_remote_file(RemotePath), !.
lod_download_url(I, Dir, Dataset, Iri):-
  % Start message.
  print_message(informational, lod_download_start(I,Iri)),

  % CKAN URLs are sometimes non-URL IRIs.
  uri_iri_logged(Dataset, Url, Iri),

  % The CKAN resource is processed.
  % We log the status of the processing,
  % and all warnings and informational messages
  % that were given during the processing.
  run_collect_messages(
    lod_process_resource(Dir, Dataset, Url),
    Status,
    Messages
  ),

  % Store log messages.
  log_status(Dataset, Status),
  maplist(log_message(Dataset), Messages),
  store_messages_to_file(Dir, Dataset),

  % End message.
  print_message(informational, lod_download_end(Status,Messages)).


%! lod_process_resource(+DataDirectory:atom, +Dataset:iri, +Url:atom) is det.

lod_process_resource(Dir, Dataset, Url):-
  register_input(Url),

  % Make sure the remote directory exists.
  url_flat_directory(Dir, Dataset, UrlDir),
  make_remote_directory_path(UrlDir),

  lod_process_files(Dir, Dataset).


%! lod_process_files(+DataDirectory:atom, +Dataset:iri) is det.

lod_process_files(Dir, Dataset):-
  pick_input(Input), !,
  lod_process_file(Dir, Dataset, Input),
  lod_process_files(Dir, Dataset).
% No more inputs to pick.
lod_process_files(_, _).


%! lod_process_file(
%!   +DataDirectory:atom,
%!   +Dataset:atom,
%!   +Input:dict
%! ) is det.

lod_process_file(Dir, Dataset, Input):-
  unpack(Input, Read, Location),
  rdf_process_file(Dir, Dataset, Read, Location),
  % Unpack the next entry!
  fail.
lod_process_file(_, _, _).


%! rdf_process_file(
%!   +DataDirectory:atom,
%!   +Dataset:atom,
%!   +Read:stream,
%!   +Location:dict
%! ) is det.

rdf_process_file(Dir, Dataset, Read, Location):-
  call_cleanup(
    rdf_transaction(
      rdf_process_file_call(Dir, Dataset, Read, Location),
      _,
      [snapshot(true)]
    ),
    close(Read)
  ).


%! rdf_process_file_call(
%!   +DataDirectory:or([atom,compound]),
%!   +Dataset:iri,
%!   +Read:stream,
%!   +Location:dict
%! ) is det.

rdf_process_file_call(Dir, Dataset, Read, Location):-
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
  assert_number_of_triples(Dataset),
  assert_void_triples(Dataset),
  
  % Save triples using the N-Triples serialization format.
  lod_resource_path(Dir, Dataset, 'input.nt.gz', Path),
  setup_call_cleanup(
    remote_open(Path, append, Write, [filter(gzip)]),
    rdf_ntriples_write(Write, [bnode_base(Base),number_of_triples(T2)]),
    close(Write)
  ),
  print_message(informational, rdf_ntriples_written(Path,T2)),

  % Log the number of triples after deduplication.
  assert(
    tmp(
      Dataset,
      rdf(Dataset, void:triples, literal(type(xsd:integer,T2)))
    )
  ),
  flag(number_of_triples_written, X, X),
  format(current_output, '~D --[deduplicate]--> ~D (All: ~D)~n', [T1,T2,X]),

  % Make sure any VoID datadumps are considered as well.
  register_void_inputs.



% HELPERS

%! assert_number_of_triples(+Dataset:iri) is det.

assert_number_of_triples(Dataset):-
  % Log the number of triples before deduplication.
  aggregate_all(
    count,
    rdf(_, _, _, _),
    N
  ),
  assert(
    tmp(
      Dataset,
      rdf(Dataset, ap:duplicate_triples, literal(type(xsd:integer,N)))
    )
  ).


%! assert_void_triples(+Dataset:iri) is det.

assert_void_triples(Dataset):-
  aggregate_all
    set(P),
    (
      rdf_current_predicate(P),
      rdf_current_id(void:_, P)
    ),
    Ps
  ),
  forall(
    (
      member(P, Ps),
      rdf(S, P, O)
    ),
    assert(tmp(Dataset, rdf(S, P, O)))
  ).


%! lod_resource_path(
%!   +DataDirectory:compound,
%!   +Dataset:iri,
%!   +File:atom,
%!   -Path:atom
%! ) is det.

lod_resource_path(
  remote(User,Machine,Dir),
  Dataset,
  File,
  remote(User,Machine,Path)
):- !,
  lod_resource_path(Dir, Dataset, File, Path).
lod_resource_path(Dir, Dataset, File, Path):-
  url_flat_directory(Dir, Dataset, UrlDir),
  directory_file_path(UrlDir, File, Path).


%! log_message(+Dataset:iri, +Message:compound) is det.

log_message(Dataset, Message):-
  with_output_to(atom(String), write_canonical_blobs(Message)),
  assert(
    tmp(
      Dataset,
      rdf(Dataset, ap:message, literal(type(xsd:string,String)))
    )
  ).


%! log_status(+Dataset:iri, +Exception:compound) is det.

log_status(_, false):- !.
log_status(_, true):- !.
log_status(Dataset, exception(Error)):-
  with_output_to(atom(String), write_canonical_blobs(Error)),
  assert(
    tmp(
      Dataset,
      rdf(Dataset, ap:status, literal(type(xsd:string,String)))
    )
  ).


%! pick_input(-Input) is nondet.

pick_input(Input):-
  retract(todo(Input)).


%! register_input(+Input:atom) is det.

register_input(Input):-
  assert(todo(Input)),
  assert(seen(Input)).


%! register_void_inputs is det.

register_void_inputs:-
  % Add all VoID datadumps to the TODO list.
  aggregate_all(
    set(VoidTodo),
    (
      rdf(_, void:dataDump, VoidTodo),
      \+ seen(VoidTodo)
    ),
    VoidTodos
  ),
  print_message(informational, found_voids(VoidTodos)),
  maplist(register_input, VoidTodos).


%! run_goals_in_threads(:Goals) is det.

run_goals_in_threads(Goals):-
  once(read_options(O1)),
  option(threads(NumberOfThreads), O1),
  (
    NumberOfThreads == 1
  ->
    maplist(call, Goals)
  ;
    concurrent(NumberOfThreads, Goals, [])
  ).


%! store_messages_to_file(+DataDirectory:compound, +Dataset:iri) is det.

store_messages_to_file(Dir, Dataset):-
  rdf_transaction(
    (
      forall(
        tmp(Dataset, rdf(S,P,O)),
        rdf_assert_triple(S, P, O)
      ),
      lod_resource_path(Dir, Dataset, 'messages.nt', Path),
      setup_call_cleanup(
        remote_open(Path, write, Write, [filter(gzip)]),
        rdf_ntriples_write(Write, []),
        close(Write)
      )
    ),
    _,
    [snapshot(true)]
  ).


rdf_assert_triple(S1, P1, O1):-
  maplist(rdf_convert_term, [S1,P1,O1], [S2,P2,O2]),
  rdf_assert(S2, P2, O2).

rdf_convert_term(literal(type(D1,V1)), literal(type(D2,V2))):- !,
  rdf_global_id(D1, D2),
  format(atom(V2), '~w', [V1]).
rdf_convert_term(X:Y, Z):- !,
  rdf_global_id(X:Y, Z).
rdf_convert_term(T, T).


%! uri_iri_logged(+Dataset:iri, +Url:atom, +Iri:atom) is det.

uri_iri_logged(Dataset, Url, Iri):-
  url_iri(Url, Iri),
  assert(tmp(Dataset, rdf(Dataset, ap:iri, Iri))),
  assert(tmp(Dataset, rdf(Dataset, ap:url, Url))).



% MESSAGES

:- multifile(prolog:message/1).

prolog:message(lod_download_start(I,Url)) -->
  ['[~D] [~w]'-[I,Url],nl].
prolog:message(lod_download_end(Status,Messages)) -->
  prolog_status(Status),
  prolog_messages(Messages),
  [nl].
prolog:message(found_voids([])) --> !.
prolog:message(found_voids([H|T])) -->
  ['A VoID dataset was found: ',H,nl],
  prolog:message(found_voids(T)).
prolog:message(rdf_ntriples_written(File,N0)) -->
  {
    flag(number_of_triples_written, N1, N1 + N0),
    N2 is N1 + N0
  },
  ['~D triples written ('-[N0]],
  remote_file(File),
  ['); ~D triples written in total.'-[N2]].


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

