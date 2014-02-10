:- module(
  ap_stat,
  [
    ap_stage_init/1, % +Potential:nonneg
    ap_stage_tick/0
  ]
).

/** <module> Auto-process statistics

Statistics for tracking the progress of automated processes.

@author Wouter Beek
@version 2013/10-2014/02
*/

:- use_module(ap(ap_dir)).
:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(server(web_modules)).

http:location(ap, root(ap), []).
:- http_handler(ap(stat), ap_stat, []).

:- initialization(web_module_add('AP Stat', ap_stat)).



ap_stat(_Request):-
  aggregate_all(
    count,
    rdfs_individual_of(_, ap:'AP'),
    N
  ),
  setoff(
    I-Column,
    (
      rdfs_individual_of(AP_Stage, ap:'AP-Stage'),
      rdf_datatype(AP_Stage, ap:name, xsd:string, Column, ap),
      rdf(AP, P, AP_Stage, ap),
      rdfs_individual_of(AP, ap:'AP'),
      rdf_global_id(rdf:LocalName, P),
      atom_concat('_', Atom, LocalName),
      atom_number(Atom, I)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Columns),
  findall(
    [Column,Succeed,Fail],
    (
      member(Column, Columns),
      aggregate_all(
        count,
        (
          rdf_datatype(AP_Stage, ap:name, xsd:string, Column, ap),
          rdf_datatype(AP_Stage, ap:status, xsd:string, succeed, ap)
        ),
        Succeed0
      ),
      Succeed is Succeed0 / N,
      aggregate_all(
        count,
        (
          rdf_datatype(AP_Stage, ap:name, xsd:string, Column, ap),
          rdf_datatype(AP_Stage, ap:status, xsd:string, error, ap)
        ),
        Fail0
      ),
      Fail is Fail0 / N
    ),
    Rows
  ),
  reply_html_page(
    app_style,
    title('Automated Processes - Statistics'),
    \html_table(
      [header_row(true)],
      `AP statistics`,
      [['Process','Succeed','Fail']|Rows]
    )
  ).



:- dynamic(stage_alias/3).

%! ap_stage_eval is det.

ap_stage_eval(AP):-
  ap_directories(AP, StageDirs),
  (
    StageDirs == []
  ->
    debug(ap_stat, 'No stage to evaluate.', [])
  ;
    length(StageDirs, Length),
    forall(
      between(0, Length, Stage),
      (
        ap_stage_eval(Alias, Stage, A, P),
        progress_bar(A, P, Bar),
        debug(ap, '[Alias:~w,Stage:~w] ~w', [Alias,Stage,Bar])
      )
    )
  ).

%! ap_stage_eval(
%!   +Alias:atom,
%!   +Stage:nonneg,
%!   -Actual:nonneg,
%!   -Potential:nonneg
%! ) is det.

ap_stage_eval(Alias, Stage, A, P):-
  atomic_list_concat([Alias,Stage,a], '_', FlagA),
  flag(FlagA, A, A),
  atomic_list_concat([Alias,Stage,p], '_', FlagP),
  flag(FlagP, P, P).



% FLAG INITIALIZATION %

ap_stage_init(Potential):-
  ap_stage_alias(Alias, Stage), !,

  % Create the potential flag.
  atomic_list_concat([Alias,Stage,p], '_', FlagP),
  flag(FlagP, _, Potential),

  % Create the actual flag.
  atomic_list_concat([Alias,Stage,a], '_', FlagA),
  flag(FlagA, _, 0),

  % Create the statistics tracking thread.
  % @tbd Not possible to set this yet.
  %option(stat_lag(Interval), O1, 10),
  intermittent_thread(
    ap_stage_eval,
    ap_stage_done(Alias, Stage),
    10000,
    _Id,
    []
  ).
ap_stage_init(_Potential):-
  debug(ap_stat, 'Unable to initialize stage.', []).



% FLAG UPDATES %

ap_stage_tick:-
  ap_stage_alias(Alias, Stage), !,
  atomic_list_concat([Alias,Stage,a], '_', Flag),
  flag(Flag, Actual, Actual + 1).
ap_stage_tick:-
  debug(ap_stat, 'Unable to tick stage.', []).



% GENERICS %

%! ap_stage_alias(-Alias:atom, -Stage:atom) is det.

ap_stage_alias(Alias, Stage):-
  thread_self(ThisThread),
  stage_alias(ThisThread, Alias, Stage).

