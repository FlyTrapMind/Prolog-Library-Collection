% Debug script for PGC.

% Do not write module loads to the standard output stream.
:- set_prolog_flag(verbose_load, silent).

% On Windows 8 I have had the pleasure of swipl defaulting to the
% =text= encoding. This did _not_ process special characters correctly.
:- set_prolog_flag(encoding, utf8).

% Print code strings with their code table replacements.
:- use_module(library(portray_text)).

% We only load this in debug mode,
% since this may give information to hackers.
:- use_module(library(http/http_error)).

% Before doing much else, we start the documentation server that
% generates Web sites based on the plDoc commenting in the swipl code files.
:- use_module(library(doc_http)).
:- doc_server(4000).

% This library allows for exploiting the color and attribute facilities
% of most modern terminals using ANSI escape sequences.
% The Windows console (swipl-win) does not (yet) support ANSI (color)
% codes.
:- use_module(library(ansi_term)).

  % Write lists of ASCII numbers as strings to the terminal.
:- use_module(library(portray_text)).
:- portray_text(true).
:- set_portray_text(ellipsis, 100).

% Enforce more stringent style checking.
:- style_check(+string).
:- style_check(+charset).

% Set the swipl terminal state via PCE.
% When swipl is started from within a terminal this does not change
% anything, so this setting applies e.g. to contexts where PraSem
% would be loaded by a shortcut on the Desktop.
%:- ignore(send(@pce, show_console, iconic)).
:- ignore(send(@pce, show_console, open)).

% Run unit tests, unless compiled with optimisation turned on.
:- use_module(library(plunit)).
:- set_test_options([load(normal),run(manual)]).
%:- set_test_options([load(normal),run(make(all))]).

% Debug monitor.
% @tbd The PCE-based debug monitor in swipl is not the most versatile
%      debug tool in existence. I would like to write a Web-based version
%      at some point.
%:- use_module(library(swi_ide)).
%:- prolog_ide(debug_monitor).

:- [load].

% Load PGC documentation and debug tools.
:- use_module(server(web_modules)).
:- use_module(server(pldoc_web)).
:- use_module(server(web_console)).

%:- use_module(library(apply)).
%:- use_module(library(lists)).
%:- initialization(load_modules_for_pldoc, after_load).

%! load_modules_for_pldoc is det.
% Loads all modules in PGC for debugging purposes:
%   1. Early catching of errors.
%   2. Fully browsable plDoc.

load_modules_for_pldoc:-
  forall(
    member(
      DirectoryName,
      [
        atms,
        datasets,
        datetime,
        dcg,
        dgraph,
        generics,
        geo,
        graph_theory,
        gv,
        html,
        http,
        %ilp, % Many undefined predicates...
        lang,
        logic,
        math,
        os,
        ps,
        owl,
        rdf,
        rdf_graph,
        rdf_mt,
        rdfs,
        server,
        skos,
        sparql,
        standards,
        stat,
        svg,
        tests,
        tms,
        ugraph,
        vocabularies,
        void,
        web,
        xml,
        xsd
      ]
    ),
    (
      Spec =.. [DirectoryName, .],
      absolute_file_name(Spec, Directory, [file_type(directory)]),
      format(atom(Search), '~w/*.pl', [Directory]),
      expand_file_name(Search, Files),
      maplist(use_module, Files)
    )
  ).

