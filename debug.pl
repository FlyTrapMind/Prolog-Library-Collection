% Debug script for PGC.

% On Windows 8 I have had the pleasure of swipl defaulting to the
% =text= encoding. This did _not_ process special characters correctly.
:- set_prolog_flag(encoding, utf8).

% Print code strings with their code table replacements.
:- use_module(library(portray_text)).

% Do not write module loads to the standard output stream.
:- set_prolog_flag(verbose_load, silent).

% Before doing much else, we start the documentation server that
% generates Web sites based on the plDoc commenting in the swipl code files.
:- use_module(library(http/http_path)).
:- assert(http:location(pldoc, root(help), [priority(10)])).

:- use_module(library(pldoc)).
:- doc_server(2222, [edit(true)]).

% This library allows for exploiting the color and attribute facilities
% of most modern terminals using ANSI escape sequences.
% The Windows console (swipl-win) does not (yet) support ANSI (color)
% codes.
:- use_module(library(ansi_term)).

  % Write lists of ASCII numbers as strings to the terminal.
:- use_module(library(portray_text)).
:- portray_text(true).

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
:- set_test_options([load(normal),run(make(all))]).

% Debug monitor.
% @tbd The PCE-based debug monitor in swipl is not the most versatile
%      debug tool in existence. I would like to write a Web-based version
%      at some point.
%:- use_module(library(swi_ide)).
%:- prolog_ide(debug_monitor).

:- [load].

:- use_module(library(apply)).
:- use_module(library(lists)).

:- initialization(load_modules_for_pldoc).

%! load_modules_for_pldoc is det.
% Loads all modules in PGC for debugging purposes:
%   1. Early catching of errors.
%   2. Fully browsable plDoc.

load_modules_for_pldoc:-
  forall(
    member(
      DirectoryName,
      [
        datasets,
        datetime,
        dbnl,
        dcg,
        dgraph,
        generics,
        geo,
        graph_theory,
        gv,
        html,
        http,
        %ilp,
        lang,
        logic,
        math,
        os,
        owl,
        rdf,
        rdf_graph,
        rdfs,
        server,
        skos,
        sparql,
        standards,
        svg,
        tests,
        tms,
        ugraph,
        vocabularies,
        xml
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
