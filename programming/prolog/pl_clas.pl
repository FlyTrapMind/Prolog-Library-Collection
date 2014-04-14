:- module(
  pl_clas,
  [
    process_options/0,
    read_options/1 % -Options:list(nvpair)
  ]
).

/** <module> Prolog command-line arguments

Support for command-line arguments given at Prolog startup.

@author Wouter Beek
@version 2014/03-2014/04
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(meta_ext)).
:- use_remote_module(generics(typecheck)).
:- use_remote_module(os(dir_infra)).

:- multifile(prolog:message//1).

%! option_specification(?OptionSpecification:list(compound)) is nondet.
% Command line options registration.
%
% @arg Short The short, i.e. one-character, name of the command-line option.
% @arg Long The long, i.e. multi-character, name of the command-line option.
% @arg Type The type of a command-line option.
%      These types are registered in module [typecheck].
% @arg Comment A human-readable description of the command-line option.

:- discontiguous(user:option_specification/1).
:- multifile(user:option_specification/1).

:- discontiguous(user:process_option/1).
:- multifile(user:process_option/1).



% Option: data.

user:option_specification([
  default(DefaultDataDir),
  help('The directory where data is stored.'),
  longflags([data]),
  opt(data),
  shortflags([d]),
  type(atom)
]):-
  absolute_file_name(project(.), ProjectDir, [file_type(directory)]),
  directory_file_path(ProjectDir, data, DefaultDataDir).

cmd_data_option(_):-
  user:file_search_path(data, _), !.
cmd_data_option(O1):-
  option(data(Dir), O1),
  nonvar(Dir),
  make_directory_path(Dir), !,
  assert(user:file_search_path(data, Dir)).
% Use the default data directory.
cmd_data_option(_):-
  absolute_file_name(
    project('.'),
    Dir1,
    [access(write),file_type(directory)]
  ),
  directory_file_path(Dir1, data, Dir2),
  make_directory_path(Dir2),
  assert(user:file_search_path(data, Dir2)).

prolog:message(incorrect_path(Dir)) -->
  ['The given value could not be resolved to a directory: ',Dir,'.~n'].



% Option:debug.

user:option_specification([
  default(false),
  help('Run in debug mode. This shows debug messages and loads debug tools.'),
  longflags([deb,debug]),
  opt(debug),
  shortflags([]),
  type(boolean)
]).

user:process_option(debug(true)):- !,
  assert(user:debug_mode),
  ensure_remote_loaded(pl(pl_debug)).
user:process_option(debug(false)).



% Option: help.

user:option_specification([
  default(false),
  help('Gives an overview of supported command-line options.'),
  longflags([help]),
  opt(help),
  shortflags([h]),
  type(boolean)
]).

user:process_option(help(true)):- !,
  findall(
    OptSpec,
    user:option_specification(OptSpec),
    OptSpecs
  ),
  opt_help(OptSpecs, Help),
  print_message(information, help(Help)),
  halt.
user:process_option(help(false)).

prolog:message(help(Help)) -->
  [Help].



% Option: version.

user:option_specification([
  default(false),
  help('Display version information.'),
  longflags([version]),
  opt(version),
  shortflags([v]),
  type(boolean)
]).

user:process_option(version(true)):- !,
  format(
    user_output,
    '  PraSem: Pragmatic Semantics for the Web of Data\n',
    []
  ),
  findall(
    Name-Description,
    user:project(Name, Description),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  forall(
    member(Name-Description, Pairs2),
    format(user_output, '    * ~w: ~w\n', [Name,Description])
  ),
  halt.
user:process_option(version(false)).



%! process_options is det.
% Reads the command-line arguments and executes those
% that are common among the PGC-using projects,

process_options:-
  read_options(O1), !,
  
  cmd_data_option(O1),
  select_option(O1, data(_), O2),
  
  exclude(unset_option, O2, O3),
  %%%order_options(O3, O4),
  maplist(user:process_option, O3).


read_options(O1):-
  findall(
    OptSpec,
    user:option_specification(OptSpec),
    OptSpecs
  ),
  opt_arguments(OptSpecs, O1, _, []).


unset_option(Option):-
  Option =.. [Name,Value],
  var(Value).

