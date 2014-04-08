:- module(
  pl_clas,
  [
    process_options/0,
    read_options/1 % -Options:list(nvpair)
  ]
).

/** <module> Prolog command line arguments

Support for command line arguments given at Prolog startup.

@author Wouter Beek
@version 2014/03-2014/04
*/

:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(meta_ext)).
:- use_remote_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(optparse)).
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

cmd_data_directory(Dir1):-
  absolute_file_name(Dir1, Dir2, [access(write),file_type(directory)]), !,
  set_data_directory(Dir2).
cmd_data_directory(Dir):-
  print_message(warning, incorrect_path(Dir)).

cmd_data_option(O1):-
  option(data(Dir), O1), !,
  cmd_data_directory(Dir).
% Use the default data directory.
cmd_data_option(_):-
  set_data_directory.

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

cmd_debug_option(O1):-
  option(debug(true), O1), !,
  assert(user:debug_mode),
  ensure_remote_loaded(pl(pl_debug)).
cmd_debug_option(_).



% Option: help.

user:option_specification([
  default(false),
  help('Gives an overview of supported command-line options.'),
  longflags([help]),
  opt(help),
  shortflags([h]),
  type(boolean)
]).

cmd_help(O1):-
  option(help, O1), !,
  opt_help(O1, Help),
  print_message(information, help(Help)),
  halt.
cmd_help(_).

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

cmd_version(O1):-
  option(version, O1), !,
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
cmd_version(_).



%! process_options is det.
% Reads the command-line arguments and executes those
% that are common among the PGC-using projects,

process_options:-
gtrace,
  read_options(O1), !,
  
  % First set the data directory,
  % since other command-line arguments may depend on it being set,
  % e.g. `project=NAME`.
  cmd_data_option(O1),
  
  % Then set the debug mode and load the debug tools.
  cmd_debug_option(O1),
  
  cmd_help(O1),
  
  cmd_version(O1).


read_options(Options):-
  findall(
    OptionSpec,
    user:option_specification(OptionSpec),
    OptionSpecs
  ),
  opt_arguments(OptionSpecs, Options, _).

