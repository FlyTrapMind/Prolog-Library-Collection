:- module(
  gnu_plot,
  [
    gnu_plot/3 % +ScriptSpec:compound
               % +Arguments:list(nvpair)
               % +Options:list(nvpair)
  ]
).

/** <module> GNU Plot

Support for calling GNU Plot.

@author Wouter Beek
@version 2015/04
*/

:- use_module(library(dcg/basics)).

:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_quote)).
:- use_module(plc(process/process_ext)).

:- predicate_options(gnu_plot/3, 3, [
  pass_to(handle_process/3, 3)
]).





%! gnu_plot(
%!   +ScriptSpec:compound,
%!   +Arguments:list(nvpair),
%!   +Options:list(nvpair)
%! ) is det.

gnu_plot(ScriptSpec, Args, Options):-
  absolute_file_name(ScriptSpec, Script, [access(execute),extensions([plt])]),
  atom_phrase(gnu_plot_args(Args), Arg),
  handle_process(
    gnuplot,
    ['-e',Arg,file(Script)],
    Options
  ).

gnu_plot_args(Args) -->
  quoted(gnu_plot_args0(Args)).

gnu_plot_args0([]).
gnu_plot_args0([K=V|T]) -->
  gnu_plot_arg(K, V),
  gnu_plot_args0(T).

gnu_plot_arg(K, V) -->
  atom(K),
  "='",
  atom(V),
  "';".

