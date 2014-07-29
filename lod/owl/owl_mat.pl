:- module(
  owl_mat,
  [
    owl_materialize/1, % +Directory:atom
    owl_materialize/2 % +FromDirectory:atom
                      % +ToDirectory:atom
  ]
).

/** <module> OWL materialization

OWL materialization support.

@author Wouter Beek
@version 2013/11-2013/12, 2014/07
*/

:- use_module(generics(db_ext)).
:- use_module(os(java_ext)).

:- db_add_novel(user:prolog_file_type(jar, jar)).



%! owl_materialize(+Directory:atom) is det.

owl_materialize(Dir):-
  owl_materialize(Dir, Dir).

%! owl_materialize(+FromDirectory:atom, +ToDirectory:atom) is det.

owl_materialize(FromDir, ToDir):-
  absolute_file_name(
    iotw('iotw-0.0.1-SNAPSHOT'),
    JarFile,
    [access(read),file_type(jar)]
  ),
  run_jar(JarFile, [file(FromDir),file(ToDir)]).

