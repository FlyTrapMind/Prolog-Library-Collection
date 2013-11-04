:- module(
  web_modules,
  [
    web_module/2, % ?ExternalName:atom
                  % ?InternalName:atom
    web_module_add/2, % +ExternalName:atom
                      % +InternalName:atom
    web_module_delete/1, % +InternalName:atom
    web_modules/1 % -Pairs:list(pair(atom,atom))
  ]
).

/** <module> Web modules

Registration infrastructure for Web modules.

@author Wouter Beek
@version 2012/10, 2013/02-2013/06, 2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(library(error)).
:- use_module(library(persistency)). % Persistency declaration.

:- db_add_novel(user:prolog_file_type(db, database)).

%! web_module_db(?ExternalName:atom, ?InternalName:atom) is nondet.

:- persistent(web_module_db(external_name:atom,internal_name:atom)).

:- initialization((
  absolute_file_name(
    project(web_modules),
    File,
    [access(write),file_type(database)]
  ),
  web_module_attach(File)
)).



web_module_attach(File):-
  db_attach(File, []).

% ! web_module(?ExternalName:atom, ?InternalName:atom) is nondet.
% Modules that are currently registered with the web console.
% Only web modules can be sensibly registered, since the web console
% looks for =|_web|=-predicates exclusively. Web modules must be
% registered before their web methods can be accessed from the web
% console.
%
% @param ExternalName The atomic name of a Prolog module for
%        intended for human consumption.
% @param InternalName The atomic name of a Prolog module.
%        intended for internal use.

web_module(ExternalName, InternalName):-
  with_mutex(web_modules, web_module_db(ExternalName, InternalName)).

%! web_module_add(+ExternalName:atom, +InternalName:atom) is det.
% Registers the given module for the web console.
% If the module is a web module, i.e. contains =|_web|=-predicates,
% then these can now be accessed from the web console.
%
% @param Module The atomic name of a module that is used internally.
% @param Name A human-readable name for the module.
%        This is displayed in the Web application.

% The module is already registered, do nothing.
web_module_add(_ExternalName1, InternalName):-
  with_mutex(web_modules, web_module_db(_ExternalName2, InternalName)), !.
% Register the module.
web_module_add(ExternalName, InternalName):-
  % The module must already be loaded.
  with_mutex(
    web_modules,
    (
      current_module(InternalName),
      assert_web_module_db(ExternalName, InternalName)
    )
  ).

%! web_module_delete(+InternalName:atom) is det.
% Deregisters the given module. This means that the =|_web|=-predicates
% of this module will no longer be accessible from the web console.

web_module_delete(InternalName):-
  with_mutex(
    web_modules,
    (
      web_module_db(_ExternalName1, InternalName), !,
      retractall_web_module_db(_ExternalName2, InternalName)
    )
  ).
web_module_delete(InternalName):-
  existence_error(web_module, InternalName).

%! web_modules(-Pairs:list(pair(atom,atom))) is det.
% Returns all modules that are currently registered with the web console.
%
% @param Pairs A list of pairs of atomic modules name.
%	 The first is the internal name, the second is the external
%	 name.

web_modules(Pairs2):-
  findall(
    ExternalName-InternalName,
    web_module(ExternalName, InternalName),
    Pairs1
  ),
  keysort(Pairs1, Pairs2).

