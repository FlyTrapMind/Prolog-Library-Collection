:- module(users_api, []).

/** <module> Users API

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@version 2009, 2013/10-2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(lib(update_passwd)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(server(dispatch)).
:- use_module(server(user_db)).
:- use_module(server(users_ui)).

:- db_add_novel(user:prolog_file_type(db, database)).

:- initialization(load_user_database).

:- http_handler(root(users_api), dispatch, []).



%! dispatch_method(+Method:oneof([delete,post]), +Request:list) is det.

dispatch_method(post, Request):-
  http_parameters(Request, [user(Name,[]),password(Password,[])]),
  http_read_data(Request, OptionsAtom, [to(atom)]),
  catch(
    atom_to_term(OptionsAtom, Options, _Bindings),
    E,
    true
  ),
  (
    var(E)
  ->
    (
      \+ user(Name)
    ->
      user_add(Name, Options),
      add_passwd('passwords', Name, Password),
      reply_json(json([ok= @true]), [width(0)])
    ;
      reply_json(json([error='Existing user']), [width(0)])
    )
  ;
    reply_json(json([error='Malformed option list']), [width(0)])
  ).
dispatch_method(delete, Request) :-
  http_parameters(Request, [user(Name,[])]),
  catch(
    (
      user_delete(Name),
      remove_passwd('passwords', Name)
    ),
    E,
    true
  ),
  (
    var(E)
  ->
    reply_json(json([ok= @true]), [width(0)])
  ;
    message_to_string(E, Msg),
    reply_json(json([error=Msg]), [width(0)])
  ).
% Returns the contents of the user file in JSON.
dispatch_method(get, Request):-
  http_parameters(Request, [user(User,[default('_')])]),
  (
    User == '_'
  ->
    % Return the properties for all users.
    list_users(_, List)
  ;
    % Return the properties for a specific user.
    list_users(User, List)
  ),
  reply_json(json(List), [width(0)]).

%! list_users(?User:atom, -List:list(nvpair)) is det.
% Returns pairs of users and their properties.

list_users(User, List) :-
  findall(
    User=Properties,
    (
      user(User),
      findall(
        Property,
        (
          user_property(User, Prop),
          term_to_atom(Prop, Property)
        ),
        Properties
      )
    ),
    List
  ).

load_user_database:-
  absolute_file_name(
    project(user),
    File,
    [access(write),file_type(database)]
  ),
  set_user_database(File).

