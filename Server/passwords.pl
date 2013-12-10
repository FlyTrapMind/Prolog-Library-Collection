:- module(
  passwords,
  [
    add_password/2, % +User:atom
                    % +Password:atom
    password_file/1, % -File:atom
    remove_password/1 % +User:atom
  ]
).

/** <module> Passwords

@author Jan Wielemaker
@author Torbjörn Lager
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(library(crypt)).
:- use_module(os(os_ext)).

:- db_add_novel(user:prolog_file_type(db, database)).

:- dynamic(authenticate:password/3).

:- initialization(init_password_db).



add_password(User, Password):-
  password_file(File),
  add_password(File, User, Password).

add_password(File, User, Password):-
  access_file(File, write),
  retractall(authenticate:password(User, _OldPath, _OldPasswd)),
  crypt(Password, EncryptedPasswd),
  assert(authenticate:password(User, File, EncryptedPasswd)),
  setup_call_cleanup(
    open(File, write, Stream, [lock(write)]),
    forall(
      authenticate:password(User0, _Path, EPasswd0),
      format(Stream, '~p:~@\n', [User0, format(EPasswd0)])
    ),
    close(Stream)
  ).

init_password_db:-
  (
    password_file(_File), !
  ;
    absolute_file_name(
      project(passwords),
      File,
      [access(write),file_type(database)]
    ),
    touch(File)
  ).

password_file(File):-
  absolute_file_name(
    project(passwords),
    File,
    [access(write),file_type(database)]
  ).

remove_password(User):-
  password_file(File),
  remove_password(File, User).

remove_password(File, User):-
  access_file(File, write),
  retractall(authenticate:password(User, _OldPath, _OldPasswd)),
  setup_call_cleanup(
    open(File, write, Stream, [lock(write)]),
    forall(
      authenticate:password(User0, _Path, EPasswd0),
      format(Stream, '~p:~@\n', [User0,format(EPasswd0)])
    ),
    close(Stream)
  ).

