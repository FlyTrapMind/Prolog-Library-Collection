:- module(
  update_password,
  [
    add_password/3, % +File
                    % +User
                    % +Password
    passwords_file/1, % -File:atom
    remove_password/2 % +File
                      % +User
  ]
).

/** <module> Update passwords

Predicates that update the passwords file.

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@version 2009, 2013/10, 2013/12
*/

:- use_module(generics(db_ext)).
:- use_module(library(crypt)).
:- use_module(os(os_ext)).

:- dynamic(authenticate:password/3).

:- db_add_novel(user:prolog_file_type(db, database)).

:- initialization(init_passwords).



add_password(File, User, Password):-
  absolute_file_name(File, Path, [access(write)]),
  retractall(authenticate:password(User, _OldPath, _OldPasswd)),
  crypt(Password, EncryptedPasswd),
  assert(authenticate:password(User, Path, EncryptedPasswd)),
  open(File, write, Stream, [lock(write)]),
  forall(
    authenticate:password(User0, _Path, EPasswd0),
    format(Stream, '~p:~@\n', [User0, format(EPasswd0)])
  ),
  close(Stream).

init_passwords:-
  passwrods_file(_File), !.
init_passwords:-
  absolute_file_name(
    project(passwords),
    File,
    [access(write),file_type(database)]
  ),
  touch(File).

%! passwords_file(File) is det.
% Returns the passwords file.

passwords_file(File):-
  absolute_file_name(
    project(passwords),
    File,
    [access(read),file_errors(error),file_type(database)]
  ).

remove_password(File, User):-
  retractall(authenticate:password(User, _OldPath, _OldPasswd)),
  open(File, write, Stream, [lock(write)]),
  forall(
    authenticate:password(User0, _Path, EPasswd0),
    format(Stream, '~p:~@\n', [User0, format(EPasswd0)])
  ),
  close(Stream).

