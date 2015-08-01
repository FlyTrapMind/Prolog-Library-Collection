:- module(
  image_ext,
  [
    image_dimensions/3, % +File:atom
                        % -Width:float
                        % -Height:float
    image_file_extension/1, % ?FileExtension:atom
    is_image_file/1 % +File:atom
  ]
).

/** <module> Image extensions

Support for image files.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(process_ext)).
:- use_module(library(readutil)).

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(bmp, bmp).
user:prolog_file_type(bmp, image).
user:prolog_file_type(gif, gif).
user:prolog_file_type(gif, image).
user:prolog_file_type(jpeg, jpeg).
user:prolog_file_type(jpeg, image).
user:prolog_file_type(jpg, jpeg).
user:prolog_file_type(jpg, image).
user:prolog_file_type(png, png).
user:prolog_file_type(png, image).

:- dynamic(user:module_uses/2).
:- multifile(user:module_uses/2).

user:module_uses(image_ext, program(identify)).



%! image_dimensions(+File:atom, -Width:float, -Height:float) is det.
% Requires ImageMagick.

image_dimensions(File, Width, Height):-
  handle_process(
    identify,
    [file(File)],
    [output_goal(phrase_from_stream(image_dimensions0(File, Width, Height)))]
  ).

%! image_dimensions0(+File:atom, -Width:float, -Height:float)// is det.

image_dimensions(File, Width, Height) -->
  atom(File),
  " ",
  atom(_FileType),
  " ",
  integer(Width),
  "x",
  integer(Height),
  dcg_done.


%! image_file_extension(+ImageFileExtension) is semidet.
%! image_file_extension(-ImageFileExtension) is nondet.

image_file_extension(Ext):-
  nonvar(Ext), !,
  image_file_extension0(Ext), !.
image_file_extension(Ext):-
  image_file_extension0(Ext).

image_file_extension0(Ext):-
  user:prolog_file_type(Ext, image).


%! is_image_file(+File:atom) is semidet.
% Determines whether a file stores an image or not based on
% its file name extension.

is_image_file(File):-
  file_name_extension(_, Ext, File),
  image_file_extension(Ext).
