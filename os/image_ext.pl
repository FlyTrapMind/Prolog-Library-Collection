:- module(
  image_ext,
  [
    image_dimensions/3 % +File:atom
                       % -Width:float
                       % -Height:float
  ]
).

/** <module> Image extensions

Support for image files.

@author Wouter Beek
@version 2014/03, 2014/05
*/

:- use_module(library(process)).
:- use_module(library(pure_input)).

:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(os(io_ext)).

% Dynamic multifile predicate for registering
% which Prolog file types denote images.
:- dynamic(user:image_file_type/1).
:- multifile(user:image_file_type/1).

% Register JPG/JPEG.
user:prolog_file_type(jpeg, jpeg).
user:prolog_file_type(jpg, jpeg).
user:image_file_type(jpeg).

% Register PNG.
user:prolog_file_type(png, png).
user:image_file_type(png).



%! image_dimensions(+File:atom, -Width:float, -Height:float) is det.

image_dimensions(File, Width, Height):-
  process_create(path(identify), [file(File)], [stdout(pipe(Stream))]),
  stream_to_atom(Stream, Atom),
  dcg_phrase(image_dimensions(File, Width, Height), Atom),
  close(Stream).


%! image_dimensions(+File:atom, -Width:float, -Height:float)// is det.

image_dimensions(File, Width, Height) -->
  atom(File),
  ` JPEG `,
  integer(Width),
  `x`,
  integer(Height),
  dcg_done.

