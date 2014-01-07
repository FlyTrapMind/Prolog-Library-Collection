:- module(
  html_image,
  [
    html_image//2, % +IMG_Options:list(nvpair)
                   % +File:atom
    html_image//4 % +DIV_Options:list(nvpair)
                  % +Description:atom
                  % +IMG_Options:list(nvpair)
                  % +File:atom
  ]
).

/** <module> HTML image

Support for the HTML image tag.

@author Wouter Beek
@version 2012/09-2013/06, 2013/10
*/

:- use_module(generics(db_ext)).
:- use_module(generics(option_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(os(file_ext)).

% Register the supported image file types.
% These are shared with module RDF_DATATYPE.
:- dynamic(user:image_file_type/1).
:- multifile(user:image_file_type/1).

:- db_add_novel(user:prolog_file_type(jpeg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpg,  jpeg)).
:- db_add_novel(user:image_file_type( jpeg      )).
:- db_add_novel(user:prolog_file_type(png,  png )).
:- db_add_novel(user:image_file_type( png       )).



%! html_image(+IMG_Options:list(nvpair), +File:atom) is det.
% @see Wrapper for html_image//4.

html_image(IMG_O1, File) -->
  {
    % Make sure the file has a supported image file type.
    file_name_type(_Base, Type, File),
    user:image_file_type(Type),
    http_absolute_location(img(File), RelativeURI, [])
  },

  html(
    % Make the image clickable, linking to the image file.
    a(
      [href=RelativeURI,target='_blank'],
      % The image itself, using the description as alternative text.
      img([class=image_thumbnail,src=RelativeURI|IMG_O1], [])
    )
  ).

%! html_image(
%!   +DIV_Options:list(nvpair),
%!   +Description:atom,
%!   +IMG_Options:list(nvpair),
%!   +File:atom
%! ) is det.
% Constructs an IMG element.
%
% @arg DIV_Options A list of name-value pairs that apply to
%        the =div= element.
% @arg Description An atomic description of the image.
% @arg IMG_Options A list of name-value pairs that apply to
%        the image element.
% @arg File The atomic name of the image file.

html_image(DIV_O1, Description, IMG_O1, File) -->
  % If no alternative text has been set, then we use the description text.
  {
    default_option(IMG_O1, alt, Description, _Alt, IMG_O2),
    merge_options(DIV_O1, [class(image)], DIV_O2)
  },
  html(
    % Construe the DIV containing the image, the link, and the description.
    div(
      DIV_O2,
      [
        \html_image(IMG_O2, File),
        % The DIV containing the image description.
        div(class=image_description,Description)
      ]
    )
  ).

