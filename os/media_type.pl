:- module(
  media_type,
  [
    media_type_string/2, % ?MediaType:compound
                         % ?String:string) is det.
    remove_media_type_encoding/2 % +MediaTypeEncoding:string
                                 % -MediaType:string
  ]
).

/** <module> Media type

Support for media types.

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(error)).



%! media_type_string(+MediaType:compound, -String:string) is det.
%! media_type_string(-MediaType:compound, +String:string) is det.
% Converts between the compound term and the string representation
% of media types.
%
% @throws type_error if String is instantiated to a non-string.
% @throws instantiation_error if neither MediaType nor String is instantiated.

media_type_string(MediaType, String):-
  ground(MediaType), !,
  MediaType = media_type(Type,Subtype),
  format(string(String), '~s/~s', [Type,Subtype]).
media_type_string(MediaType, StringEnc):-
  string(StringEnc), !,
  remove_media_type_encoding(StringEnc, String),
  split_string(String, "/", " ", [Type,Subtype]),
  MediaType = media_type(Type,Subtype).
media_type_string(_, String):-
  nonvar(String), !,
  type_error(string, String).
media_type_string(MediaType, _):-
  instantiation_error(MediaType).


%! remove_media_type_encoding(
%!   +MediaTypeEncoding:string,
%!   -MediaType:string
%! ) is det.

remove_media_type_encoding(MediaTypeEncoding, MediaType):-
  split_string(MediaTypeEncoding, ";", " ", [MediaType|_]), !.
remove_media_type_encoding(MediaType, MediaType).
