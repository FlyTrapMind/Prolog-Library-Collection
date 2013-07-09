:- module(
  rdf_datatype,
  [
    rdf_convert_datatype/4, % +FromDatatype:uri
                            % +FromValue
                            % +ToDatatype:uri
                            % -ToValue
    rdf_datatype/2, % ?DatatypeName:atom
                    % ?Datatype:uri
    rdf_datatype/3, % ?Datatype:oneof([atom,uri])
                    % ?LexicalValue
                    % ?CanonicalValue
    rdf_datatype/4 % ?DatatypeName:atom
                   % ?LexicalValue
                   % ?Datatype:uri
                   % ?CanonicalValue
  ]
).

/** <module> RDF datatype

RDF datatypes. The XML Schema datatype plus custom defined datatypes.

This module uses the =|img|= search file name for finding images.

@author Wouter Beek
@version 2013/03-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(typecheck)).
:- use_module(os(file_ext)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xml_schema_datatypes)).

% Temporary namespace for the image datatype.
:- xml_register_namespace(prasem, 'http://www.wouterbeek.com/prasem.owl#').

:- db_add_novel(user:file_search_path(file, project('Files'))).
:- db_add_novel(user:file_search_path(img, project('Images'))).

% Register the supported image file types.
% These are shared with module HTML.
:- dynamic(user:image_file_type/1).
:- multifile(user:image_file_type/1).
:- db_add_novel(user:prolog_file_type(jpeg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpg, jpeg)).
:- db_add_novel(user:image_file_type(jpeg)).
:- db_add_novel(user:prolog_file_type(png, png)).
:- db_add_novel(user:image_file_type(png)).

:- rdf_meta(rdf_convert_datatype(r,+,r,-)).
:- rdf_meta(rdf_datatype(?,r)).
:- rdf_meta(rdf_datatype_(?,r)).
:- rdf_meta(rdf_datatype(r,?,?)).
:- rdf_meta(rdf_datatype(?,?,r,?)).



%! rdf_convert_datatype(
%!   ?FromDatatype:oneof([atom,uri]),
%!   ?FromValue,
%!   ?ToDatatype:oneof([atom,uri]),
%!   ?ToValue
%! ) is nondet.

rdf_convert_datatype(FromDatatype, FromValue, ToDatatype, ToValue):-
  rdf_datatype(FromDatatype, FromValue, Canonical),
  rdf_datatype(ToDatatype, ToValue, Canonical).

%! rdf_datatype(?DatatypeName:atom, ?Datatype:uri) is nondet.
% Translations between datatype names and datatype URIs.
%
% @arg DatatypeName The atomic name of an XML Schema datatype.
% @arg Datatype The URI of an XML Schema datatype.

rdf_datatype(DatatypeName, Datatype):-
  var(DatatypeName), var(Datatype), !,
  rdf_datatype_(DatatypeName, Datatype).
rdf_datatype(DatatypeName, Datatype):-
  once(rdf_datatype_(DatatypeName, Datatype)).

rdf_datatype_(image, prasem:image).
rdf_datatype_(DatatypeName, Datatype):-
  xmls_datatype(DatatypeName, Datatype).

%! rdf_datatype(
%!   ?Datatype:oneof([atom,uri]),
%!   ?LexicalValue,
%!   ?CanonicalValue
%! ) is nondet.
% Warapper for rdf_datatype/4 allowing both datatype names and datatype URIs.
%
% @arg Datatype Either the atomic name of a datatype or a URI
%        representing a datatype.
% @arg LexicalValue
% @arg CanonicalValue

rdf_datatype(Datatype, LexicalValue, CanonicalValue):-
  is_uri(Datatype), !,
  rdf_datatype(_DatatypeName, LexicalValue, Datatype, CanonicalValue).
rdf_datatype(DatatypeName, LexicalValue, CanonicalValue):-
  rdf_datatype(DatatypeName, LexicalValue, _Datatype, CanonicalValue).

%! rdf_datatype(
%!   ?DatatypeName:atom,
%!   ?LexicalValue,
%!   ?Datatype:uri,
%!   ?CanonicalValue
%! ) is nondet.
% Warapper for rdf_datatype/4 allowing both datatype names and datatype URIs.
%
% @arg DatatypeName The atomic name of a datatype.
% @arg LexicalValue
% @arg Datatype The URI of a datatype.
% @arg CanonicalValue

rdf_datatype('XMLLiteral', Value, prasem:'XMLLiteral', Value):- !,
  atom(Value).
rdf_datatype(file, LexicalValue, prasem:file, CanonicalValue):-
  nonvar(CanonicalValue), !,
  absolute_file_name(file(CanonicalValue), LexicalValue, [access(read)]).
rdf_datatype(file, LexicalValue, prasem:file, CanonicalValue):-
  nonvar(LexicalValue), !,
  file_base_name(LexicalValue, CanonicalValue).
rdf_datatype(image, LexicalValue, prasem:image, CanonicalValue):-
  nonvar(CanonicalValue), !,
  file_name_type(_Base, FileType, CanonicalValue),
  user:image_file_type(FileType),
  absolute_file_name(img(CanonicalValue), LexicalValue, [access(read)]),
  % Do not backtrack on image file types.
  !.
rdf_datatype(image, LexicalValue, prasem:image, CanonicalValue):-
  nonvar(LexicalValue), !,
  file_base_name(LexicalValue, CanonicalValue).
rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue):-
  xmls_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue).
