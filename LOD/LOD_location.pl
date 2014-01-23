:- module(
  'LOD_location',
  [
    'LOD_location'/2, % +Prefix:atom
                      % -Location:atom
    'LOD_register_location'/2 % +Prefix:atom
                              % +Location:atom
  ]
).

/** <module> LOD location

Support for Web locations that store LOD descriptions.

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(db_ext)).
:- use_module(xml(xml_namespace)).

:- dynamic('LOD_location_'/2).



'LOD_register_location'(Prefix, Location):-
  db_add_novel('LOD_location_'(Prefix, Location)).

% 'LOD_location'(+Prefix:atom, -Location:iri) is det.
% Used in case the XML namespace does not denote
%  a machine-readable description of the vocabulary, e.g. Dublin Core.

'LOD_location'(Prefix, Location):-
  'LOD_location_'(Prefix, Location), !.
'LOD_location'(Prefix, Location):-
  xml_current_namespace(Prefix, Location).

