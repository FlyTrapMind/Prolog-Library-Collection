:- module(
  'LOD_location',
  [
% HEADER
    'LOD_header'/3, % +Prefix:atom
                    % ?Name:atom
                    % ?Value:atom
    'LOD_register_header'/3, % +Prefix:atom
                             % +Name:atom
                             % +Value:atom
% LOCATION
    'LOD_location'/2, % +Prefix:atom
                      % -Location:atom
    'LOD_register_location'/2 % +Prefix:atom
                              % +Location:atom
  ]
).

/** <module> LOD location

Support for Web locations that store LOD descriptions.

@author Wouter Beek
@version 2014/01-2014/02
*/

:- use_module(generics(db_ext)).
:- use_module(xml(xml_namespace)).

:- dynamic(lod_header/3).
:- dynamic(lod_location/2).



% 'LOD_header'(+Prefix:atom, ?Name:atom, ?Value:atom) is nondet.

'LOD_header'(Prefix, Name, Value):-
  lod_header(Prefix, Name, Value).

'LOD_register_header'(Prefix, Name, Value):-
  db_add_novel(lod_header(Prefix, Name, Value)).


% 'LOD_location'(+Prefix:atom, -Location:iri) is det.
% Used in case the XML namespace does not denote
%  a machine-readable description of the vocabulary, e.g. Dublin Core.

'LOD_location'(Prefix, Location):-
  lod_location(Prefix, Location).

'LOD_register_location'(Prefix, Location):-
  db_add_novel(lod_location(Prefix, Location)).

