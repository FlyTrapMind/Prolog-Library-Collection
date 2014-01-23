:- module(
  flickrwrappr,
  [
    is_flickrwrappr_resource/1, % +Resource:iri
    flickrwrappr_query/3 % +Resource:iri
                         % -Resources:ordset([bnode,iri,literal])
                         % -Propositions:ordset(list)
  ]
).

/** <module> FlickrWrappr

@author Wouter Beek
@see http://wifo5-03.informatik.uni-mannheim.de/flickrwrappr/
@version 2014/01
*/

:- use_module(generics(uri_ext)).
:- use_module(library(uri)).
:- use_module('LOD'('LOD_query')).



is_flickrwrappr_resource(IRI):-
  uri_components(
    IRI,
    uri_components(_,'wifo5-03.informatik.uni-mannheim.de',Path,_,_)
  ),
  file_directory_name(Path, '/flickrwrappr/photos/').


flickrwrappr_query(IRI1, Resources, Propositions):-
  uri_query_add(IRI1, format, rdf, IRI2),
  'LOD_local_query'(IRI2, Resources, Propositions).

