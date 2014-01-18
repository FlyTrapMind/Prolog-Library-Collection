:- module(
  rdf_image,
  [
    rdf_assert_image/4, % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +Object:or([bnode,iri,literal])
                        % +Graph:atom
    rdf_image/2, % +URL:atom
                 % -File:atom
    rdf_image/5 % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Object:or([bnode,iri,literal])
                % ?ImageFile:atom
                % ?Graph:atom
  ]
).

/** <module> RDF image

Storing images in RDF is easy: just include a URL to the image.
But Web resources are not always sustainable,
 so images often go missing.

This module caches images when they are asserted in RDF.
When triples including images are read,
 the cache is updated if it has changed
 (and an image resource is available online).

@author Wouter Beek
@version 2014/01
*/

:- use_module(generics(uri_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dcmit, 'http://purl.org/dc/dcmitype/').

:- rdf_meta(rdf_assert_image(r,r,r,+)).
:- rdf_meta(rdf_image(r,r,r,-,?)).



rdf_assert_image(S, P, O, G):-
  rdf_image(O, _),
  is_image_url(O),
  rdf_assert_individual(O, dcmit:'Image', G),
  rdf_assert(S, P, O, G).


%! rdf_image(+URL:atom, -File:atom) is det.

rdf_image(URL, File):-
  url_to_file(URL, File),
  (
    access_file(File, exist), !
  ;
    download_to_file(URL, File)
  ).


rdf_image(S, P, O, File, G):-
  rdf(S, P, O, G),
  rdfs_individual_of(O, dcmit:'Image'),
  rdf_image(O, File).
