:- module(
  file_mime,
  [
    file_mime/2 % +File:atom
                % -Mime:atom
  ]
).

/** <module> File mime

Returns the MIME of a given file.

@author Wouter Beek
@version 2012/10, 2014/01
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(pure_input)).
:- use_module(os(io_ext)).



file_mime(File, Mime):-
  file_to_atom(File, Atom),
  atom_codes(Atom, Codes),
  phrase(file_mime(Mime), Codes), !.
file_mime(File, Mime):-
  gtrace,
  file_mime(File, Mime).
/*
file_mime(File, Mime):-
  phrase_from_file(file_mime(Mime), File).
*/


file_mime('application/x-turtle') -->
  `@prefix`, !,
  dcg_all.
file_mime('text/html') -->
  `<!DOCTYPE HTML`, !,
  dcg_all.
file_mime(Mime) -->
  blanks,
  (xml_declaration(_) ; ""), blanks,
  (xml_comment ; ""), blanks,
  xml_something(Mime),
  dcg_all.

xml_comment -->
  `<!--`,
  dcg_until([end_mode(inclusive)], test, _),
  blanks_to_nl.
test -->
  `-->`.

%! xml_declaration(?Version:float)// .
% The XML specification also permits an XML declaration at
%  the top of the document with the XML version and possibly
%  the XML content encoding. This is optional but recommended.

xml_declaration(Version) -->
  `<?xml`, whites,
  (xml_version(Version), whites ; ""),
  (xml_encoding, whites ; ""),
  `?>`, blanks_to_nl.

xml_doctype('application/rdf+xml') -->
  `<!DOCTYPE rdf:RDF`, !,
  dcg_all.

xml_encoding -->
  `encoding=`,
  quoted("UTF-8").

xml_something('application/rdf+xml') -->
  `<rdf:RDF`, !,
  dcg_all.
xml_something(MIME) -->
  xml_doctype(MIME).
xml_something('text/xml') -->
  dcg_all.

xml_version(Version) -->
  `version=`,
  quoted(float(Version)).

