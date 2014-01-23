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


file_mime(File, Mime):-
  phrase_from_file(file_mime(Mime), File).



file_mime('text/html') -->
  "<!DOCTYPE HTML ", !,
  dcg_all.
file_mime(Mime) -->
  "<?xml ",
  (xml_version(_), " " ; ""),
  (xml_encoding ; ""),
  "?>",
  blanks_to_nl, line_feed,
  (xml_comment ; ""),
  xml_doctype(Mime),
  dcg_all.

xml_comment -->
  "<!--",
  dcg_until([end_mode(inclusive)], "-->", _),
  blanks_to_nl, line_feed.

xml_doctype(Mime) -->
  "<!DOCTYPE ",
  xml_doctype_value(Mime).

xml_doctype_value('application/rdf+xml') -->
  "rdf:RDF", !,
  dcg_all.
xml_doctype_value('text/xml') -->
  dcg_all.

xml_encoding -->
  "encoding=",
  quoted("UTF-8").

xml_version(Version) -->
  "version=",
  quoted(float(Version)).

