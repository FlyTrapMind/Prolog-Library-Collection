:- module(mime_url, []).

/** <module> MIME URL

Generates a CSV file with the following columns:
  - URL (not always syntactically conformant)
  - Format (according to CKAN metadata)
  - MIME (according to CKAN metadata)

@author Wouter Beek
@version 2013/03
*/

:- use_module(library(csv)).
:- use_module(os(file_ext)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_term(rdf_string)).

:- initialization(mime_url).



mime_url:-
  absolute_file_name(
    data(datahub_io),
    CKAN_File,
    [access(read),file_type(turtle)]
  ),
  mime_url(CKAN_File).


mime_url(CKAN_File):-
  rdf_setup_call_cleanup(
    [format(turtle)],
    CKAN_File,
    mime_url_rows(Rows)
  ),
  file_alternative(CKAN_File, _, mime_url, csv, CSV_File),
  csv_write_file(CSV_File, Rows).


mime_url_rows(Rows, Graph):-
  findall(
    row(URL,Format,MIME),
    (
      rdf_string(Resource, ckan:url, URL, Graph),
      (
        rdf_string(Resource, ckan:format, Format, Graph)
      ->
        true
      ;
        Format = null
      ),
      (
        rdf_string(Resource, ckan:mimetype, MIME, Graph)
      ->
        true
      ;
        MIME = null
      )
    ),
    Rows
  ).

