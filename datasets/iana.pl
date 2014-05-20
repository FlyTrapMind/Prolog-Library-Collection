:- module(
  iana,
  [
    iana_mime/1, % ?Mime:atom
    iana_uri_scheme/1 % ?Scheme:iri
  ]
).

/** <module> IANA CSV to RDF

Crawl IANA Web pages and convert their contents to RDF.

@author Wouter Beek
@version 2014/01-2014/03, 2014/05
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(xml(xml_namespace)).

:- use_module(plRdf(rdf_download)).
:- use_module(plRdf(rdfs_label_ext)).
:- use_module(plRdf_conv(csv_to_rdf)).
:- use_module(plRdf_ser(rdf_serial)).
:- use_module(plRdf_term(rdf_string)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').



% QUERYING %

%! iana_mime(+Mime:atom) is semidet.
%! iana_mime(-Mime:atom) is nondet.
% @tbd

iana_mime(Mime):-
  init_iana_mime(mime),
  Mime = dummy.

iana_uri_scheme(Scheme):-
  init_iana_uri_scheme(uri_scheme),
  rdf_string(_, iana:uri_scheme, Scheme, uri_scheme).



% PERFORM SPECIFIC SCRAPES %

init_iana_mime(Graph):-
  absolute_file_name(data(mime), File, [file_type(ntriples)]),
  rdf_download(
    iana_scrape_url_(
      'MIME-Registration',
      [application,audio,image,message,model,multipart,text,video]
    ),
    'http://www.iana.org/assignments/media-types/',
    File,
    Graph,
    [freshness_lifetime(3600)]
  ).

init_iana_uri_scheme(Graph):-
  absolute_file_name(data(uri_scheme), File, [file_type(ntriples)]),
  rdf_download(
    iana_scrape_url_(
      'URI-Schema-Registration',
      ['uri-schemes-1','uri-schemes-2']
    ),
    'http://www.iana.org/assignments/uri-schemes/',
    File,
    Graph,
    [freshness_lifetime(3600)]
  ).

iana_scrape_url_(ResourceClassName, Categories, Url, File, Graph, _):-
  iana_scrape_url(Graph, Url, ResourceClassName, Categories),
  rdf_save([graph(Graph),format(ntriples)], Graph, File).



% GENERIC SCRAPING SUPPORT %

%! iana_scrape_url(
%!   +Graph:atom,
%!   +IanaUrl:url,
%!   +ResourceClass:iri,
%!   +Categories:list(atom)
%! ) is det.

iana_scrape_url(Graph, IanaUrl, ResourceClassName, Categories):-
  maplist(
    iana_scrape_url_category(Graph, IanaUrl, ResourceClassName),
    Categories
  ).


%! iana_scrape_url_category(
%!   +Graph:atom,
%!   +IanaUrl:url,
%!   +ResourceClassName:atom,
%!   +Category:atom
%! ) is det.
% @tbd Parse from HTTP stream directly.
%      ~~~{.pl}
%      :- use_module(library(http/http_open)).
%      setup_call_cleanup(
%        http_open(Url, Stream, []),
%        csv_to_rdf(Stream, Graph, iana, ResourceClass),
%        close(Stream)
%      )
%      ~~~

iana_scrape_url_category(Graph, IanaUrl, ResourceClassName, Category):-
  atomic_list_concat([IanaUrl,Category,'.csv'], Url),
  csv_to_rdf(Url, Graph, iana, ResourceClassName).

