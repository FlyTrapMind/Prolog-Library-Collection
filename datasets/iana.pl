:- module(
  iana,
  [
    iana_mime/1, % ?ContentType:atom
    iana_mime/2, % ?Type:atom
                 % ?Subtype:atom
    iana_mime_file_extension/2, % ?ContentType:atom
                                % ?DefaultExtension:atom
    iana_register_mime/3, % +Type:atom
                          % +Subtype:atom
                          % +DefaultExtension:atom
    iana_uri_scheme/1 % ?Scheme:iri
  ]
).

/** <module> IANA CSV to RDF

Crawl IANA Web pages and convert their contents to RDF.

This module currently supports:
  - MIME content types.
  - URI Schemes

@author Wouter Beek
@version 2014/01-2014/03, 2014/05
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(xml(xml_namespace)).

:- use_module(plRdf(rdf_download)).
:- use_module(plRdf(rdfs_build)).
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
  iana_mime(Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', Mime).

iana_mime(Type, Subtype):-
  init_iana_mime(mime),
  rdf_string(Registration, iana:name, Subtype, mime),
  rdf(Registration, rdf:type, Class, mime),
  rdf(Class, rdfs:label, literal(type(xsd:string,Type)), mime).


iana_uri_scheme(Scheme):-
  init_iana_uri_scheme(uri_scheme),
  rdf_string(_, iana:uri_scheme, Scheme, uri_scheme).



% PERFORM SPECIFIC SCRAPES %

init_iana_mime(Graph):-
  absolute_file_name(data(mime), File, [file_type(ntriples)]),
  rdf_download(
    iana_scrape_url1_(
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
    iana_scrape_url2_(
      'URI-Schema-Registration',
      ['uri-schemes-1','uri-schemes-2']
    ),
    'http://www.iana.org/assignments/uri-schemes/',
    File,
    Graph,
    [freshness_lifetime(3600)]
  ).

iana_scrape_url1_(ClassName1, Categories, Url, File, Graph, _):-
  forall(
    member(Category, Categories),
    (
      atomic_list_concat([ClassName1,Category], '-', ClassName2),
      rdf_global_id(iana:ClassName1, Class1),
      rdf_global_id(iana:ClassName2, Class2),
      rdfs_assert_label(Class2, Category, Graph),
      rdfs_assert_subclass(Class1, Class2, Graph),
      iana_scrape_url_category(Graph, Url, ClassName2, Category)
    )
  ),
  rdf_save([graph(Graph),format(ntriples)], Graph, File).

iana_scrape_url2_(ClassName, Categories, Url, File, Graph, _):-
  iana_scrape_url(Graph, Url, ClassName, Categories),
  rdf_save([graph(Graph),format(ntriples)], Graph, File).



% GENERIC SCRAPING SUPPORT %

%! iana_scrape_url(
%!   +Graph:atom,
%!   +IanaUrl:url,
%!   +ResourceClass:iri,
%!   +Categories:list(atom)
%! ) is det.

iana_scrape_url(Graph, IanaUrl, ClassName, Categories):-
  maplist(
    iana_scrape_url_category(Graph, IanaUrl, ClassName),
    Categories
  ).


%! iana_scrape_url_category(
%!   +Graph:atom,
%!   +IanaUrl:url,
%!   +ClassName:atom,
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

iana_scrape_url_category(Graph, IanaUrl, ClassName, Category):-
  atomic_list_concat([IanaUrl,Category,'.csv'], Url),
  csv_to_rdf(Url, Graph, iana, ClassName).

