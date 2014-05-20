:- module(
  iana,
  [
% QUERYING
    iana_uri_scheme/1, % ?Scheme:iri
% SPECIFIC SCRAPES
    iana_scrape_mime/1, % +Graph:atom
    iana_scrape_uri_scheme/1 % +Graph:atom
  ]
).

/** <module> IANA CSV to RDF

Crawl IANA Web pages and convert their contents to RDF.

@author Wouter Beek
@version 2014/01-2014/03, 2014/05
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdfs)).

:- use_module(xml(xml_namespace)).

:- use_module(plRdf(rdfs_label_ext)).
:- use_module(plRdf_conv(csv_to_rdf)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').



% QUERYING %

iana_uri_scheme(Scheme):-
  rdfs_label(Registration, Scheme, _, uri_scheme),
  rdfs_individual_of(Registration, iana:'URISchemaRegistration').



% GENERIC SCRAPING %

iana_scrape_mime(Graph):-
  iana_scrape_url(
    Graph,
    'http://www.iana.org/assignments/media-types/',
    'MIME-Registration',
    [application,audio,image,message,model,multipart,text,video]
  ).


iana_scrape_uri_scheme(Graph):-
  iana_scrape_url(
    Graph,
    'http://www.iana.org/assignments/uri-schemes/',
    'URI-Schema-Registration',
    ['uri-schemes-1','uri-schemes-2']
  ).



% SPECIFIC SCRAPES %

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

