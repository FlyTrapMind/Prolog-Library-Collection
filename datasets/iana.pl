:- module(
  iana,
  [
    iana_mime/1, % ?ContentType:atom
    iana_mime/2, % ?Type:atom
                 % ?Subtype:atom
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
:- use_module(library(xpath)).

:- use_module(generics(db_ext)).

:- use_module(plHtml(html)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_download)).
:- use_module(plRdf(rdfs_build)).
:- use_module(plRdf(rdfs_label_ext)).
:- use_module(plRdf_conv(csv_to_rdf)).
:- use_module(plRdf_ser(rdf_serial)).
:- use_module(plRdf_term(rdf_string)).

:- rdf_register_prefix(iana, 'http://www.iana.org/assignments/').

%:- iana_register_mime(application, 'atom+xml', atom).
%:- iana_register_mime(application, 'x-rar-compressed', rar).
%:- iana_register_mime(application, 'x-bibtex', bib).
%:- iana_register_mime(application, 'n-quads', nq).
%:- iana_register_mime(application, 'n-triples', nt).



% QUERYING %

%! iana_mime(+Mime:atom) is semidet.
%! iana_mime(-Mime:atom) is nondet.
% @tbd

iana_mime(Mime):-
  nonvar(Mime), !,
  atomic_list_concat([Type,Subtype], '/', Mime),
  iana_mime(Type, Subtype).
iana_mime(Mime):-
  iana_mime(Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', Mime).

iana_mime(Type, Subtype):-
  Graph = mime,
  init_iana_mime(Graph),
  rdf_string(Registration, iana:name, Subtype, Graph),
  rdf(Registration, rdf:type, Class, Graph),
  rdf_string(Class, rdfs:label, Type, Graph).


iana_register_mime(Type1, Subtype, DefaultExtension):-
  Graph = mime,
  
  % The table of the Website we scrape for file extensions
  % contains a typo: `applicaiton` i.o. `application`.
  (Type1 == applicaiton -> Type2 = application ; Type2 = Type1),

  % Type.
  atomic_list_concat(['MIME-Registration',Type2], '-', ClassName),
  rdf_global_id(iana:ClassName, Class),
  rdfs_assert_subclass(Class, iana:'Registration', Graph),
  rdfs_assert_label(Class, Type2, Graph),

  % Subtype.
  rdf_bnode(Registration),
  rdf_assert_individual(Registration, Class, Graph),
  rdf_assert_string(Registration, iana:name, Subtype, Graph),
  rdfs_assert_label(Registration, Subtype, Graph),
  
  % Template.
  atomic_list_concat([Type2,Subtype], '/', ContentType),
  rdf_assert_string(Registration, iana:template, ContentType, Graph),
  
  % Default extension.
  rdf_assert_string(Registration, iana:default_extension, DefaultExtension,
      Graph),
  
  % Prolog file type mapping: extension <-> content type.
  db_add_novel(user:prolog_file_type(DefaultExtension, ContentType)).


iana_uri_scheme(Scheme):-
  Graph = uri_scheme,
  init_iana_uri_scheme(Graph),
  rdf_string(_, iana:uri_scheme, Scheme, Graph).



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
    [freshness_lifetime(3600),graph(Graph)]
  ),
  assert_mime_extensions.

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
  rdf_save_any(File, [format(ntriples),graph(Graph)]).

assert_mime_extensions:-
  download_html(
    'http://www.webmaster-toolkit.com/mime-types.shtml',
    Dom,
    [html_dialect(html4)]
  ),
  forall(
    (
      member(Class, [tablerowdark,tablerowlight]),
      xpath(Dom, //tr(@class=Class), TR),
      xpath(TR, td(1,content), [DefaultExtension]),
      xpath(TR, td(2,content), [ContentType])
    ),
    (
      atomic_list_concat([Type,Subtype], '/', ContentType),
      iana_register_mime(Type, Subtype, DefaultExtension)
    )
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
    [freshness_lifetime(3600),graph(Graph)]
  ).

iana_scrape_url2_(ClassName, Categories, Url, File, Graph, _):-
  iana_scrape_url(Graph, Url, ClassName, Categories),
  rdf_save_any(File, [format(ntriples),graph(Graph)]).



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

