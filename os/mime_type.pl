:- use_module(library(lists)).
:- use_module(library(semweb/rdf_turtle)). % RDF-serialization.
:- use_module(library(semweb/rdf_turtle_write)). % RDF-serialization.
:- use_module(library(xpath)).

:- use_module(generics(db_ext)).
:- use_module(html(html)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf_term(rdf_datatype)).

iana_mime(MIME):-
  nonvar(MIME), !,
  atomic_list_concat([Type,Subtype], '/', MIME),
  iana_mime(Type, Subtype).
iana_mime(MIME):-
  iana_mime(Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', MIME).

iana_mime(Type, Subtype):-
  rdfs_individual_of(Registration, iana:'Registration'),
  iana_mime(Registration, Type, Subtype).

iana_mime(Registration, Type, Subtype):-
  rdfs_label(Registration, Subtype),
  once(rdf(Registration, rdf:type, Class)),
  once(rdfs_label(Class, Type)).

%! iana_mime_file_extension(+MIME:atom, +DefaultExtension:atom) is semidet.
%! iana_mime_file_extension(-MIME:atom, +DefaultExtension:atom) is semidet.
%! iana_mime_file_extension(+MIME:atom, -DefaultExtension:atom) is semidet.
%! iana_mime_file_extension(?MIME:atom, ?DefaultExtension:atom) is nondet.

iana_mime_file_extension(MIME, DefaultExtension):-
  nonvar(MIME), !,
  atomic_list_concat([Type,Subtype], '/', MIME),
  rdf_string(Registration, iana:template, Subtype, _),
  rdf(Registration, rdf:type, Class),
  rdfs_label(Class, _, Type),
  rdf_string(Registration, iana:default_extension, DefaultExtension, _).
iana_mime_file_extension(MIME, DefaultExtension):-
  rdf_string(Registration, iana:default_extension, DefaultExtension, _),
  iana_mime(Registration, Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', MIME).

%! iana_register_mime(
%!   +Type:atom,
%!   +Subtype:atom,
%!   +DefaultExtension:atom
%! ) is det.

% Already registered.
iana_register_mime(Type, Subtype, _):-
  iana_mime(Type, Subtype), !.
% New registration.
iana_register_mime(Type, Subtype, DefaultExtension):-
  iana_register_mime(Type, Subtype, DefaultExtension, mime_ext).

iana_register_mime(Type1, Subtype, DefaultExtension, G):-
  % The table of the Website we scrape for file extensions
  %  contains a typo: `applicaiton` i.o. `application`.
  (Type1 == applicaiton -> Type2 = application ; Type2 = Type1),

  % Assert type.
  rdf_global_id(iana:Type2, Class),
  rdf_assert(Class, rdfs:subClassOf, iana:'Registration', G),
  rdf_assert(Class, rdfs:label, literal(type(xsd:string,Type2)), G),

  % Assert subtype.
  rdf_bnode(Registration),
  rdf_assert_individual(Registration, Class, G),
  rdf_assert(Registration, iana:template, literal(type(xsd:string,Subtype)), G),
  rdf_assert(Registration, rdfs:label, literal(type(xsd:string,Subtype)), G),

  assert_mime_schema_ext(G),
  rdf_assert(Registration, iana:default_extension, literal(type(xsd:string,DefaultExtension)), G),

  atomic_list_concat([Type2,Subtype], '/', MIME),
  db_add_novel(user:prolog_file_type(DefaultExtension, MIME)).

assert_mime_schema_ext(G):-
  % Property default file extension.
  rdf_assert_property(iana:default_extension, G),
  rdf_assert(iana:default_extension, rdfs:domain, iana:'Registration', G),
  rdf_assert(iana:default_extension, rdfs:range, xsd:string, G),
  rdf_assert(iana:default_extension, rdfs:label, literal(type(xsd:string,'default file extension')), G).

  iana_register_mime(application, 'atom+xml',         atom),
  iana_register_mime(application, 'x-rar-compressed', rar ),
  iana_register_mime(application, 'x-bibtex',         bib ),
  iana_register_mime(application, 'n-quads',          nq  ),
  iana_register_mime(application, 'n-triples',        nt  ).

assert_mime_extensions(G):-
  download_html(
    [html_dialect(html4)],
    'http://www.webmaster-toolkit.com/mime-types.shtml',
    DOM
  ),
  forall(
    (
      member(Class, [tablerowdark,tablerowlight]),
      xpath(DOM, //tr(@class=Class), TR),
      xpath(TR, td(1,content), [DefaultExtension]),
      xpath(TR, td(2,content), [MIME])
    ),
    (
      atomic_list_concat([Type,Subtype], '/', MIME),
      iana_register_mime(Type, Subtype, DefaultExtension, G)
    )
  ).

