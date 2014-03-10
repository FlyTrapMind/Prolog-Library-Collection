:- module(
  mime_type,
  [
% READ
    mime_type/1, % ?MIME:atom
    mime_type/2, % ?Type:atom
                 % ?Subtype:atom
    mime_type_file_extension/2, % ?MIME:atom
                                % ?DefaultExtension:atom
% BUILD
    mime_register_type/3 % +Type:atom
                         % +Subtype:atom
                         % +DefaultExtension:atom
  ]
).

/** <module> MIME type

Support for IANA-registered MIME types.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(generics(db_ext)).
:- use_module(html(html)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)). % RDF-serialization.
:- use_module(library(semweb/rdf_turtle_write)). % RDF-serialization.
:- use_module(library(xpath)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(rdfs(rdfs_label_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(iana_to_rdf)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iana, 'http://www.iana.org/assignments/').

:- initialization(init_mime).




mime_type(MIME):-
  nonvar(MIME), !,
  atomic_list_concat([Type,Subtype], '/', MIME),
  mime_type(Type, Subtype).
mime_type(MIME):-
  mime_type(Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', MIME).

mime_type(Type, Subtype):-
  rdfs_individual_of(Registration, iana:'Registration'),
  mime_type(Registration, Type, Subtype).

mime_type(Registration, Type, Subtype):-
  rdfs_label(Registration, _, Subtype, _),
  once(rdf(Registration, rdf:type, Class)),
  once(rdfs_label(Class, _, Type, _)).


%! mime_type_file_extension(+MIME:atom, +DefaultExtension:atom) is semidet.
%! mime_type_file_extension(-MIME:atom, +DefaultExtension:atom) is semidet.
%! mime_type_file_extension(+MIME:atom, -DefaultExtension:atom) is semidet.
%! mime_type_file_extension(?MIME:atom, ?DefaultExtension:atom) is nondet.

mime_type_file_extension(MIME, DefaultExtension):-
  nonvar(MIME), !,
  atomic_list_concat([Type,Subtype], '/', MIME),
  rdf_datatype(
    Registration,
    iana:template,
    xsd_string,
    Subtype,
    _
  ),
  rdf(Registration, rdf:type, Class),
  rdfs_label(Class, _, Type, _),
  rdf_datatype(
    Registration,
    iana:default_extension,
    xsd:string,
    DefaultExtension,
    _
  ).
mime_type_file_extension(MIME, DefaultExtension):-
  rdf_datatype(
    Registration,
    iana:default_extension,
    xsd:string,
    DefaultExtension,
    _
  ),
  mime_type(Registration, Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', MIME).


%! mime_register_type(
%!   +Type:atom,
%!   +Subtype:atom,
%!   +DefaultExtension:atom
%! ) is det.

% Already registered.
mime_register_type(Type, Subtype, _):-
  mime_type(Type, Subtype), !.
% New registration.
mime_register_type(Type, Subtype, DefaultExtension):-
  mime_register_type(Type, Subtype, DefaultExtension, mime_ext).

mime_register_type(Type1, Subtype, DefaultExtension, Graph):-
  % The table of the Website we scrape for file extensions
  %  contains a typo: `applicaiton` i.o. `application`.
  (Type1 == applicaiton -> Type2 = application ; Type2 = Type1),

  % Assert type.
  rdf_global_id(iana:Type2, Class),
  rdfs_assert_subclass(Class, iana:'Registration', Graph),
  rdfs_assert_label(Class, Type2, Graph),

  % Assert subtype.
  rdf_bnode(Registration),
  rdf_assert_individual(Registration, Class, Graph),
  rdf_assert_datatype(
    Registration,
    iana:template,
    xsd:string,
    Subtype,
    Graph
  ),
  rdfs_assert_label(Registration, Subtype, Graph),

  assert_mime_schema_ext(Graph),
  rdf_assert_datatype(
    Registration,
    iana:default_extension,
    xsd:string,
    DefaultExtension,
    Graph
  ),

  atomic_list_concat([Type2,Subtype], '/', MIME),
  db_add_novel(user:prolog_file_type(DefaultExtension, MIME)).

assert_mime_schema_ext(Graph):-
  % Property default file extension.
  rdf_assert_property(iana:default_extension, Graph),
  rdfs_assert_domain(iana:default_extension, iana:'Registration', Graph),
  rdfs_assert_range(iana:default_extension, xsd:string, Graph),
  rdfs_assert_label(iana:default_extension, 'default file extension', Graph).


init_mime:-
  absolute_file_name(
    data(mime),
    File,
    [access(read),extensions([ttl]),file_errors(fail)]
  ), !,
  rdf_load(File, [format(turtle),graph(mime)]),
  mime_register_type(application, 'atom+xml',         atom),
  mime_register_type(application, 'x-rar-compressed', rar ),
  mime_register_type(application, 'x-bibtex',         bib ),
  mime_register_type(application, 'n-quads',          nq  ),
  mime_register_type(application, 'n-triples',        nt  ).
init_mime:-
  assert_iana(
    mime,
    'http://www.iana.org/assignments/media-types/',
    iana:'MIMERegistration',
    [application,audio,image,message,model,multipart,text,video]
  ),

  assert_mime_extensions(mime),

  absolute_file_name(data('mime.ttl'), File, [access(write)]),
  rdf_save_turtle(File, [graph(mime)]),

  init_mime.


assert_mime_extensions(Graph):-
  download_html('http://www.webmaster-toolkit.com/mime-types.shtml', DOM),
  forall(
    (
      member(Class, [tablerowdark,tablerowlight]),
      xpath(DOM, //tr(@class=Class), TR),
      xpath(TR, td(1,content), [DefaultExtension]),
      xpath(TR, td(2,content), [MIME])
    ),
    (
      atomic_list_concat([Type,Subtype], '/', MIME),
      mime_register_type(Type, Subtype, DefaultExtension, Graph)
    )
  ).

