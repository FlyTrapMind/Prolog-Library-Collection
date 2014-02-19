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
@version 2014/02
*/

:- use_module(generics(db_ext)).
:- use_module(generics(uri_ext)).
:- use_module(html(html)).
:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xpath)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(rdfs(rdfs_label_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(mime, 'http://www.iana.org/assignments/media-types#').

:- initialization(init_mime).



mime_type(MIME):-
  nonvar(MIME), !,
  atomic_list_concat([Type,Subtype], '/', MIME),
  mime_type(Type, Subtype).
mime_type(MIME):-
  mime_type(Type, Subtype),
  atomic_list_concat([Type,Subtype], '/', MIME).

mime_type(Type, Subtype):-
  rdfs_individual_of(Registration, mime:'Registration'),
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
    mime:template,
    xsd_string,
    Subtype,
    _
  ),
  rdf(Registration, rdf:type, Class),
  rdfs_label(Class, _, Type, _),
  rdf_datatype(
    Registration,
    mime:default_extension,
    xsd:string,
    DefaultExtension,
    _
  ).
mime_type_file_extension(MIME, DefaultExtension):-
  rdf_datatype(
    Registration,
    mime:default_extension,
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
  rdf_global_id(mime:Type2, Class),
  rdfs_assert_subclass(Class, mime:'Registration', Graph),
  rdfs_assert_label(Class, Type2, Graph),

  % Assert subtype.
  rdf_bnode(Registration),
  rdf_assert_individual(Registration, Class, Graph),
  rdf_assert_datatype(
    Registration,
    mime:template,
    xsd:string,
    Subtype,
    Graph
  ),
  rdfs_assert_label(Registration, Subtype, Graph),

  assert_mime_schema_ext(Graph),
  rdf_assert_datatype(
    Registration,
    mime:default_extension,
    xsd:string,
    DefaultExtension,
    Graph
  ),

  atomic_list_concat([Type2,Subtype], '/', MIME),
  db_add_novel(user:prolog_file_type(DefaultExtension, MIME)).

assert_mime_schema_ext(Graph):-
  % Property default file extension.
  rdf_assert_property(mime:default_extension, Graph),
  rdfs_assert_domain(mime:default_extension, mime:'Registration', Graph),
  rdfs_assert_range(mime:default_extension, xsd:string, Graph),
  rdfs_assert_label(mime:default_extension, 'default file extension', Graph).


init_mime:-
  absolute_file_name(
    data(mime),
    File,
    [access(read),extensions([ttl]),file_errors(fail)]
  ), !,
  rdf_load([mime('text/turtle')], mime, File).
init_mime:-
  assert_mime_iana(mime),
  assert_mime_extensions(mime),

  absolute_file_name(data('mime.ttl'), File, [access(write)]),
  rdf_save([mime('text/turtle')], mime, File),

  init_mime.


assert_mime_iana(Graph):-
  assert_mime_schema(Graph),
  maplist(
    assert_mime_category(Graph),
    [application,audio,image,message,model,multipart,text,video]
  ).


assert_mime_extensions(Graph):-
  url_to_html('http://www.webmaster-toolkit.com/mime-types.shtml', DOM),
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


assert_mime_category(Graph, Type):-
  assert_mime_schema(Graph, Type, Class),

  atomic_list_concat(
    ['http://www.iana.org/assignments/media-types/',Type,'.csv'],
    URL
  ),
  download_to_file([], URL, File),

  setup_call_cleanup(
    csv_read_file(File, [_|Rows]),
    maplist(assert_mime_row(Graph, Class), Rows),
    delete_file(File)
  ).


assert_mime_schema(Graph):-
  % Class registration.
  rdfs_assert_class(mime:'Registration', Graph),
  rdfs_assert_label(
    mime:'Registration',
    'IANA Media Type registration',
    Graph
  ),

  % Property description.
  rdf_assert_property(mime:description, Graph),
  rdfs_assert_domain(mime:description, mime:'Registration', Graph),
  rdfs_assert_range(mime:description, xsd:string, Graph),
  rdfs_assert_label(mime:description, description, Graph),

  % Property template.
  rdf_assert_property(mime:template, Graph),
  rdfs_assert_domain(mime:template, mime:'Registration', Graph),
  rdfs_assert_range(mime:template, xsd:string, Graph),
  rdfs_assert_label(mime:template, template, Graph),

  % Property reference.
  rdf_assert_property(mime:reference, Graph),
  rdfs_assert_domain(mime:reference, mime:'Registration', Graph),
  rdfs_assert_range(mime:reference, xsd:string, Graph),
  rdfs_assert_label(mime:reference, reference, Graph).

assert_mime_schema(Graph, Type, Class):-
  rdf_global_id(mime:Type, Class),
  rdfs_assert_subclass(Class, mime:'Registration', Graph),
  rdfs_assert_label(Class, Type, Graph).


assert_mime_row(Graph, Class, Row):-
  rdf_bnode(Registration),
  rdf_assert_individual(Registration, Class, Graph),
  assert_mime_row1(Graph, Registration, Row).

assert_mime_row1(
  Graph,
  Registration,
  row(Name,Template,Description,Reference)
):-
  assert_mime_row(Graph, Registration, row(Name,Template,Reference)),
  (
    Description == '', !
  ;
    rdf_assert_datatype(
      Registration,
      mime:description,
      xsd:string,
      Description,
      Graph
    )
  ), !.
assert_mime_row1(Graph, Registration, row(Name,Template,Reference)):-
  (
    Template == '', !
  ;
    rdf_assert_datatype(
      Registration,
      mime:template,
      xsd:string,
      Template,
      Graph
    )
  ),
  (
    Reference == '', !
  ;
    rdf_assert_datatype(
      Registration,
      mime:reference,
      xsd:string,
      Reference,
      Graph
    )
  ),
  rdfs_assert_label(Registration, Name, Graph).

