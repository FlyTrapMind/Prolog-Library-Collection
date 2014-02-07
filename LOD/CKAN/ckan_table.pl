:- module(ckan_table, []).

/** <module> CKAN table

@author Wouter Beek
@version 2014/02
*/

:- use_module(generics(meta_ext)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_lit_read)).



ckan_table(_Request):-
  findall(
    [],
    rdf(AP, 

  once(rdf_literal(Resource, ckan:url, URL, Site1)),
  once(rdf_literal(Resource, ckan:id, ResourceId, Site1)),
  once(rdf_literal(Resource, ckan:format, ResourceFormat, Site1)),
  (
    once(rdf_literal(Resource, ckan:resource_type, ResourceType, Site1))
  ->
    atomic_list_concat([ResourceId,ResourceFormat,ResourceType,URL], '\n', X1)
  ;
    atomic_list_concat([ResourceId,ResourceFormat,URL], '\n', X1)
  ),
  debug(semuri, 'Starting:\n~w', [X1]),

  once(rdf(Package, ckan:resources, Resource, Site1)),
  once(rdf_literal(Package, ckan:name, PackageName, Site1)),
  once(rdf_literal(Package, ckan:title, PackageTitle, Site1)),
  atomic_list_concat([PackageName,PackageTitle], '\n', X2),

  once(rdf(Package, ckan:organization, Organization, Site1)),
  once(rdf_literal(Organization, ckan:display_name, OrganizationName, Site1)),

  setoff(
    UserName,
    (
      rdf(Organization, ckan:users, User, Site1),
      rdf_literal(User, ckan:fullname, UserName, Site1)
    ),
    UserNames
  ),
  atomic_list_concat(UserNames, '\n', UserName),

  setoff(
    TagName,
    (
      rdf(Package, ckan:tags, Tag, Site1),
      rdf_literal(Tag, ckan:name, TagName, Site1)
    ),
    TagNames
  ),
  atomic_list_concat(TagNames, '\n', TagName),
  



  
  reply_html_page(
    app_style,
    title('CKAN table'),
    \html_table(
      [header_column(false),header_row(true),indexed(true)],
      'Results of the CKAN AP',
      Rows
    )
  ).

