:- module(ckan_table, []).

/** <module> CKAN table

@author Wouter Beek
@version 2014/02
*/

:- use_module(ap(ap_table)).
:- use_module(generics(meta_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_container)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(server(web_modules)).

http:location(ckan, root(ckan), []).
:- http_handler(ckan(table), ckan_table, []).

:- initialization(web_module_add('CKAN Table', ckan_table)).



ckan_table(_Request):-
  ap_table(ckan_header, ckan_row).

ckan_header(Header, ['Name','Title','Organization','Users','Tags'|Header]).

ckan_row([H|T], [Name,Title,Organization,Users,Tags,H|T]):-
  rdf_collection_member(H, AP, ap),
  rdf(AP, ap:resource, Resource, ap),
  once(rdf(Package, ckan:resources, Resource, _)),
  once(rdf_literal(Package, ckan:name, Name, _)),
  once(rdf_literal(Package, ckan:title, Title, _)),

  % Organization.
  once(rdf(Package, ckan:organization, X, _)),
  once(rdf_literal(X, ckan:display_name, Organization, _)),

  % Users.
  setoff(
    UserName,
    (
      rdf(Organization, ckan:users, User, Site1),
      rdf_literal(User, ckan:fullname, UserName, Site1)
    ),
    UserNames
  ),
  atomic_list_concat(UserNames, '\n', Users),

  % Tags.
  setoff(
    TagName,
    (
      rdf(Package, ckan:tags, Tag, Site1),
      rdf_literal(Tag, ckan:name, TagName, Site1)
    ),
    TagNames
  ),
  atomic_list_concat(TagNames, '\n', Tags).

