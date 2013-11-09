:- module(
  article,
  [
    general_information//2, % :Title
                            % +Authors:list(iri)
    paragraph//1, % :Content
    section//2 % :Title
               % :Content
  ]
).

/** <module> Article

Predicates for generating a Web article.

@author Wouter Beek
@version 2013/11
*/

:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_lit_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').

:- html_meta(general_information(html,+,?,?)).
:- html_meta(paragraph(html,?,?)).
:- html_meta(section(html,html,?,?)).

:- rdf_meta(author(r,?,?)).
:- rdf_meta(authors(t,?,?)).
:- rdf_meta(general_information(+,t,?,?)).



author(A_) -->
  {
    rdf_global_id(A_, A),
    rdf_literal(A, foaf:firstName, FirstName, G),
    rdf_literal(A, foaf:lastName, LastName, G)
  },
  html(div(class=author, ['Author: ',FirstName,' ',LastName])).

authors(L) -->
  {is_list(L)}, !,
  html(div(class=authors, \authors_(L))).
authors(A) -->
  authors([A]).

authors_([]) --> !.
authors_([H|T]) -->
  author(H),
  authors_(T).

general_information(Title, Authors) -->
  html([h1(class=article_title,Title),\authors(Authors)]).

paragraph(Content) -->
  html(p(class=paragraph, Content)).

section(Title, Content) -->
  html([h1(class=section_title, Title), Content]).

title(Title) -->
  html(h1(class=article_title, Title)).

