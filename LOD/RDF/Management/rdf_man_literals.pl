:- module(rdf_man_literals, []).

/** <module> RDF management: literals

Support for managing RDF literals.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(web_modules)).

http:location(rdf,     root(rdf), []).
http:location(rdf_man, rdf(man),  []).
:- http_handler(rdf_man(literals), rdf_man_literals, []).

:- web_module_add('RDFm literals', rdf_man_literals).



rdf_man_literals(_Request):-
  reply_html_page(
    app_style,
    title('RDFm literals'),
    html(\rdf_man_literals)
  ).


rdf_man_literals -->
  rdf_man_incorrect_literals,
  %rdf_man_correct_literals.


rdf_man_correct_literals -->
  {
    aggregate_all(
      set([S,P,O,PlValue,G]),
      (
        rdf(S, P, O, G),
        rdf_literal(O, LexicalForm, Datatype, LangTag),
        rdf_literal_map(LexicalForm, Datatype, LangTag, PlValue)
      ),
      CorrectRows
    )
  },
  rdf_html_table(
    [header_row(true)],
    html(p('Overview of correct RDF literals in all RDF graphs.')),
    [['Subject','Predicate','Literal','Prolog value','Graph']|CorrectRows]
  ).


rdf_man_incorrect_literals -->
  {
    aggregate_all(
      set([S,P,L,G]),
      (
        rdf(S, P, L, G),
        rdf_literal(L, LexicalForm, Datatype, LangTag),
        \+ rdf_literal_map(LexicalForm, Datatype, LangTag, PlValue)
      ),
      IncorrectRows
    )
  },
  rdf_html_table(
    [header_row(splg)],
    html('Overview of incorrect RDF literals in all RDF graphs.'),
    IncorrectRows
  ).


rdf_man_lexical_form_operations(Goal, 
  findall(
    [S,P,Literal1,Literal2,G],
    (
      rdf_literal(S, P, LexicalForm1, Datatype, LanguageTag, 

