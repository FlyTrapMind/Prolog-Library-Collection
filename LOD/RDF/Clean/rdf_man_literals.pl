:- module(rdf_man_literals, []).

/** <module> RDF management: literals

Support for managing RDF literals.

@author Wouter Beek
@version 2014/03
*/

:- use_module(generics(meta_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_web(rdf_html_table)).
:- use_module(server(web_modules)).

http:location(rdf, root(rdf), []).
:- http_handler(rdf(literals), rdf_literals, []).

:- web_module_add('RDF literals', rdf_literals).



rdf_literals(_Request):-
  reply_html_page(app_style, title('RDF literals'), html(\rdf_literals)).

rdf_literals -->
  {
    setoff(
      [S,P,O,PlValue,G],
      (
        rdf(S, P, O, G),
        rdf_literal(O, LexicalForm, Datatype, LangTag),
        rdf_literal_map(LexicalForm, Datatype, LangTag, PlValue)
      ),
      CorrectRows
    ),
    setoff(
      [S,P,O,G],
      (
        rdf(S, P, O, G),
        rdf_literal(O, LexicalForm, Datatype, LangTag),
        \+ rdf_literal_map(LexicalForm, Datatype, LangTag, PlValue)
      ),
      IncorrectRows
    )
  },
  html([
    \rdf_html_table(
      [header_row(true)],
      html(p('Overview of correct RDF literals.')),
      [['Subject','Predicate','Literal','Prolog value','Graph']|CorrectRows]
    ),
    \rdf_html_table(
      [header_row(true)],
      html(p('Overview of incorrect RDF literals.')),
      [['Subject','Predicate','Literal','Graph']|IncorrectRows]
    )
  ]).

