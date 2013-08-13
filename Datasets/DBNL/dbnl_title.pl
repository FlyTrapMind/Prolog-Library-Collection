:- module(
  dbnl_title,
  [
    dbnl_title/2 % +Graph:atom
                 % +Title:uri
  ]
).

/** <module> DBNL TITLE

Predicates for scraping a DBNL title page.

# Workflow

Links to primary texts are passert to dbnl_text/3.

# Content

## Left

  * beschikbare_titels:list(uri)
  * dbnl_logo:file

## Center

  * author:atom
  * beschikbare_tekst_in_de_dbnl
    * title:atom
    * year:atom
  * genres:atom
  * primaire_teksten_in_de_dbnl
    * author:atom
    * category:atom
    * publication_venue:atom
    * title:atom
    * year:atom
  * secundaire_literatuur_in_de_dbnl
    * authors:list(atom)
    * publication_venue:atom
    * title:uri
    * year:atom
  * subgenres:atom
  * terug_naar_overzicht:uri
  * title:atom
  * year:atom

## Right

# URI

~~~{.txt}
http://www.dbnl.org/titels/titel.php?id=ferr002atma01
~~~

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_primary)).
:- use_module(dbnl(dbnl_secondary_summary)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_html)).
:- use_module(dcg(dcg_multi)).
:- use_module(generics(atom_ext)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(semweb/rdf_db)).
:- use_module(standards(xpath_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_picarta(+Graph:atom, +Title:uri, +DOM:dom) is det.

dbnl_picarta(Graph, Title, DOM):-
  forall(
    (
      xpath2(DOM, //div(@id=meer), DIV),
      xpath2(DIV, a(@href), PicartaURI)
    ),
    rdf_assert(Title, dbnl:picarta, PicartaURI, Graph)
  ).

%! dbnl_title(+Graph:atom, +Title:uri) is det.

dbnl_title(Graph, Title):-
  rdf(Title, dbnl:original_page, URI, Graph),
  dbnl_title(Graph, Title, URI).

%! dbnl_title(+Graph:atom, +Title:uri, +URI:uri) is det.

dbnl_title(Graph, Title, URI):-
  dbnl_uri_to_html(URI, DOM),

  % Process contents.
  dbnl_dom_center(DOM, Content),
  xpath2(Content, div(content), Contents),
  phrase(dbnl_title0(Graph, Title), Contents),

  % Picarta link.
  dbnl_picarta(Graph, Title, DOM),
  !.
% Non-existing URIs. Contact the DBNL about this.
dbnl_title(_Graph, _Title, URI):-
  member(
    URI,
    [
      'http://www.dbnl.org/titels/titel.php?id=_ikst001ikst0'
    ]
  ),
  !.
% Debug.
dbnl_title(_Graph, _Title, URI):-
  gtrace, %DEB
  format(user_output, '~w\n', [URI]).

%! dbnl_title0(+Graph:atom, +Title:uri, +Contents:dom) is det.

% Skip notes.
dbnl_title0(Graph, Title) -->
  [element(p, [class=note], _)],
  !,
  dbnl_title0(Graph, Title).
% Author.
dbnl_title0(Graph, Title) -->
  [element(span, [class='titelpagina-auteur'], AuthorDOM)],
  !,
  {forall(
    (
      xpath2(AuthorDOM, //a(@href), RelativeAuthorURI),
      xpath2(AuthorDOM, //a(content), [AuthorName])
    ),
    (
      dbnl_uri_resolve(RelativeAuthorURI, AbsoluteAuthorURI),
      dbnl_assert_author(Graph, AbsoluteAuthorURI, AuthorName, Author),
      rdf_assert(Title, dbnl:author, Author, Graph)
    )
  )},
  dbnl_title0(Graph, Title).
% Genres.
dbnl_title0(Graph, Title) -->
  [element(span, [class='titelpagina-genres'], [_B, GenresAtom1])],
  !,
  {
    sub_atom(GenresAtom1, 2, _, 0, GenresAtom2),
    split_atom_exclusive(', ', GenresAtom2, GenreNames),
    maplist(dbnl_assert_genre(Graph), GenreNames, Genres),
    forall(
      member(Genre, Genres),
      rdf_assert(Title, dbnl:genre, Genre, Graph)
    )
  },
  dbnl_title0(Graph, Title).
% Subgenres.
dbnl_title0(Graph, Title) -->
  [element(span, [class='titelpagina-subgenres'], [_B, SubgenresAtom1])],
  !,
  % In some cases there are no subgenres.
  {atom_length(SubgenresAtom1, Length),
  (
    Length =< 2
  ->
    SubgenreNames = []
  ;
    sub_atom(SubgenresAtom1, 2, _, 0, SubgenresAtom2),
    split_atom_exclusive(', ', SubgenresAtom2, SubgenreNames)
  ),
  maplist(dbnl_assert_subgenre(Graph), SubgenreNames, Subgenres),
  forall(
    member(Subgenre, Subgenres),
    rdf_assert(Title, dbnl:subgenre, Subgenre, Graph)
  )},
  dbnl_title0(Graph, Title).
% Title + year.
dbnl_title0(Graph, Title) -->
  [element(span, [class='titelpagina-titel'], [Atom])], !,
  % A year may occur after the title.
  {
    dcg_phrase(
      (dbnl_title(Graph, Title), (dbnl_year(Graph, Title) ; "")),
      Atom
    )
  },
  dbnl_title0(Graph, Title).
% Summary
dbnl_title0(Graph, Title) -->
  [element(h4, [], ['Algemene informatie/samenvatting(en)'])], !,
  dbnl_summary(Graph, Title),
  dbnl_title0(Graph, Title).
% Primary text links.
dbnl_title0(Graph, Title) -->
  [element(h4, [], ['Beschikbare tekst in de dbnl'])], !,
  dcg_multi(dbnl_primary_text_link(Graph, Title), 1),
  dbnl_title0(Graph, Title).
% Secondary text links.
dbnl_title0(Graph, Title) -->
  [element(h4, [], ['Secundaire literatuur in de dbnl'])], !,
{gtrace},
  dbnl_summary(Graph, Title),
  dbnl_title0(Graph, Title).
% Skip linebreaks.
dbnl_title0(Graph, Title) -->
  [element(br, _, _)], !,
  dbnl_title0(Graph, Title).
% Skip italic text message on availability of scans.
dbnl_title0(Graph, Title) -->
  [element(i, [], ['(alleen scans beschikbaar)'])], !,
  dbnl_title0(Graph, Title).
% Atom
dbnl_title0(Graph, Title) -->
  [Atom],
  atom(Atom), !,
{gtrace},
  dbnl_title0(Graph, Title).
dbnl_title0(_Graph, _Title) --> [], !.
% Unrecognized content.
dbnl_title0(_Graph, _Title) -->
  dcg_debug.

dbnl_primary_text_link(Graph, Title) -->
  html_element(a, [href=RelativeURI], [TitleAtom]),
  % Sometimes a 'scans only' message occurs in italic. We skip this.
  (html_element(i, _, _) ; ""),
  [YearAtom],
  !,
  {
    % A year may occur after the title.
    %gtrace,
    dcg_phrase(dbnl_title(Graph, Title), TitleAtom),
    dcg_phrase(dbnl_year(Graph, Title), YearAtom),
    dbnl_uri_resolve(RelativeURI, AbsoluteURI),
    dbnl_primary(Graph, Title, AbsoluteURI)
  }.

