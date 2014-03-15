:- module(
  rdf_web,
  [
    rdf_explain_web/4, % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +Object:or([bnode,iri,literal])
                       % -SVG:list
    rdf_load_web/2, % +Graph:atom
                    % -DOM:list
    rdf_save_web/2 % +Graph:atom
                   % -DOM:list
  ]
).

/** <module> RDF Web

Web predicates for RDF graphs.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/09, 2013/11-2014/01
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(generics(uri_ext)).
:- use_module(html(html_table)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_meta_auto_expand)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdf_reasoning(rdf_bnode_map)).
:- use_module(rdf_reasoning(rdf_mat)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).
:- use_module(xml(xml_namespace)).

% This allows a user to type `rdf:type` in the Web console and
% have it translated to a full URI.
:- rdf_meta_expand(rdf_web:rdf_explain_web(e,e,e,i)).

:- http_handler(root(rdf), rdf_web, []).

:- initialization(web_module_add('RDF', rdf_web)).



%! rdf_explain_web(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   -SVG:list
%! ) is det.

rdf_explain_web(S, P, O, SVG):-
  with_output_to(atom(TripleName), rdf_triple_name(S, P, O)),
  tms_export_argument_web(TripleName, SVG).

%! rdf_load_web(+Graph:atom, -Markup:list) is det.
% Loads the graph with the given name into memory.
% Graphs that are loaded via this front-end should be located in the user's
% =data= subdirectory.
%
% @arg Graph The atomic name of a graph.
% @arg Markup

% Prefer turtle.
rdf_load_web(Graph, Markup):-
  rdf_load([], Graph, File), !,
  Markup =
    [element(p, [], ['Graph ', Graph, ' was loaded from file ', File, '.'])].
rdf_load_web(Graph, Markup):-
  Markup = [element(p, [], ['An RDF graph named ', Graph,
    ' could not be found in the personal data directory.'])].

%! rdf_save_web(+Graph:atom, -DOM:list) is det.
% Saves the RDF graph with the given name from the Web interface.

rdf_save_web(Graph, Markup):-
  rdf_save([format(Format)], Graph, File), !,
  Markup =
      [element(p,[],[
        'Graph ',
        Graph,
        ' was saved to file ',
        File,
        'using serialization format ',
        Format,
        '.'
      ])].
rdf_save_web(Graph, Markup):-
  Markup = [element(p,[],['Graph ',Graph,' could not be saved.'])].

rdf_web(_Request):-
  reply_html_page(app_style, title('RDF Web'), \rdf_web).

rdf_web -->
  {
    once(rdf_graph(G)), !,
    setoff(
      option(value=SLabel,SLabel),
      (
        rdf_subject(G, STerm),
        dcg_with_output_to(atom(SLabel), rdf_term_name(STerm))
      ),
      SItems
    ),
    setoff(
      option(value=PLabel,PLabel),
      (
        rdf_predicate(G, PTerm),
        dcg_with_output_to(atom(PLabel), rdf_term_name(PTerm))
      ),
      PItems
    ),
    setoff(
      option(value=OLabel,OLabel),
      (
        rdf_object(G, OTerm),
        dcg_with_output_to(atom(OLabel), rdf_term_name(OTerm))
      ),
      OItems
    )
  },
  rdf_web_form(SItems, PItems, OItems).
rdf_web -->
  rdf_load_web_.

rdf_web_form(SItems, PItems, OItems) -->
  html(
    form([class='pure-form',id=explain_rdf_triple], [
      fieldset(class='pure-group', [
        input([
          class='pure-input-1-2',
          id=rdf_subject_input,
          list=rdf_subjects,
          placeholder='Subject term',
          type=text
        ]),
        datalist(id=rdf_subjects, SItems),
        input([
          class='pure-input-1-2',
          id=rdf_predicate_input,
          list=rdf_predicates,
          placeholder='Predicate term',
          type=text
        ]),
        datalist(id=rdf_predicates, PItems),
        input([
          class='pure-input-1-2',
          id=rdf_object_input,
          list=rdf_objects,
          placeholder='Object term',
          type=text
        ]),
        datalist(id=rdf_objects, OItems),
        button(
          [
            class=['pure-button','pure-input-1-2','pure-button-primary'],
            type=submit
          ],
          'Sign in'
        )
      ])
    ])
  ).

rdf_load_web_ -->
  html(
    form([class='pure-form',id='rdf-load'], [
      fieldset(class='pure-group', [
        input([
          class='pure-input-1-2',
          id='rdf-file',
          type=file
        ])
      ])
    ])
  ).

