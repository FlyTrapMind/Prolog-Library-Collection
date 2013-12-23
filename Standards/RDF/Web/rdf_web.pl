:- module(
  rdf_web,
  [
    rdf_explain_web/4, % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +Object:or([bnode,iri,literal])
                       % -SVG:list
    rdf_graphs_web/1, % -DOM:list
    %rdf_load_web/2, % +Graph:atom
    %                % -DOM:list
    %rdf_load_web/3, % +Graph:atom
    %                % +Format:oneof([turtle,xml])
    %                % -DOM:list
    rdf_mat_web/3, % +Graph:atom
                   % +Regime:atom
                   % -DOM:list
    rdf_namespaces_web/1, % -DOM:list
    rdf_namespaces_web/2, % +Graph:atom
                          % -DOM:list
    %rdf_save_web/2, % +Graph:atom
    %                % -DOM:list
    rdf_web_argument/2 % +In:atom
                       % -Out:iri
  ]
).

/** <module> RDF Web

Web predicates for RDF graphs.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05, 2013/09, 2013/11-2013/12
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(html(html_table)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_bnode_map)).
:- use_module(rdf(rdf_mat)).
:- use_module(rdf(rdf_meta_auto_expand)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_term)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).
:- use_module(xml(xml_namespace)).

% This allows a user to type `rdf:type` in the Web console and
% have it translated to a full URI.
:- rdf_meta_expand(rdf_web:rdf_explain_web(e,e,e,i)).

:- http_handler(root(rdf), rdf_web, []).

:- initialization(web_module_add('RDF', rdf_web, rdf)).



%! rdf_explain_web(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal]),
%!   -SVG:list
%! ) is det.

rdf_explain_web(S, P, O, SVG):-
  with_output_to(atom(TripleName), rdf_triple_name([], S, P, O)),
  tms_export_argument_web(TripleName, SVG).

%! rdf_graphs_web(-Markup:list) is det.
% Returns the markup for an enumeration of the currently loaded graphs.
%
% @param Markup An HTML table.

rdf_graphs_web(Markup):-
  findall(
    [Graph,Triples],
    rdf_statistics(triples_by_graph(Graph, Triples)),
    List
  ),
  (
    List == []
  ->
    Markup = [element(p,[],['There are no loaded RDF graphs.'])]
  ;
    html_table(
      [caption('The currently loaded graphs:'),header(true),indexed(true)],
      [['Graph','Number of triples'] | List],
      Table
    ),
    Markup = [Table]
  ).

%! rdf_load_web(+Graph:atom, -Markup:list) is det.
% Loads the graph with the given name into memory.
% Graphs that are loaded via this front-end should be located in the user's
% =data= subdirectory.
%
% @param Graph The atomic name of a graph.
% @param Markup

% Prefer turtle.
rdf_load_web(Graph, Markup):-
  rdf_load2(File, Graph), !,
  Markup =
    [element(p, [], ['Graph ', Graph, ' was loaded from file ', File, '.'])].
rdf_load_web(Graph, Markup):-
  Markup = [element(p, [], ['An RDF graph named ', Graph,
    ' could not be found in the personal data directory.'])].

%! rdf_mat_web(+Graph:atom, +Regime:atom, -DOM:list) is det.

rdf_mat_web(G, Regime, [DOM1,DOM2]):-
  % Run amterialization.
  materialize(Regime, G),

  % Collect all recently deduced triples.
  setoff(
    [S2,P2,O2,G],
    (
      rdf_mat:recent_triple(S1, P1, O1, G),
      maplist(rdf_term_name([]), [S1,P1,O1], [S2,P2,O2])
    ),
    L1
  ),

  % Display the triples in an HTML table.
  html_table(
    [
      caption('The triples that were added in the last materialization run.'),
      header(true),
      indexed(true)
    ],
    [['Subject','Predicate','Object','Graph']|L1],
    DOM1
  ),

  % Collect the legend for the blank nodes that occur in
  % at least one of the recently deduced triples.
  setoff(
    [B,R2,G],
    (
      rdf_mat:recent_triple(S, _P, O, G),
      (B = S ; B = O),
      rdf_is_bnode(B),
      b2r(G, B, R1),
      rdf_term_name([], R1, R2)
    ),
    L2
  ),

  % Display the blank node mapping in an HTML table.
  html_table(
    [
      caption('The blank node mapping that is used in the above results'),
      header(true),
      indexed(true)
    ],
    [['Blank node','Mapped to','Graph']|L2],
    DOM2
  ).

%! rdf_namespaces_web(-Markup:list) is det.
% Returns a list of the currently defined namespaces in HTML markup format.
%
% @param Markup A list of HTML markup elements.

rdf_namespaces_web(Markup):-
gtrace,
  xml_current_namespaces(Namespaces),
  rdf_namespaces_web0(Namespaces, Table),
  Markup = [element(p, [], ['The currently loaded namespaces:']), Table].

%! rdf_namespaces_web(+Graph:atom, -Markup:list) is det.
% Returns a list of the namespaces that occur in a specific graph,
% in HTML markup format.
%
% @param Graph The atomic name of a graph.
% @param Markup A list of HTML markup elements.

rdf_namespaces_web(Graph, Markup):-
  rdf_current_namespaces(Graph, Namespaces),
  rdf_namespaces_web0(Namespaces, Table),
  Markup = [element(p, [], ['The namespaces in graph ', Graph, '.']), Table].

%! rdf_namespaces_web0(+Namespaces:list(atom), -Table:dom) is det.
% Returns the markup for an enumeration of the given namespaces.
%
% @param Namespaces A list of atomic names of namespaces.
% @param Markup A list of HTML markup elements.

rdf_namespaces_web0(Namespaces, Table):-
  findall(
    [Prefix,URI],
    (
      member(Prefix, Namespaces),
      rdf_current_prefix(Prefix, URI)
    ),
    L
  ),
  html_table([header(true)], [['Prefix','URI']|L], Table).

%! rdf_save_web(+Graph:atom, -DOM:list) is det.
% Saves the RDF graph with the given name from the Web interface.

rdf_save_web(Graph, Markup):-
  rdf_save2(File, [format(Format), graph(Graph)]), !,
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
        with_output_to(atom(SLabel), rdf_term_name(STerm))
      ),
      SItems
    ),
    setoff(
      option(value=PLabel,PLabel),
      (
        rdf_predicate(G, PTerm),
        with_output_to(atom(PLabel), rdf_term_name(PTerm))
      ),
      PItems
    ),
    setoff(
      option(value=OLabel,OLabel),
      (
        rdf_object(G, OTerm),
        with_output_to(atom(OLabel), rdf_term_name(OTerm))
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

rdf_web_argument(R, R):-
  is_uri(R), !.
rdf_web_argument(R1, R2):-
  R1 = _:_, !,
  rdf_global_id(R1, R2).
rdf_web_argument(R1, R4):-
  split_atom_exclusive(':', R1, [R2,R3]), !,
  rdf_web_argument(R2:R3, R4).
rdf_web_argument(R, R):-
  type_error(resource, R).

