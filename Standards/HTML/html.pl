:- module(
  html,
  [
    html_link//1, % +Link:pair(url,atom)
    html_pair//2, % +Element1
                  % +Element2
    html_pair//3, % :WriteMethod
                  % +Element1
                  % +Element2
    html_quadruple//4, % +Element1
                       % +Element2
                       % +Element3
                       % +Element4
    html_quadruple//5, % :WriteMethod
                       % +Element1
                       % +Element2
                       % +Element3
                       % +Element4
    html_triple//3, % +Element1
                    % +Element2
                    % +Element3
    html_triple//4, % :WriteMethod
                    % +Element1
                    % +Element2
                    % +Element3
    html_tuple//1, % +Elements:list
    html_tuple//2, % :WriteMethod
                   % +Elements:list
    reply_html_file/2, % +Style:atom
                       % +File:atom

% FETCHING
    file_to_html/2, % +File:atom
                    % -HTML:dom
    download_html/3, % +Options:list(nvpair)
                     % +URL:atom
                     % -HTML:dom

% PARSING
    html_attribute/2, % +Attributes:list(nvpair)
                      % +Attribute:nvpair
    parse_attributes_html/3 % +Context:oneof([table])
                            % +Attributes:list(nvpair)
                            % -ParsedAttributes:list(nvassignment)
  ]
).

/** <module> HTML

Support for HTML.

# Generation

From Prolog list to HTML table.

# Parsing

HTML characters, escaping the '<' and '>' characters.

HTML atom conversion, using HTML characters.

HTML attribute parsing, used in HTML table generation.

@author Wouter Beek
@version 2012/09-2013/06, 2013/11, 2014/03
*/

:- use_module(dcg(dcg_meta)).
:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(html(pl_term_html)).
:- use_module(http(http)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(sgml)).

% Assert DTD file locations.
:- db_add_novel(user:file_search_path(dtd, html(.))).

% Assert the HTML file types.
:- db_add_novel(user:prolog_file_type(htm, html)).
:- db_add_novel(user:prolog_file_type(html, html)).

:- meta_predicate(html_pair(3,+,+,?,?)).
:- rdf_meta(html_pair(:,r,r,?,?)).
:- meta_predicate(html_quadruple(3,+,+,+,+)).
:- rdf_meta(html_quadruple(:,r,r,r,r)).
:- meta_predicate(html_triple(3,+,+,+,?,?)).
:- rdf_meta(html_triple(:,r,r,r,?,?)).
:- meta_predicate(html_tuple(3,+,?,?)).
:- meta_predicate(html_tuple_(+,3,+,?,?)).
:- rdf_meta(html_tuple(:,t,?,?)).



%! html_link(+Link:or([atom,pair(url,atom)]))// is det.
% Generates an HTML link.
% Does not fail on an atom as input, allowing some elements to not be linked.

html_link(URL-Label) --> !,
  html(a(href=URL, Label)).
% Also allow elements with no link.
html_link(Label) -->
  html(Label).


%! html_pair(+Element1, +Element2)// is det.
% @see html_pair//3

html_pair(E1, E2) -->
  html_pair(pl_term_html, E1, E2).

%! html_pair(:WriteMethod, +Element1, +Element2)// is det.
% Generates an HTML representation for a pair.

html_pair(Dcg, E1, E2) -->
  html_tuple_(pair, Dcg, [E1,E2]).


%! html_quadruple(+Element1, +Elememt2, +Element3, +Element4)// is det.
% @see html_quadruple//5

html_quadruple(E1, E2, E3, E4) -->
  html_quadruple(pl_term_html, E1, E2, E3, E4).

%! html_quadruple(:Dcg, +Element1, +Elememt2, +Element3, +Element4)// is det.
% Generates an HTML representation for a triple.

html_quadruple(Dcg, E1, E2, E3, E4) -->
  html_tuple_(triple, Dcg, [E1,E2,E3,E4]).


%! html_triple(:Dcg, +Elements:list)// is det.
% @see html_tripl//2

html_triple(E1, E2, E3) -->
  html_triple(pl_term_html, E1, E2, E3).

%! html_triple(:Dcg, +Elements:list)// is det.
% Generates an HTML representation for a triple.

html_triple(Dcg, E1, E2, E3) -->
  html_tuple_(triple, Dcg, [E1,E2,E3]).


%! html_tuple(+Elements:list)// is det.
% @see html_tuple//2

html_tuple(L) -->
  html_tuple(pl_term_html, L).

%! html_tuple(:Dcg, +Elements:list)// is det.
% Generates an HTML representation for a tuple.

html_tuple(Dcg, L) -->
  html_tuple_(tuple, Dcg, L).

html_tuple_(Class, Dcg, L) -->
  html(
    span(class=Class, [
      &(lang),
      \html_tuple_elements(Dcg, L),
      &(rlang)
    ])
  ).

html_tuple_elements(_, []) --> !, [].
html_tuple_elements(Dcg, [H]) --> !,
  html(span(class=element, \dcg_call(Dcg, H))).
html_tuple_elements(Dcg, [H|T]) -->
  html([
    span(class=element, \dcg_call(Dcg, H)),
    ',',
    \html_tuple_elements(Dcg, T)
  ]).


%! reply_html_file(+Style:atom, +File:atom) is det.
% Serve the given HTML file using the given styling.
%
% @arg Style The atomic name of the HTML style of the page served.
%        This style has to be defined using the multifile
%        preficates user:body//2 and user:head//2.
% @arg File The atomic base name of the HTML file that is served.

reply_html_file(Style, File):-
  absolute_file_name(stcn_html(File), HTML, [access(read),file_type(html)]),
  load_html_file(HTML, DOM),
  contains_term(element(body, _, Body), DOM),
  reply_html_page(Style, [], Body).



% FETCHING %

%! file_to_html(+File:atom, -HTML:dom) is det.
% Retrieves the HTML DOM from the file described with the given
% absolute file name.

file_to_html(File, HTML_DOM):-
  open(File, read, Stream, [encoding(utf8),type(test)]),
  html_from_stream(HTML_DOM, Stream).


% html_from_stream(-HTML:dom, +Stream:stream) is det.
% Retrieves the HTML DOM from the given stream.
%
% @throws Limit exceeded exception due to >50 errors.
%         =|error(limit_exceeded(max_errors, Max), _)|=

html_from_stream(HTML_DOM, Stream):-
  load_html(Stream, HTML_DOM, []).


%! download_html(+Options:list(nvpoair), +Url:atom, -Html:dom) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.
%
% The following options are supported:
%   * =|html_dialect(+HtmlDialect:oneof([html4,html5])|=
%   * Other options are passed to http_goal/3 and, subsequently, http_open/3.

download_html(O1, Url, HtmlDom):-
  select_option(html_dialect(HtmlDialect), O1, O2, html5),
  temporarily_set_flag(
    html_dialect,
    HtmlDialect,
    http_goal(Url, O2, html_from_stream(HtmlDom))
  ).



% PARSING %

% This attribute specifies the width (in pixels only) of the frame around a
% table (see the Note below for more information about this attribute).
% @tbd Deprecated, use CSS2 instead.
attribute(border, pixels, [table]).

%! html_attribute(+Attributes:list(nvpair), +Attribute:nvpair) is nondet.
% Succeeds (semidet) or instantiates (nondet) the given attribute within
% the given attributes list.
%
% This predicate is typically used to extract the value belonging to a
% certain attribute name from a given set of attribute-value pairs that
% occurs in a DOM element/3 term.
%
% This predicate uses the swipl options library.
% In accordance with this, =Attribute= can be either of the form
% =|Name(Value)|= or =|Name=Value|=.

html_attribute(Attributes, Attribute):-
  memberchk(Attribute, Attributes).

parse_attribute(Context, Attribute, Name=Value):-
  Attribute =.. [Name, Value],
  attribute(Name, Type, Contexts),
  memberchk(Context, Contexts), !,
  html_typecheck(Type, Value).

parse_attributes_html(Context, Attributes, ParsedAttributes):-
  maplist(parse_attribute(Context), Attributes, ParsedAttributes).

html_typecheck(pixels, Value):-
  html_typecheck(integer, Value), !.
html_typecheck(Type, Value):-
  must_be(Type, Value).

