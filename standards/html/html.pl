:- module(
  html,
  [
    download_html/3, % +Options:list(nvpair)
                     % +Utl:atom
                     % -Html:dom
    file_to_html/2, % +File:atom
                    % -Html:dom
    html_link//1, % +Link:pair(url,atom)
    reply_html_file/2 % +Style:atom
                      % +File:atom
  ]
).

/** <module> HTML

Support for HTML.

### Generation

From Prolog list to HTML table.

### Parsing

HTML characters, escaping the '<' and '>' characters.

HTML atom conversion, using HTML characters.

HTML attribute parsing, used in HTML table generation.

@author Wouter Beek
@version 2012/09-2013/06, 2013/11, 2014/03, 2014/05
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(sgml)).

:- use_module(dcg(dcg_meta)).
:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(http(http_goal)).
:- use_module(pl_web(html_pl_term)).

% Assert DTD file locations.
user:file_search_path(dtd, html(.)).

% Assert the HTML file types.
user:prolog_file_type(htm, html).
user:prolog_file_type(html, html).



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
html_from_stream(HtmlDom, Stream):-
  load_html(Stream, HtmlDom, []).


%! file_to_html(+File:atom, -Html:dom) is det.
% Retrieves the HTML DOM from the file described with the given
% absolute file name.

file_to_html(File, HtmlDom):-
  open(File, read, Stream, [encoding(utf8),type(test)]),
  load_html(Stream, HtmlDom, []).


%! html_link(+Link:or([atom,pair(url,atom)]))// is det.
% Generates an HTML link.
% Does not fail on an atom as input, allowing some elements to not be linked.

html_link(URL-Label) --> !,
  html(a(href=URL, Label)).
% Also allow elements with no link.
html_link(Label) -->
  html(Label).


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

