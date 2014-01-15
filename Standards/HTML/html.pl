:- module(
  html,
  [
    reply_html_file/2, % +Style:atom
                       % +File:atom

% FETCHING
    file_to_html/2, % +File:atom
                    % -HTML:list
    url_to_html/2, % +URL:atom
                   % -HTML:list

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
@version 2012/09-2013/06, 2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(generics(typecheck)).
:- use_module(http(http)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).

% Assert DTD file locations.
:- db_add_novel(user:file_search_path(dtd, html(.))).

% Assert the HTML file types.
:- db_add_novel(user:prolog_file_type(htm, html)).
:- db_add_novel(user:prolog_file_type(html, html)).

:- meta_predicate(html_process_exception(+,0)).



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

%! process_exception(+Exception, :Goal) is det.
% Processes an exception thrown by load_structure/3

% Retry after a while upon exceeding a limit.
% Thrown by load_structure/3.
html_process_exception(error(limit_exceeded(max_errors, Max), Context), Goal):- !,
  debug(html, 'Encountered ~w error(s) while parsing HTML.', [Max]),
  debug(html, 'Context:\t~w', [Context]),
  sleep(10),
  call(Goal).
html_process_exception(Exception, _Goal):-
  debug(html, '!UNRECOGNIZED EXCEPTION! ~w', [Exception]).

% html_from_stream(-HTML:dom, +Stream:stream) is det.
% Retrieves the HTML DOM from the given stream.
%
% @throws Limit exceeded exception due to >50 errors.
%         =|error(limit_exceeded(max_errors, Max), _)|=

html_from_stream(HTML_DOM, Stream):-
  html_from_stream(HTML_DOM, 0, Stream).

html_from_stream(_, 5, _):- !,
  debug(
    html,
    'The maximum number of attempts was reached for load_structure/3 \c
     in html_from_stream/3.',
    []
  ).
html_from_stream(HTML_DOM, Attempts, Stream):-
  dtd(html, HTML_DTD),
  catch(
    load_structure(
      stream(Stream),
      HTML_DOM,
      [
        dtd(HTML_DTD),
        dialect(xmlns),
        max_errors(-1),
        shorttag(true),
        space(remove),
        syntax_errors(quiet)
      ]
    ),
    Exception,
    (
      NewAttempts is Attempts + 1,
      html_process_exception(
        Exception,
        html_from_stream(HTML_DOM, NewAttempts, Stream)
      )
    )
  ).

%! url_to_html(+URL:atom, -HTML_DOM:list) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.
%
% @arg URI
% @arg HTML_DOM
% @throws existence_error(url, Id)

url_to_html(URL, HTML_DOM):-
  http_goal(URL, [], html_from_stream(HTML_DOM)).



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

