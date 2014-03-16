:- module(
  xml_dom,
  [
    xml_doctype/2, % +Stream:stream
                   % -DocType
    xml_dom_to_atom/3, % +Options:list(nvpair)
                       % +XML_DOM:list
                       % -XML:atom
    xml_dom_to_file/3, % +Options:list(nvpair)
                       % +XML_DOM:list
                       % +File:atom
    xml_file_to_dom/2, % +File:atom
                       % -XML:dom
    xml_inject_dom_with_attribute/4, % +OldDOM:dom
                                     % +Class:atom
                                     % AttributeValuePairs:list(nvpair)
                                     % -NewDOM:dom
    xml_url_to_dom/2 % +URI:uri
                     % -XML_DOM:list
  ]
).

/** <module> XML DOM

Predicates that operate on / generate XML DOM.

@author Wouter Beek
@tbd HTTP-serve DTD files.
@version 2012/10, 2013/02-2013/05, 2013/07, 2013/09, 2013/11
*/

:- use_module(generics(db_ext)).
:- use_module(html(html)). % This is required for the HTML DTD file path.
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(os(file_ext)).
:- use_module(os(io_ext)).
:- use_module(standards(sgml_parse)).
:- use_module(uri(rfc3987_dcg)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(svg, 'http://www.w3.org/2000/svg').

:- db_add_novel(user:prolog_file_type(css, css)).
:- db_add_novel(user:prolog_file_type(dtd, dtd)).
:- db_add_novel(user:prolog_file_type(xml, xml)).

% /css
:- db_add_novel(http:location(css, root(css),  [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix,priority(10)]).



on_begin(Tag, Attributes, _Parser):-
  memberchk(xmlns:_=_, Attributes),
  throw(tag(Tag)).

on_cdata(_CDATA, _Parser):-
  throw(error(cdata)).

stylesheet_pi(CSS_FileSpecification, PI):-
  stylesheet_pi('text/css', CSS_FileSpecification, PI).

stylesheet_pi(Type, CSS_FileSpecification, pi(PI)):-
  http_absolute_location(CSS_FileSpecification, CSS_File, []),
  format(atom(PI), 'xml-stylesheet type="~w" href="~w"', [Type,CSS_File]).

%! xml_doctype(+Stream, -DocType) is semidet.
% Parse a _repositional_ stream and get the  name of the first XML
% element *and* demand that this element defines XML namespaces.
% Fails if the document is illegal XML before the first element.
%
% Note that it is not possible to define valid RDF/XML without
% namespaces, while it is not possible to define a valid absolute
% Turtle URI (using <URI>) with a valid xmlns declaration.
%
% @author Jan Wielemaker
% @version 2011

xml_doctype(Stream, DocType):-
  catch(
    setup_call_cleanup(
      make_parser(Stream, Parser, State),
      sgml_parse(
        Parser,
        [
          call(begin, on_begin),
          call(cdata, on_cdata),
          max_errors(10),
          source(Stream),
          syntax_errors(quiet)
        ]
      ),
      cleanup_parser(Stream, Parser, State)
    ),
    Exception,
    true
  ),
  nonvar(Exception),
  Exception = tag(DocType).

%! xml_dom_to_atom(+Options:list(nvpair), +DOM:list, -XML:atom) is det.
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%   * =|style(+StyleName:atom)|=
%     The atomic name of a style file on the =css= search path.

xml_dom_to_atom(O1, DOM1, XML):-
  % Add style to XML DOM.
  (
    select_option(style(StyleName), O1, O2)
  ->
    file_name_type(StyleName, css, StyleFile),
    stylesheet_pi(css(StyleFile), PI),
    DOM2 = [PI|DOM1]
  ;
    DOM2 = DOM1,
    O2 = O1
  ),

  % XML DOM to stream.
  setup_call_cleanup(
    tmp_file_stream(utf8, TmpFile, Out),
    % Set the header to false, since this XML content will be inserted inside
    % a Web page.
    % We do add the stylesheet parsing instruction, since this is allowed by
    % Firefox.
    xml_dom_to_stream([header(false)|O2], DOM2, Out),
    close(Out)
  ),

  % Stream to atom.
  setup_call_cleanup(
    open(TmpFile, read, In, [encoding(utf8),type(text)]),
    stream_to_atom(In, XML),
    (
      close(In),
      % Do not safe-delete temporary files.
      delete_file(TmpFile)
    )
  ).

%! xml_dom_to_file(+Options:list(nvpair), +DOM:list, +File:atom) is det.

xml_dom_to_file(O1, DOM, File):-
  setup_call_cleanup(
    open(File, write, OutputStream, [encoding(utf8),type(test)]),
    xml_dom_to_stream(O1, DOM, OutputStream),
    close(OutputStream)
  ).

%! xml_dom_to_stream(
%!   +Options:list(nvpair),
%!   +XML_DOM:list,
%!   +OutputStream:stream
%! ) is det.
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%     Default: =html=

xml_dom_to_stream(O1, DOM, OutputStream):-
  option(dtd(Doctype), O1, html),
  dtd(Doctype, DTD),
  merge_options([dtd(DTD)], O1, O2),
  xml_write(OutputStream, DOM, O2).

%! xml_file_to_dom(+File:atom, -XML:dom) is det.
% Reads the XML from the given file and return the DOM.

xml_file_to_dom(File, XML):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    xml_stream_to_dom(Stream, XML),
    close(Stream)
  ).

%! xml_inject_dom_with_attribute(
%!   +OldDOM:dom,
%!   +Class:atom,
%!   +AttributeValuePairs:pair,
%!   -NewDOM:dom
%! ) is det.
% Sometimes we use a DOM that is generated by some external application.
% This DOM may then miss some attribute that we want to amplify it with.
%
% For instance, when GraphViz generates an SVG graph representation,
% we may want to add an `onclick` attribute to add some user interaction.
%
% For these cases we want to travese the entire DOM tree and insert a
% given list of attribute-value pairs into the elements that have the
% given class.

% onclick="function(){...};"
xml_inject_dom_with_attribute([], _Class, _AttributeValuePair, []):- !.
xml_inject_dom_with_attribute(
  [Atom|DOM1],
  Class,
  AttributeValuePair,
  [Atom|DOM2]
):-
  atom(Atom), !,
  xml_inject_dom_with_attribute(DOM1, Class, AttributeValuePair, DOM2).
xml_inject_dom_with_attribute(
  [element(Type,Attributes1,Contents1)|DOM1],
  Class,
  AttributeValuePair,
  [element(Type,Attributes2,Contents2)|DOM2]
):-
  (
    member(class=Class, Attributes1)
  ->
    xml_current_namespace(svg, SVG_Namespace),
    member(element(SVG_Namespace:title, [], [Name]), Contents1),
    format(atom(Function), 'clickme(\'~w\')', [Name]),
    Attributes2 = [onclick=Function|Attributes1]
  ;
    Attributes2 = Attributes1
  ),
  xml_inject_dom_with_attribute(
    Contents1,
    Class,
    AttributeValuePair,
    Contents2
  ),
  xml_inject_dom_with_attribute(DOM1, Class, AttributeValuePair, DOM2).

%! xml_stream_to_dom(+Stream:stream, -XML:dom) is det.
% Reads the XML DOM from the given stream.

xml_stream_to_dom(Stream, XML):-
  load_structure(
    stream(Stream),
    XML,
    [
      dialect(xml),
      max_errors(-1),
      shorttag(false),
      space(remove),
      syntax_errors(quiet)
    ]
  ).

%! xml_url_to_dom(+URI:uri, -XML:dom) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.

xml_url_to_dom(URI, XML):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(URI, Stream, [timeout(60)]),
    % The try to make this goal succeed.
    xml_stream_to_dom(Stream, XML),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).

