:- module(
  xml_dom,
  [
    xml_doctype/2, % +Stream:stream
                   % -DocType
    xml_dom_as_atom//1, % +Dom:atom
    xml_dom_to_atom/3, % +Dom:list(compound)
                       % -XML:atom
                       % +Options:list(nvpair)
    xml_dom_to_file/3, % +Dom:list(compound)
                       % +File:atom
                       % +Options:list(nvpair)
    xml_file_to_dom/2, % +File:atom
                       % -Dom:list(compound)
    xml_inject_dom_with_attribute/4, % +OldDom:dom
                                     % +Class:atom
                                     % +AttributeValuePairs:list(nvpair)
                                     % -NewDom:dom
    xml_url_to_dom/2 % +Url:atom
                     % -Dom:list(compound)
  ]
).

/** <module> XML: DOM

Predicates that operate on / generate XML DOM.

@author Wouter Beek
@tbd HTTP-serve DTD files.
@version 2012/10, 2013/02-2013/05, 2013/07, 2013/09, 2013/11, 2014/03, 2014/07,
         2014/10
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_open)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).

:- use_module(generics(db_ext)).
:- use_module(os(file_ext)).
:- use_module(os(io_ext)).
:- use_module(standards(sgml_parse)).



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



%! xml_dom_as_atom(+Dom:or([atom,list(compound)]))// is det.
% Includes the given DOM inside the generated HTML page.
%
% DOM is either a list or compound term or an atom.

xml_dom_as_atom(DomAtom) -->
  {atom(DomAtom)}, !,
  html(\[DomAtom]).
xml_dom_as_atom(Dom) -->
  {xml_dom_to_atom(Dom, Xml, [])},
  xml_dom_as_atom(Xml).



%! xml_dom_to_atom(
%!   +Dom:list(compound),
%!   -Xml:atom,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%   * =|style(+StyleName:atom)|=
%     The atomic name of a style file on the =css= search path.

xml_dom_to_atom(XmlDom1, XmlAtom, Options1):-
  % Add style to XML DOM.
  (   select_option(style(StyleBase), Options1, Options2)
  ->  local_file_components(StyleLocal, StyleBase, css),
      stylesheet_pi(css(StyleLocal), PI),
      XmlDom2 = [PI|XmlDom1]
  ;   XmlDom2 = XmlDom1,
      Options2 = Options1
  ),

  % XML DOM to stream.
  merge_options([header(false)], Options2, Options3),
  setup_call_cleanup(
    tmp_file_stream(utf8, TmpFile, Out),
    % Set the header to false, since this XML content will be inserted inside
    % a Web page.
    % We do add the stylesheet parsing instruction, since this is allowed by
    % Firefox.
    xml_dom_to_stream(XmlDom2, Out, Options3),
    close(Out)
  ),

  % Stream to atom.
  setup_call_cleanup(
    open(TmpFile, read, In, [encoding(utf8),type(text)]),
    stream_to_atom(In, XmlAtom),
    (
      close(In),
      % Do not safe-delete temporary files.
      delete_file(TmpFile)
    )
  ).



%! xml_dom_to_file(
%!   +Dom:list(compound),
%!   +File:atom,
%!   +Options:list(nvpair)
%! ) is det.

xml_dom_to_file(Dom, File, Options):-
  setup_call_cleanup(
    open(File, write, Out, [encoding(utf8),type(test)]),
    xml_dom_to_stream(Dom, Out, Options),
    close(Out)
  ).


%! xml_dom_to_stream(
%!   +Dom:list(compound),
%!   +Out:stream,
%!   +Options:list(nvpair)
%! ) is det.
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%     Default: =html=

xml_dom_to_stream(Dom, Out, Options1):-
  option(dtd(Doctype), Options1, html),
  dtd(Doctype, Dtd),
  merge_options([dtd(Dtd)], Options1, Options2),
  xml_write(Out, Dom, Options2).



%! xml_file_to_dom(+File:atom, -Dom:list(compound)) is det.
% Reads the XML from the given file and return the DOM.

xml_file_to_dom(File, Dom):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    xml_stream_to_dom(Stream, Dom),
    close(Stream)
  ).



%! xml_inject_dom_with_attribute(
%!   +FromXmlDom:list(compound),
%!   +Class:atom,
%!   +AttributeValuePairs:pair,
%!   -NewXmlDom:list(compound)
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
xml_inject_dom_with_attribute([], _, _, []):- !.
xml_inject_dom_with_attribute(
  [Atom|XmlDom1],
  Class,
  AttributeValuePair,
  [Atom|XmlDom2]
):-
  atom(Atom), !,
  xml_inject_dom_with_attribute(XmlDom1, Class, AttributeValuePair, XmlDom2).
xml_inject_dom_with_attribute(
  [element(Type,Attributes1,Contents1)|XmlDom1],
  Class,
  AttributeValuePair,
  [element(Type,Attributes2,Contents2)|XmlDom2]
):-
  (
    member(class=Class, Attributes1)
  ->
    rdf_current_prefix(svg, SvgNamespace),
    member(element(SvgNamespace:title, [], [Name]), Contents1),
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
  xml_inject_dom_with_attribute(XmlDom1, Class, AttributeValuePair, XmlDom2).



%! xml_stream_to_dom(+Stream:stream, -Dom:list(compound)) is det.
% Reads the XML DOM from the given stream.

xml_stream_to_dom(Stream, Dom):-
  load_structure(
    stream(Stream),
    Dom,
    [
      dialect(xml),
      max_errors(-1),
      shorttag(false),
      space(remove),
      syntax_errors(quiet)
    ]
  ).



%! xml_url_to_dom(+Uri:uri, -Dom:list(compound)) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.

xml_url_to_dom(Uri, Dom):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(Uri, Stream, [timeout(60)]),
    % The try to make this goal succeed.
    xml_stream_to_dom(Stream, Dom),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).

