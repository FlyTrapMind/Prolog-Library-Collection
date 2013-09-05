:- module(
  xml_dom,
  [
    dom_to_xml/3, % +DTD_File:atom
                  % +DOM
                  % -XML:atom
    dom_to_xml/4, % +DTD_File:atom
                  % +StyleName:atom
                  % +DOM
                  % -XML:atom
    dom_to_xml_file/3, % +DTD_Name:atom,
                       % +DOM:list
                       % +Out:oneof([atom,stream])
    dom_to_xml_file/4, % +DTD_Name:atom,
                       % +DOM:list
                       % +Out:oneof([atom,stream])
                       % +Options:list(nvpair)
    file_to_xml/2, % +File:atom
                   % -XML:dom
    stream_to_xml/2, % +Stream:stream
                     % -XML:dom
    stylesheet_pi/2, % +CSS_FileSpecification
                     % -PI:atom
    stylesheet_pi/3, % +Type:oneof(['text/css'])
                     % +CSS_FileSpecification
                     % -PI:atom
    uri_to_xml/2, % +URI:uri
                  % -XML:dom
    xml_doctype/2, % +Stream:stream
                   % -DocType
    xml_inject_dom_with_attribute/4 % +OldDOM:dom
                                    % +Class:atom
                                    % AttributeValuePairs:list(nvpair)
                                    % -NewDOM:dom
  ]
).

/** <module> XML_DOM

Predicates that operate on / generate XML DOM.

@author Wouter Beek
@version 2012/10, 2013/02-2013/05, 2013/07
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(os(file_ext)).
:- use_module(os(io_ext)).
:- use_module(standards(sgml_parse)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso, 'http://www.iso.org/').
:- xml_register_namespace(std, 'http://www.example.org/standards/').
:- xml_register_namespace(w3c, 'http://www.w3.org/').

:- db_add_novel(user:prolog_file_type(css, css)).
:- db_add_novel(user:prolog_file_type(dtd, dtd)).
:- db_add_novel(user:prolog_file_type(xml, xml)).

% Serve CSS files.
:- db_add_novel(http:location(css, root(css),  [])).
:- db_add_novel(user:file_search_path(css, server(css))).
:- http_handler(
  css(.),
  serve_files_in_directory(css),
  [prefix, priority(10)]
).



%! dom_to_xml(+DTD_Name:atom, +DOM:list, -XML:atom) is det.
% @see dom_to_xml/4

dom_to_xml(DTD_Name, DOM, XML):-
  tmp_file_stream(utf8, TemporaryFile, Out),
  % Set the header to false, since this XML content will be inserted inside
  % a Web page.
  % We do add the stylesheet parsing instruction, since this is allowed by
  % Firefox.
  dom_to_xml_file(DTD_Name, DOM, Out, [header(false)]),
  setup_call_cleanup(
    open(TemporaryFile, read, In, [encoding(utf8),type(text)]),
    stream_to_atom(In, XML),
    (
      close(In),
      % Do not safe-delete temporary files.
      delete_file(TemporaryFile)
    )
  ).

%! dom_to_xml(+DTD_Name:atom, +Style_Name:atom, +DOM:list, -XML:atom) is det.
% Translates DOM to XML, applying DTD checks and Style decoration.
%
% @param DTD_Name The atomic name of a DTD file. File locations that
%   contain DTD files must be asserted using
%   =|file_search_path/3|=.

dom_to_xml(DTD_Name, Style_Name, DOM, XML):-
  file_name_type(Style_Name, css, Style),
  stylesheet_pi(css(Style), PI),
  dom_to_xml(DTD_Name, [PI | DOM], XML).

%! dom_to_xml_file(
%!   +DTD_Name:atom,
%!   +DOM:list,
%!   +Out:oneof([atom,stream])
%! ) is det.

dom_to_xml_file(DTD_Name, DOM, Out):-
  dom_to_xml_file(DTD_Name, DOM, Out, []).

%! dom_to_xml_file(
%!   +DTD_Name:atom,
%!   +DOM:list,
%!   +Out:oneof([atom,stream]),
%!   +Options:list(nvpair)
%! ) is det.

dom_to_xml_file(DTD_Name, DOM, Out, Options1):-
  is_stream(Out), !,
  new_dtd(DTD_Name, DTD),
  % Retrieve the first DTD file with the given name.
  dtd_file(DTD_Name, DTD_File),
  load_dtd(DTD, DTD_File),
  merge_options([dtd(DTD)], Options1, Options2),
  xml_write(Out, DOM, Options2),
  close(Out),
  free_dtd(DTD).
dom_to_xml_file(DTD_Name, DOM, File, Options):-
  open(File, write, Out, [encoding(utf8),type(test)]),
  dom_to_xml_file(DTD_Name, DOM, Out, Options).

%! dtd_file(+Name:atom, -File:atom) is det.
% Returns the first DTD file with the given name or throws an
% existence error.
%
% @param Name The atomic name of a DTD file.
% @param File The atomic name of the path of a DTD file.

dtd_file(Name, File):-
  % By setting option =solutions= to value =all= we backtrack over
  % the various file search paths that are defined for =dtd=.
  once(
    absolute_file_name(
      dtd(Name),
      File,
      [access(read), file_type(dtd), solutions(all)]
    )
  ).

%! file_to_xml(+File:atom, -XML:dom) is det.
% Reads the XML from the given file and return the DOM.

file_to_xml(File, XML):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    stream_to_xml(Stream, XML),
    close(Stream)
  ).

%! stream_to_xml(+Stream:stream, -XML:dom) is det.
% Reads the XML DOM from the given stream.

stream_to_xml(Stream, XML):-
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

stylesheet_pi(CSS_FileSpecification, PI):-
  stylesheet_pi('text/css', CSS_FileSpecification, PI).

stylesheet_pi(Type, CSS_FileSpecification, pi(PI)):-
  http_absolute_location(CSS_FileSpecification, CSS_File, []),
  format(atom(PI), 'xml-stylesheet type="~w" href="~w"', [Type, CSS_File]).

%! uri_to_xml(+URI:uri, -XML:dom) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.

uri_to_xml(URI, XML):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(URI, Stream, [timeout(60)]),
    % The try to make this goal succeed.
    stream_to_xml(Stream, XML),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).

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
          max_errors(1),
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

on_begin(Tag, Attributes, _Parser):-
  memberchk(xmlns:_=_, Attributes),
  throw(tag(Tag)).

on_cdata(_CDATA, _Parser):-
  throw(error(cdata)).

%! xml_inject_dom_with_attribute(
%!   +OldDOM:dom,
%!   +Class:atom,
%!   +AttributeValuePairs:pair,
%!   -NewDOM:dom
%! ) is det.
% Sometimes we use a DOM that is generated by some external application.
% This DOM may then miss some attribute that we want to amplify it with.
%
% For instance, when GraphViz generated an SVG graph representation, we
% want to add an onclick attribute to add user interaction.
%
% For these cases we want to travese the entire DOM tree and insert a
% given list of attribute-value pairs into the elements that have the
% given class.

% onclick="function(){...};"
xml_inject_dom_with_attribute([], _Class, _AttributeValuePair, []):- !.
xml_inject_dom_with_attribute(
  [Atom | DOM1],
  Class,
  AttributeValuePair,
  [Atom | DOM2]
):-
  atom(Atom), !,
  xml_inject_dom_with_attribute(DOM1, Class, AttributeValuePair, DOM2).
xml_inject_dom_with_attribute(
  [element(Type, Attributes1, Contents1) | DOM1],
  Class,
  AttributeValuePair,
  [element(Type, Attributes2, Contents2) | DOM2]
):-
  if_then_else(
    member(class=Class, Attributes1),
    (
      xml_current_namespace(svg, SVG_Namespace),
      member(element(SVG_Namespace:title, [], [Name]), Contents1),
      format(atom(Function), 'clickme(\'~w\')', [Name]),
      Attributes2 = [onclick=Function | Attributes1]
    ),
    Attributes2 = Attributes1
  ),
  xml_inject_dom_with_attribute(Contents1, Class, AttributeValuePair, Contents2),
  xml_inject_dom_with_attribute(DOM1, Class, AttributeValuePair, DOM2).

