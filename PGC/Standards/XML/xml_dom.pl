:- module(
  xml_dom,
  [
    xml_doctype/2, % +Stream:stream
                   % -DocType
    xml_dom_as_atom//1, % +DOM:atom
    xml_dom_to_atom/3, % +Options:list(nvpair)
                       % +XmlDom:list
                       % -XML:atom
    xml_dom_to_file/3, % +Options:list(nvpair)
                       % +XmlDom:list
                       % +File:atom
    xml_file_to_dom/2, % +File:atom
                       % -XmlDom:list(compound)
    xml_inject_dom_with_attribute/4, % +OldDOM:dom
                                     % +Class:atom
                                     % AttributeValuePairs:list(nvpair)
                                     % -NewDOM:dom
    xml_url_to_dom/2 % +URI:uri
                     % -XmlDom:list(compound)
  ]
).

/** <module> XML DOM

Predicates that operate on / generate XML DOM.

@author Wouter Beek
@tbd HTTP-serve DTD files.
@version 2012/10, 2013/02-2013/05, 2013/07, 2013/09, 2013/11, 2014/03
*/

:- use_module(generics(db_ext)).
:- use_module(html(html)). % This is required for the HTML DTD file path.
:- use_module(library(http/html_write)).
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
http:location(css, root(css),  []).
user:file_search_path(css, server(css)).
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


%! xml_dom_as_atom(+Dom:or([atom,list(compound)]))// is det.
% Includes the given DOM inside the generated HTML page.
%
% DOM is either a list or compound term or an atom.

xml_dom_as_atom(DomAtom) -->
  {atom(DomAtom)}, !,
  html(\[DomAtom]).
xml_dom_as_atom(Dom) -->
  {xml_dom_to_atom([], Dom, DomAtom)},
  xml_dom_as_atom(DomAtom).


%! xml_dom_to_atom(
%!   +Options:list(nvpair),
%!   +XmlDom:list(compound),
%!   -XmlAtom:atom
%! ) is det.
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%   * =|style(+StyleName:atom)|=
%     The atomic name of a style file on the =css= search path.

xml_dom_to_atom(O1, XmlDom1, XmlAtom):-
  % Add style to XML DOM.
  (
    select_option(style(StyleName), O1, O2)
  ->
    file_name_type(StyleName, css, StyleFile),
    stylesheet_pi(css(StyleFile), PI),
    XmlDom2 = [PI|XmlDom1]
  ;
    XmlDom2 = XmlDom1,
    O2 = O1
  ),

  % XML DOM to stream.
  setup_call_cleanup(
    tmp_file_stream(utf8, TmpFile, Out),
    % Set the header to false, since this XML content will be inserted inside
    % a Web page.
    % We do add the stylesheet parsing instruction, since this is allowed by
    % Firefox.
    xml_dom_to_stream([header(false)|O2], XmlDom2, Out),
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
%!   +Options:list(nvpair),
%!   +XmlDom:list(compound),
%!   +File:atom
%! ) is det.

xml_dom_to_file(O1, XmlDom, File):-
  setup_call_cleanup(
    open(File, write, OutputStream, [encoding(utf8),type(test)]),
    xml_dom_to_stream(O1, XmlDom, OutputStream),
    close(OutputStream)
  ).


%! xml_dom_to_stream(
%!   +Options:list(nvpair),
%!   +XmlDom:list(compound),
%!   +OutputStream:stream
%! ) is det.
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%     Default: =html=

xml_dom_to_stream(O1, XmlDom, OutputStream):-
  option(dtd(Doctype), O1, html),
  dtd(Doctype, Dtd),
  merge_options([dtd(Dtd)], O1, O2),
  xml_write(OutputStream, XmlDom, O2).


%! xml_file_to_dom(+File:atom, -XmlDom:list(compound)) is det.
% Reads the XML from the given file and return the DOM.

xml_file_to_dom(File, XmlDom):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    xml_stream_to_dom(Stream, XmlDom),
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
  xml_inject_dom_with_attribute(XmlDom1, Class, AttributeValuePair, XmlDom2).


%! xml_stream_to_dom(+Stream:stream, -XmlDom:list(compound)) is det.
% Reads the XML DOM from the given stream.

xml_stream_to_dom(Stream, XmlDom):-
  load_structure(
    stream(Stream),
    XmlDom,
    [
      dialect(xml),
      max_errors(-1),
      shorttag(false),
      space(remove),
      syntax_errors(quiet)
    ]
  ).


%! xml_url_to_dom(+URI:uri, -XmlDom:list(compound)) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.

xml_url_to_dom(URI, XmlDom):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(URI, Stream, [timeout(60)]),
    % The try to make this goal succeed.
    xml_stream_to_dom(Stream, XmlDom),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).

