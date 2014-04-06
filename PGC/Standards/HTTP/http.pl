:- module(
  http,
  [
    http_dateTime/1, % -DateTime:term
    serve_nothing/1, % +Request:list
    xml_serve_atom/1, % +XML:atom
    xml_serve_dom/2 % +Options:list(nvpair)
                    % +DOM:list
  ]
).

/** <module> HTTP

Predicates for sending out HTTP requests.

@author Wouter Beek
@version 2012/10, 2013/02, 2013/11, 2014/01
*/

:- use_module(library(http/http_header)).
:- use_remote_module(xml(xml_dom)).



%! http_dateTime(-DateTime:atom) is det.
% Returns a term describing the current date and time.
%
% @compat RFC 1123

http_dateTime(DateTime):-
  get_time(TimeStamp),
  http_timestamp(TimeStamp, DateTime).


serve_nothing(Request):-
  memberchk(pool(client(_, _ , _In, Out)), Request),
  http_reply_header(Out, status(no_content), []).


%! xml_serve_atom(+XML:atom) is det.
% Serves the given XML-formatted atom.

xml_serve_atom(XML):-
  % The User Agent needs to know the content type and encoding.
  % If the UTF-8 encoding is not given here explicitly,
  % Prolog throws an IO exception on `format(XML)`.
  format('Content-type: application/xml; charset=utf-8~n~n'),
  format(XML).


%! xml_serve_dom(+Options:list(nvpair), +DOM:list) is det.
% Serves the given XML DOM.
%
% The following options are supported:
%   * =|dtd(+Doctype:atom)|=
%     The atomic name of the DTD that should be used for the XML DOM.
%     The DTD is first searched for in the cache of DTD objects.
%     If the given doctype has no associated DTD in the cache,
%     it searches for a file using the file search path =dtd=.
%   * =|style(+StyleName:atom)|=
%     The atomic name of a style file on the =css= search path.

xml_serve_dom(O1, DOM):-
  xml_dom_to_atom(O1, DOM, XML),
  xml_serve_atom(XML).

