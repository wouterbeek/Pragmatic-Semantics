:- module(tms_web, []).

/** <module> TMS web

Web-interface for truth maintenance systems.

@author Wouter Beek
@version 2013/10-2014/01
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(meta_ext)).
:- use_remote_module(generics(uri_query)).
:- use_remote_module(gv(gv_file)).
:- use_remote_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(server(web_console)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(server(web_ui)).
:- use_remote_module(tms(tms)).
:- use_remote_module(tms(tms_export)).
:- use_remote_module(xml(xml_dom)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').

http:location(tms, root(tms), []).
:- http_handler(root(tms), tms_web, [prefix]).

user:web_module('TMS', tms_web).



%! tms(+Request:list) is det.
% TMS graph navigation Web pages.

% A graph representation of the given TMS node.
tms_web(Request):-
  request_query_read(Request, node, NLocal), !,
  
  % From TMS node name to TMS name denoted by that name.
  rdf_global_id(doyle:NLocal, N),
  
  % From TMS node to SVG DOM.
  http_absolute_uri(tms(.), BaseURL),
  tms_export_node([base_url(BaseURL),recursive(false)], N, GIF),
  graph_to_svg_dom([method(dot)], GIF, SvgDom),
  
  % Insert SVG DOM into Web page.
  reply_html_page(
    app_style,
    title(['TMS node ',NLocal]),
    \xml_dom_as_atom(SvgDom)
  ).
% A graph representation of the given TMS.
tms_web(Request):-
  request_query_read(Request, tms, TMS), !,
  
  % From TMS to SVG DOM representation.
  http_absolute_uri(tms(.), BaseURL),
  tms_export_graph([base_url(BaseURL)], TMS, GIF),
  graph_to_svg_dom([method(sfdp)], GIF, SvgDom),
  
  % Insert SVG DOM into Web page.
  reply_html_page(
    app_style,
    title(['TMS ',TMS]),
    \xml_dom_as_atom(SvgDom)
  ).
% A table of all TMS-es.
tms_web(_Request):-
  findall(
    [TMS_URL-TMS,Type,NumberOfJs,NumberOfNs],
    (
      tms(Type, TMS),
      http_absolute_uri(tms(.), BaseURL),
      uri_query_add(BaseURL, tms, TMS, TMS_URL),
      aggregate_all(
        set(J),
        tms_justification(TMS, J),
        Js
      ),
      length(Js, NumberOfJs),
      aggregate_all(
        set(N),
        tms_node(TMS, N),
        Ns
      ),
      length(Ns, NumberOfNs)
    ),
    Rows
  ),
  reply_html_page(
    app_style,
    title('Overerview of TMSs'),
    \html_table(
      [header_row(true),indexed(true)],
      html('The currently loaded Truth Maintenance Systems.'),
      [['TMS','Type','#Justifications','#Nodes']|Rows]
    )
  ).

