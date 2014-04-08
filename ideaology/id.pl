:- module(id, []).

/** <module> IDEAology

@author Wouter Beek
@version 2014/03
*/

:- use_remote_module(generics(db_ext)).
:- use_remote_module(generics(uri_query)).
:- use_remote_module(gv(gv_file)).
:- use_remote_module(latex(latex_to_html)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(xml(xml_dom)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(id, 'http://www.wouterbeek.com/IDEAology/').

http:location(id, root(id), []).
:- http_handler(root(id), id, [prefix,priority(0)]).

user:web_module('IDEAology', id).



% Node.
id(Request):-
  request_query_read(Request, node, Id), !,
  id(Id, Title, DCG, _),
  reply_html_page(app_Style, title(Title), \(DCG)).
% Network.
id(_Request):-
  findall(
    vertex(Id, Id, [label(Label),'URL'(Location2)]),
    (
      id(Id, Title, _, _),
      atomic_list_concat(['<<b>',Title,'</b>>'], Label),
      http_absolute_uri(id(.), Location1),
      uri_query_add(Location1, node, Id, Location2)
    ),
    Vs
  ),
  findall(
    edge(Id1, Id2, []),
    (
      id(Id2, _, _, Id1s),
      member(Id1, Id1s)
    ),
    Es
  ),
  GAttrs = [charset('UTF-8'),directedness(forward),overlap(false)],
  graph_to_svg_dom([method(dot)], graph(Vs,Es,GAttrs), SvgDom),
  reply_html_page(
    app_style,
    title('IDEAology'),
    html(\xml_dom_as_atom(SvgDom))
  ).


%! id(?Id:atom, -Title:atom, :DCG, -Antecedents:ordset(atom)) is nondet.

id(0, 'Test',       test,       []   ).
id(1, 'Cyber cat',  cyber_cat,  [0]  ).
id(2, 'Blank node', blank_node, [0,1]).

cyber_cat -->
  html(p('BLITZZZ!')).

test --> [].

blank_node -->
  {absolute_file_name(data(blank_node), File, [access(read),file_type(latex)])},
  latex_to_html(File).

