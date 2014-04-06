:- module(
  visualizer,
  [
    statistics_db/0,
    visualize_dot/0,
    visualize_svg/0
  ]
).

/** <module> Visualizer

Visualizer predicates.

@author Wouter Beek
@version 2012/09
*/

:- use_module(library(aggregate)).
:- use_remote_module(os(file_ext)).
:- use_remote_module(server(link_collection)).



statistics_db:-
  aggregate_all(
    set(Indegree/Outdegree/Link),
    (
      site(Link),
      indegree(Link, Indegree),
      outdegree(Link, Outdegree)
    ),
    Triples
  ),
  reverse(Triples, Triples_),
  Triples_ = [S1, S2, S3, S4, S5 | _Triples_],
  forall(
    member(Indegree/Outdegree/Link, [S1, S2, S3, S4, S5]),
    format(
      'Site: ~w:\n\tIndegree: ~w\n\tOutdegree: ~w\n',
      [Link, Indegree, Outdegree]
    )
  ).

visualize_dot:-
  flag(visualize_id, ID, ID + 1),
  format(atom(Name), 'visualize_~w', [ID]),
  user:file_search_path(debug, Directory),
  create_file(Directory, Name, graphviz, File),
  open(
    File,
    write,
    _Stream,
    [alias(prasem), close_on_abort(true), type(text)]
  ),
  format(prasem, 'digraph circuit {\n', []),
  forall(
    site(Site),
    format(prasem, '  "~w"\n', [Site])
  ),
  format(prasem, '\n', []),
  forall(
    link(Site1, Site2),
    format(prasem, '  "~w" -> "~w"\n', [Site1, Site2])
  ),
  format(prasem, '}\n', []),
  close(prasem).

visualize_svg:-
  user:file_search_path(debug, Directory),
  create_file(Directory, temp, rdf, RDF_File),
  rdf_save([format(rdf_xml)], prasem, RDF_File),
  file_to_atom(RDF_File, RDF),
  rdf_to_svg(RDF, SVG),
  create_file(Directory, temp, scalable_vector_graphics, SVG_File),
  atom_to_file(SVG, SVG_File).

rdf_to_svg(RDF, SVG):-
  atomic_list_concat(
    [
'<?xml version="1.0" standalone="yes"?>
<svg width="4in" height="3in" version="1.1" xmlns = "http://www.w3.org/2000/svg">
  <metadata>',
      RDF,
'  </metadata>
</svg>'
    ],
    SVG
  ).
