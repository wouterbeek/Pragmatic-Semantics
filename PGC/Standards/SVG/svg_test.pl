:- module(svg_test, []).

/** <module> SVG unit tests

@author Wouter Beek
@version 2013/07-2013/09
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(print_ext)).
:- use_module(gv(gv_file)).
:- use_module(library(plunit)).
:- use_module(svg(svg_dom)).
:- use_module(svg(svg_elements)).



:- begin_tests(svg_dom).

test(svg_document, []):-
  once(
    phrase(
      svg_document(
        Tree,
        word(svg),
        [
          svg_rectangle([svg_x(0.5,cm),svg_y(1.5,cm)]),
          svg_rectangle([svg_x(1.5,cm),svg_y(2.5,cm)]),
          svg_rectangle([svg_x(2.5,cm),svg_y(3.5,cm)]),
          svg_rectangle([svg_x(3.5,cm),svg_y(0.5,cm)])
        ]
      ),
      Codes
    )
  ),
  atom_codes(Atom, Codes),
  formatnl(Atom),
  tree_to_gv_file([method(dot),to_file_type(pdf)], Tree, File),
  formatnl(File).

:- end_tests(svg_dom).



:- begin_tests(svg_elements).

test(svg_rectangle, []):-
  once(
    phrase(
      svg_rectangle(Tree, word(svg), [svg_x(0.5,cm),svg_y(1.5,cm)]),
      Codes
    )
  ),
  atom_codes(Atom, Codes),
  formatnl(Atom),
  tree_to_gv_file([method(dot),to_file_type(pdf)], Tree, File),
  formatnl(File).

:- end_tests(svg_elements).

