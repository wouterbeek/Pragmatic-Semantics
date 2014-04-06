:- module(
  tms_test,
  [
    doyle_test0/0,
    doyle_test1/0,
    doyle_test2/0
  ]
).

/** <module> TMS_TEST

Tests for the TMS module.

@author Wouter Beek
@version 2013/05, 2013/09-2013/10
*/

:- use_module(doyle(doyle)).
:- use_module(gv(gv_file)).
:- use_module(library(http/http_path)).
:- use_module(os(run_ext)).
:- use_module(tms(tms)).
:- use_module(tms(tms_export)).
:- use_module(tms(tms_web)).



doyle_test0:-
  TMS = doyle_test0,
  register_tms(doyle, TMS),
  doyle_reset(TMS),
  doyle_init(TMS),
  doyle_add_node(TMS, '1', N1),
  doyle_add_justification(TMS, [], [], 'J1', N1, _J1),
  http_absolute_uri(root(.), BaseURL),
  tms_export_graph([base_url(BaseURL)], TMS, GIF),
  graph_to_gv_file([method(dot),to_file_type(pdf)], GIF, PDF_File),
  open_pdf(PDF_File).

doyle_test1:-
  TMS = doyle_test1,
  register_tms(doyle, TMS),
  doyle_reset(TMS),
  doyle_init(TMS),
  doyle_add_node(TMS, 'A', A),
  doyle_add_node(TMS, 'B', B),
  doyle_add_node(TMS, 'C', C),
  doyle_add_node(TMS, 'D', D),
  doyle_add_node(TMS, 'E', E),
  doyle_add_node(TMS, 'F', F),
  doyle_add_justification(TMS, [C],    [],  'J1', A, _),
  doyle_add_justification(TMS, [],     [A], 'J2', B, _),
  doyle_add_justification(TMS, [A],    [],  'J3', C, _),
  doyle_add_justification(TMS, [B],    [],  'J4', D, _),
  doyle_add_justification(TMS, [C],    [],  'J5', D, _),
  doyle_add_justification(TMS, [],     [],  'J6', E, _),
  doyle_add_justification(TMS, [C, E], [],  'J7', F, _),
  http_absolute_uri(root(.), BaseURL),
  tms_export_graph([base_url(BaseURL)], TMS, GIF),
  graph_to_gv_file([method(dot),to_file_type(pdf)], GIF, PDF_File),
  open_pdf(PDF_File).

doyle_test2:-
  TMS = doyle_test2,
  register_tms(doyle, TMS),
  doyle_reset(TMS),
  doyle_init(TMS),
  doyle_add_node(TMS, '1', N1),
  doyle_add_node(TMS, '2', N2),
  doyle_add_node(TMS, '3', N3),
  doyle_add_node(TMS, '4', N4),
  doyle_add_node(TMS, '5', N5),
  doyle_add_node(TMS, '6', N6),
  doyle_add_justification(TMS, [N3],    [],   'J1', N1, _J1 ),
  doyle_add_justification(TMS, [],      [N1], 'J2', N2, _J2 ),
  doyle_add_justification(TMS, [N1],    [],   'J3', N3, _J3 ),
  doyle_add_justification(TMS, [N2],    [],   'J4', N4, _J4a),
  doyle_add_justification(TMS, [N3],    [],   'J5', N4, _J4b),
  doyle_add_justification(TMS, [],      [],   'J6', N5, _J5 ),
  doyle_add_justification(TMS, [N3,N5], [],   'J7', N6, _J6 ),
  http_absolute_uri(root(.), BaseURL),
  tms_export_graph([base_url(BaseURL)], TMS, GIF),
  graph_to_gv_file([method(dot),to_file_type(pdf)], GIF, PDF_File),
  open_pdf(PDF_File).

