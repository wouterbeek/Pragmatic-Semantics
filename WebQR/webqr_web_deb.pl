:- module
  webqr_web_deb,
  [
    bg_web/3, % +Mode:oneof([natlang,triple])
              % +TestNumber:between(1,3)
              % -SvgDom:list(compound)
    mod_web/2 % +TestNumber:between(1,3)
              % -SvgDom:list(compound)
  ]
).

/** <module> WebQR debug tools

Web-interface for WebQR-specific debug tools.

mod_web/2 draws the QR test model with the given identifier.

bg_web/3 draws the behavior graph of the test model with
  the given identifier.

@author Wouter Beek
@version 2013/12
*/

:- use_remote_module(generics(db_ext)).
:- use_remote_module(gv(gv_file)).
:- use_module(library(http/http_dispatch)).
:- use_remote_module(qsim(qsim_mod)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(webqr(webqr_uc)).

% /webqr/deb
:- multifile(http:location/3).
http:location(webqr, root(webqr), []).
:- http_handler(webqr(debug), webqr_debug, []).
user:web_module('WebQR debug', webqr_web_deb).



%! bg_web(
%!   +Mode:oneof([natlang,qr,triple]),
%!   +TestNumber:between(1,3),
%!   -SvgDom:list(compound)
%! ) is det.

bg_web(Mode, N, SvgDom):-
  format(atom(Graph), 'qsim~w', [N]),
  webqr_uc(N, Graph),
  export_bg(Mode, Graph, GIF),
  graph_to_svg_dom([method(dot)], GIF, SvgDom).


mod_web(N, SvgDom):-
  format(atom(G), 'qsim~w', [N]),
  webqr_uc(N, G),
  export_mod(G, GIF),
  graph_to_svg_dom([method(sfdp)], GIF, SvgDom).

