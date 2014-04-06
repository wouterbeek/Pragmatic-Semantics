:- module(
  html_list,
  [
    html_handler_list//2, % +Options:list(nvpair)
                          % +ParentPath:atom
    html_list//2, % +Options:list(nvpair)
                  % +Elements:list(ground)
    html_list//3 % +Options:list(nvpair)
                 % :Cell
                 % +Elements:list(ground)
  ]
).

/** <module> HTML list

Support for generating HTML lists.

@author Wouter Beek
@version 2013/10-2013/11, 2014/01, 2014/03
*/

:- use_module(dcg(dcg_meta)).
:- use_module(html(html)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(pl_web(html_pl_term)).
:- use_module(server(web_modules)).

:- meta_predicate(html_list(+,3,+,?,?)).
:- meta_predicate(html_list_items(//,+,?,?)).



%! html_handler_list(+Options:list(nvpair), +ParentPath:atom)// is det.
% Generates an HTML list for all URL paths that have an HTTP handler
% and that reside under the given parent path.

html_handler_list(O1, ParentPath) -->
  {
    findall(
      Link3-Label,
      (
        http:location(Label, ParentSpec1, _),
        ParentSpec1 =.. [ParentPath,_],
        ParentSpec2 =.. [ParentPath,'.'],
        http_absolute_location(ParentSpec2, Link1, []),
        atomic_list_concat([Link1,Label], Link2),
        file_name_extension(Link2, html, Link3)
      ),
      Pairs
    )
  },
  html_list(O1, html_link, Pairs).


%! html_list(+ListOptions:list(nvpair), +Elements:list(ground))// is det.
%! html_list(+ListOptions:list(nvpair), :Cell, +Elements:list(ground))// is det.
% Generates an HTML list containing the given list items.
%
% The following list options are supported:
%   * =|ordered(Ordered:boolean)|=
%     Whether an order =ol= or unordered =ul= HTML list is used.

html_list(O1, Elements) -->
  html_list(O1, html_pl_term, Elements).

html_list(O1, Cell, Elements) -->
  {select_option(ordered(Ordered), O1, O2, false)},
  (
    {Ordered == false}
  ->
    html(ul(O2, \html_list_items(Cell, Elements)))
  ;
    {Ordered == true}
  ->
    html(ol(O2, \html_list_items(Cell, Elements)))
  ).


html_list_items(_, []) --> !, [].
% First try to call the `Cell` DCG on the `H` argument.
% `H` may be a pair of the form `X-Y`.
html_list_items(Cell, [H|T]) -->
  html(li(\dcg_call(Cell, H))), !,
  html_list_items(Cell, T).
% Since generating the cell for `H` failed, we interpret
% the first element in the pair as an options list (i.e. HTML attributes)
% to the list item tag.
html_list_items(Cell, [O1-H|T]) -->
  html(li(O1, \dcg_call(Cell, H))),
  html_list_items(Cell, T).

