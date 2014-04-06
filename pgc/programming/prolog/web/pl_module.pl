:- module(
  pl_module,
  [
    pl_module//1, % +Module:atom
    pl_modules//0
  ]
).

/** <module> Prolog module

Web interface to Prolog modules.

@author Wouter Beek
@version 2014/03
*/

:- use_remote_module(generics(meta_ext)).
:- use_remote_module(generics(uri_query)).
:- use_remote_module(html(html_list)).
:- use_remote_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(prolog_xref)).
:- use_remote_module(pl_web(pl_predicate)).
:- use_remote_module(pl_web(html_pl_term)).



%! module_properties(+Module:atom, -Row:list) is det.
% Returns a list of module properties.
%
% The list is ensured to consist of the following members:
%    1. The class to which the module belongs.
%    2. The file from which the module was loaded.
%    3. The list of exported predicates.
%    4. The list of exported operators.

module_properties(
  Module,
  [
    class(Class),
    file(File),
    predicates(Module, Predicates),
    operators(Module, Operators)
  ]
):-
  module_property(Module, class(Class)),
  module_property(Module, file(File)),
  ignore(module_property(Module, exports(Predicates))),
  default([], Predicates),
  ignore(module_property(Module, exported_operators(Operators))),
  default([], Operators).


pl_module(Module) -->
  {module_property(Module, exports(Predicates))},
  pl_predicates(Module, Predicates).


pl_modules -->
  {
    % Set of currently loaded modules.
    aggregate_all(
      set(Module),
      current_module(Module),
      Modules
    ),
    % Properties of modules.
    findall(
      [Module|Properties],
      (
        member(Module, Modules),
        module_properties(Module,Properties)
      ),
      Rows
    )
  },
  html_table(
    [header_row(true),indexed(true)],
    html('Overview of modules.'),
    html_pl_term,
    [['Module','Class','File','Exported predicates','Exported operators']
        |Rows]
  ).

