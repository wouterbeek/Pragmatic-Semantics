:- module(
  html_dropdown_list,
  [
    html_dropdown_list//3 % +Name:atom
                          % :Legend
                          % +Options:list(pair(atom))
  ]
).

/** <module> HTML drop-down list

Generates HTML drop-down lists.

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_meta)).
:- use_module(library(http/html_write)).



%! html_dropdown_list(+Name:atom, :Legend, +Options:list(pair(atom)))// is det.

html_dropdown_list(Legend, Options) -->
  html(
    fieldset(class='pure-group', [
      legend(\dcg_call(Legend)),
      select(name=Name, \options(Options))
    ])
  ).

options([Value-Label|Options]) -->
  html(option(value=Value, Label)),
  options(Options).
options([]) --> [].
