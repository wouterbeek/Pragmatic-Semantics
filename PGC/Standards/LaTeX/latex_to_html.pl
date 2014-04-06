:- module(
  latex_to_html,
  [
    latex_to_html//1 % +File:atom
  ]
).

/** <module> LaTeX to HTML

Conversion from LaTeX to HTML in the form of the DCG-based HTML DSL
of SWI-Prolog.

@author Wouter Beek
@version 2014/03
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(latex(latex_db)).
:- use_module(latex(latex_tree)).
:- use_module(library(http/html_write)).



latex_to_html(File) -->
  {latex_to_tree(File, Tree)},
  l2h(Tree).


l2h([bold(Content)|T]) -->
  html([
    b(\l2h(Content)),
    \l2h(T)
  ]).
l2h([definition(_,_)|T]) -->
  l2h(T).
l2h([emphasis(Content)|T]) -->
  html([
    i(\l2h(Content)),
    \l2h(T)
  ]).
l2h([math(Content)|T]) -->
  html([
    tt(Content),
    \l2h(T)
  ]).
l2h([quote_block(Content)|T]) -->
  html([
    quote(\l2h(Content)),
    \l2h(T)
  ]).
l2h([quote_inline(Content)|T]) -->
  html([
    i(\l2h(Content)),
    \l2h(T)
  ]).
l2h([word(W1),word(W2)|T]) -->
  html([
    W1,
    space,
    \l2h([word(W2)|T])
  ]).

