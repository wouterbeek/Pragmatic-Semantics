:- module(
  html_pl_term,
  [
    html_pl_term//1 % +PlTerm
  ]
).

/** <module> HTML Prolog term

@author Wouter Beek
@version 2014/01-2014/03
*/

:- use_remote_module(generics(uri_query)).
:- use_remote_module(html(html)).
:- use_remote_module(html(html_list)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_remote_module(pl_web(html_pl_error)).
:- use_remote_module(pl_web(html_pl_generic)).



%! html_pl_term(@PlTerm)// is det.
% @tbd What about blobs?

% Error term.
html_pl_term(error(Formal,Context)) --> !,
  {Formal =.. [ErrorKind|_]},
  html(
    div(class=error, [
      div(class=error_kind,
        ErrorKind
      ),
      div(class=error_formal,
        \html_error_formal(Formal)
      ),
      \html_error_context(Context)
    ])
  ).
% Prolog module.
html_pl_term(Module) -->
  {current_module(Module)}, !,
  html_module(Module).
% Class compound term.
html_pl_term(class(Class)) --> !,
  html(span(class=class, Class)).
% File compound term.
html_pl_term(file(File)) --> !,
  html_file(File).
% Prolog predicate terms.
html_pl_term(predicates(Module, Predicates)) --> !,
  html_list([ordered(false)], html_predicate(Module), Predicates).
% Prolog operators.
html_pl_term(operators(Module, Operators)) --> !,
  html_list([ordered(false)], html_operator(Module), Operators).
% Integer.
html_pl_term(Integer) -->
  {integer(Integer)}, !,
  {format(atom(FormattedInteger), '~:d', [Integer])},
  html(span(class=integer, FormattedInteger)).
% Floating point value.
html_pl_term(Float) -->
  {float(Float)}, !,
  {format(atom(FormattedFloat), '~G', [Float])},
  html(span(class=float, FormattedFloat)).
% String.
html_pl_term(String) -->
  {string(String)}, !,
  html(span(cass=string, String)).
% Atom.
html_pl_term(Atom) -->
  {atom(Atom)}, !,
  html(span(class=atom, Atom)).
% HTML link.
html_pl_term(URL-Label) --> !,
  html_link(URL-Label).
% Compound terms are converted to an atom first.
html_pl_term(PlTerm) -->
  {with_output_to(atom(Atom), write_canonical(PlTerm))},
  html(span(class=compound, Atom)).

