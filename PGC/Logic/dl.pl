:- module(
  description_logic,
  [
    circular/1, % +Terminology:list
    subformulas/2 % +Formula
                  % -Subformulas:ordset
  ]
).

/** <module> DESCRIPTION_LOGIC

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(ordsets)).
:- use_module(library(plunit)).



/*begin(latex)
\begin{boxdefinition}[Cyclic terminology]
\label{def:cyclic}
$\mathcal{T}$ is cyclic iff there is an equality $X \equiv Y$ in $\mathcal{T}$
such that $X$ uses $X$ in $\mathcal{T}$.

$X$ uses $Y$ in $\mathcal{T}$ iff $X$ directly uses $Z$ in $\mathcal{T}$
and $Z$ uses $Y$ in $\mathcal{T}$.

$X$ directly uses $Y$ in $\mathcal{T}$ iff there is an equality $X \equiv Z$
in $\mathcal{T}$ such that $Y$ is a subformula of $Z$.
\end{boxdefinition}
end(latex)*/

circular(T):-
  member(equality(X, _Y), T),
  uses(X, X, T),
  % A terminology is circular if at least one of its symbolic names
  % uses itself.
  !.

directly_uses(X, Y, T):-
  member(equality(X, Z), T),
  subformulas(Z, ZS),
  member(Y, ZS),
  !.

%! subformulas(+Formula, -Subformulas:ordset) is det.

subformulas(C, [C]):-
  atomic(C), !.
subformulas(atomic_negation(C), S2):-
  !,
  subformulas(C, S1),
  ord_add_element(S1, atomic_negation(C), S2).
subformulas(concept_negation(C), S2):-
  !,
  subformulas(C, S1),
  ord_add_element(S1, concept_negation(C), S2).
subformulas(intersection(C,D), S4):-
  !,
  subformulas(C, S1),
  subformulas(D, S2),
  ord_union(S1, S2, S3),
  ord_add_element(S3, intersection(C,D), S4).
subformulas(value_restriction(R,C), S4):-
  !,
  subformulas(R, S1),
  subformulas(C, S2),
  ord_union(S1, S2, S3),
  ord_add_element(S3, value_restriction(R,C), S4).
subformulas(limited_existential_quantification(R), S2):-
  !,
  subformulas(R, S1),
  ord_add_element(S1, limited_existential_quantification(R), S2).
subformulas(existential_quantification(R,C), S4):-
  !,
  subformulas(R, S1),
  subformulas(C, S2),
  ord_union(S1, S2, S3),
  ord_add_element(S3, existential_quantification(R,C), S4).

uses(X, Y, T):-
  directly_uses(X, Y, T),
  !.
uses(X, Y, T):-
  directly_uses(X, Z, T),
  uses(Z, Y, T).



:- begin_tests(dl).

test(circular, []):-
  circular([equality(a, b), equality(b, a)]).
test(circular, [fail]):-
  circular([equality(a, b), equality(b, c)]).

:- end_tests(dl).
