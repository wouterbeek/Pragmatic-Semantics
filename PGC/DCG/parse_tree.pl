:- module(
  parse_tree,
  [
    parse_tree/3 % +TreeName:atom
                 % +SubTrees:list
                 % -Tree:compound
  ]
).

/** <module> Parse trees

Predicate for manipulating parse trees.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- use_module(library(apply)).



%! parse_tree(+TreeName:atom, +SubTrees:list, -Tree:compound)// is det.
% Constructs a tree based on a list of direct subtrees and variables
% (excluded).
%
% The variables come from unused optional rules in the DCG body.
%
% @arg TreeName The atomic name of the grammar rule for which
%      the tree is constructed.
% @arg SubTrees A list of compound terms (direct subtrees)
%      and variables (excluded from the created tree).
% @arg Tree A compound term representing a parse tree.

parse_tree(P, SubT1, T):-
  include(nonvar, SubT1, SubT2),
  T =.. [P|SubT2].

