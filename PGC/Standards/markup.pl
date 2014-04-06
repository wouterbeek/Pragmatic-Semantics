:- module(
  markup,
  [
    format_number/3, % +Unit:unit
                     % +Number:float
                     % -Atom:atom
    integer_sequence/2, % +Sequence:list(integer)
                        % -Markup:compound
    sonnet/2 % +Sentences:list(atom)
             % -Markup:compound
  ]
).

/** <module> Markup

@author Wouter Beek
@version 2012/10, 2013/07
*/

:- use_module(standards(css), [attribute_value/3 as css_attribute_value]).



%! format_number(+Unit:unit, +Number:float, -FormattedNumber:atom) is det.
% Formats a number according to a certain unit scale.
%
% @arg Unit An atomic unit descriptor.
% @arg Number Any number (e.g., integer, float).
% @arg FormattedNumber The atomic result of formatting the number.
%
% @tbd Make sure the values for unit are registered with
%      specific markup languages.

format_number(Unit, Number, FormattedNumber):-
  atom_number(Atom, Number),
  atomic_concat(Atom, Unit, FormattedNumber).

%! integer_sequence(+Sequence:list(integer), -Markup:compound) is det.

integer_sequence([], []):- !.
integer_sequence([H | T], [element(span, [style=Style], [H1]) | Markup]):-
  FontSize is 100 + 10 * H,
  atom_number(H1, H),
  format_number('%', FontSize, FontSize_pct),
  css_attribute_value('font-size', FontSize_pct, Style),
  integer_sequence(T, Markup).

%! sonnet(+Sonnet:list(atom), -Markup:compound) is det.

sonnet(Sonnet, element(figure, [], Ts)):-
  sonnet0(Sonnet, Ts).

sonnet0(
  [Sentence1, Sentence2],
  [element(p, [], [Sentence1, element(br, [], []), Sentence2])]
):- !.
sonnet0(
  [Sentence1, Sentence2, Sentence3, Sentence4 | Sentences],
  [
    element(
      p,
      [],
      [
        Sentence1,
        element(br, [], []),
        Sentence2,
        element(br, [], []),
        Sentence3,
        element(br, [], []),
        Sentence4
      ]
    )
  |
    Markup
  ]
):-
  sonnet0(Sentences, Markup).

