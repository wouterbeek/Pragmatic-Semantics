:- meta_predicate(dcg_replace(//,//,?,?)).
dcg_replace(_, _, [], []):- !.
dcg_replace(FromDCG, ToDCG), ToDCG -->
  FromDCG, !,
  dcg_replace(FromDCG, ToDCG).
dcg_replace(FromDCG, ToDCG), [Code] -->
  [Code],
  dcg_replace(FromDCG, ToDCG).

a --> `a`.
b --> `b`.

test:-
  phrase(dcg_replace(a, b), ``, Codes),
  atom_codes(Atom, Codes),
  format(user_output, '~a\n', [Atom]).

