:- module(
  dcg_dict,
  [
    conj//1, % ?Lang:atom
    disj//1, % ?Lang:atom
    language//1, % ?Lang:atom
    pre//1, % ?Lang:atom
    uncertainty//1 % ?Lang:atom
  ]
).

/** <module> DCG_DICT

Dictionary for DCGs.

@author Wouter Beek
@version 2013/05-2013/06
*/

:- use_module(dcg(dcg_ascii)).



conj(en) --> "and".
conj(nl) --> "en".
conj(_Lang) --> ",".

disj(nl) --> "of".

language(nl) --> "Latijn".

pre(en) --> "after".
pre(en) --> "before".
pre(en) --> "In".
pre(en) --> "in".
pre(nl) --> "In".
pre(nl) --> "in".
pre(nl) --> "na".
pre(nl) --> "voor".
pre(nl) --> "vóór".

% Three dots uncertainty representation.
uncertainty(_Lang) --> "...".
% Question mark uncertainty representation.
uncertainty(Lang) -->
  opening_bracket,
  uncertainty(Lang),
  closing_bracket.
uncertainty(_Lang) --> "?".
uncertainty(_Lang) --> "ca.".
