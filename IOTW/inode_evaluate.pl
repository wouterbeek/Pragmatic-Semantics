:- module(
  inodes_evaluate,
  [
    evaluate_inodes/2 % +Options:list(nvpair)
                      % +IdentityHierarchyHash:atom
  ]
).

/** <module> Identity nodes evaluation

Evaluates results from identity experiments.

@author Wouter Beek
@version 2013/12
*/

:- use_module(generics(pair_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(csv)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(iotw(inode)).
:- use_module(rdf(rdf_stat)).



evaluate_inodes(O1, GA_Hash):-
  DeltaPerc = 0.05,
  absolute_file_name(iotw(stats), File, [access(write)]),
  setup_call_cleanup(
    open(File, append, OutStream),
    evaluate_inodes(O1, 1.0, DeltaPerc, GA_Hash, OutStream),
    close(OutStream)
  ).

evaluate_inodes(_O1, Perc, DeltaPerc, _GA_Hash, _OutStream):-
  Perc < DeltaPerc, !.
evaluate_inodes(O1, Perc1, DeltaPerc, GA_Hash, OutStream):-
  evaluate_inodes(O1, Perc1, GA_Hash, OutStream),
  Perc2 is Perc1 - DeltaPerc,
  evaluate_inodes(O1, Perc2, DeltaPerc, GA_Hash, OutStream).

evaluate_inodes(O1, Perc, GA_Hash1, OutStream):-
  % Create the reduced identity hierarchy.
  once(ihier(GA_Hash1, G, ISets1, _P_Assoc1, _, _)),
  random_subset(ISets1, ISets2),
  % Can we find these back?
  ord_subtract(ISets1, ISets2, ISets3),
  assert_inodes(O1, G, ISets2, GA_Hash2),

  % Higher approximation recall.
  assoc_to_higher_pairs(GA_Hash1, H_Approx1),
  assoc_to_higher_pairs(GA_Hash2, H_Approx2),
  ord_intersection(H_Approx1, H_Approx2, H_Approx12),
  maplist(length, [H_Approx1,H_Approx2,H_Approx12], [H1,H2,H12]),
  divide(H12, H1, HRecall),

  % Lower approximation recall.
  assoc_to_lower_pairs(GA_Hash1, L_Approx1),
  assoc_to_lower_pairs(GA_Hash2, L_Approx2),
  ord_intersection(L_Approx1, L_Approx2, L_Approx12),
  maplist(length, [L_Approx1,L_Approx2,L_Approx12], [L1,L2,L12]),
  divide(L12, L1, LRecall),

  % Quality
  maplist(divide, [L1,L2], [H1,H2], [Q1,Q2]),

  % Can the extracted alignments be found?
  ordsets_to_pairs(ISets3, IPairs3),
  ord_intersection(IPairs3, H_Approx2, H_IPairs3),
  maplist(length, [IPairs3,H_IPairs3], [IPairs3_Length,H_IPairs3_Length]),
  divide(H_IPairs3_Length, IPairs3_Length, H_IPairs3_Perc),
  
  % Higher cover.
  count_subjects(_, _, G, NumberOfSubjectTerms),
  % No reflexive cases.
  NumberOfPairs is NumberOfSubjectTerms * (NumberOfSubjectTerms - 1),
  divide(H2, NumberOfPairs, HCover),
  
  % DEB
  RemPerc is 1.0 - Perc,
  debug(inodes_evaluate, '~6f of alignments removed.', [RemPerc]),
  debug(inodes_evaluate, '~6f of removed alignments are in higher.', [H_IPairs3_Perc]),
  debug(inodes_evaluate, '~6f of all pairs are in higher.', [HCover]),
  H_IPairs3_Perc_Corrected is H_IPairs3_Perc - HCover,
  debug(inodes_evaluate, '~6f of all pairs are in higher (corrected).', [H_IPairs3_Perc_Corrected]),
  debug(inodes_evaluate, 'Recall::\tLow:~6f\tHigh:~6f', [LRecall,HRecall]),
  debug(inodes_evaluate, 'Quality::\t~6f->~6f', [Q1,Q2]),
  
  csv_write_stream(
    OutStream,
    [row(Perc,LRecall,HRecall,Q2,HCover,H_IPairs3_Perc,H_IPairs3_Perc_Corrected)],
    []
  ),
  flush_output(OutStream).

assoc_to_higher_pairs(GA_Hash, Pairs):-
  assoc_to_pairs(GA_Hash, higher, Pairs).

assoc_to_lower_pairs(GA_Hash, Pairs):-
  assoc_to_pairs(GA_Hash, lower, Pairs).

assoc_to_pairs(GA_Hash, Approx1, Pairs2):-
  findall(
    Pairs1,
    (
      approx(Approx1, Approx2),
      inode(_, _, GA_Hash, _, Approx2, _, _, _, Pairs1)
    ),
    Pairss
  ),
  ord_union(Pairss, Pairs2).

approx(X, X).
approx(higher, lower).

divide(X, Y, 1.0):-
  X =:= Y, !.
divide(X, _, 0.0):-
  X =:= 0.0, !.
divide(_, Y, 1.0):-
  Y =:= 0.0, !.
divide(X, Y, Z):-
  Z is X / Y.
