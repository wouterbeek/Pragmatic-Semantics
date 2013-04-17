propplus(Q1:_/Qder1, Q2:_/Qder2):-
  Q1 \== Q2,
  Qder1 == Qder2.  

propmin(Q1:_/Qder1, Q2:_/Qder2):-
  Q1 \== Q2,
  (
    Qder1 == increasing,
    Qder2 == decreasing 
  ;
    Qder1 == decreasing,
    Qder2 == increasing
  ;
    Qder1 == steady,
    Qder2 == steady
  ).

influenceplus(Q1:Mag1/Dir1, Q2:_/Dir2):-
  Q1 \== Q2,
  relative_mag(Q1:Mag1/Dir1, Sign1),
  qder(Dir2, Sign2),
  Sign1 == Sign2.

influencemin(Q1:Mag1/Dir1, Q2:_/Dir2):-
  Q1 \== Q2,
  relative_mag(Q1:Mag1/Dir1, Sign1),
  qder(Dir2, Sign2),
  (
    Sign1 == 1,
    Sign2 == -1
  ;
    Sign1 == 0,
    Sign2 == 0
  ;
    Sign1 == -1,
    Sign2 == 1
  ).

mult_influence(Q1:Mag1/Dir1, Q2:Mag2/Dir2, Q3:_/Dir3, Rel):-
  Q1 \== Q2,
  Q2 \== Q3,
  Q3 \== Q1,
  relative_mag(Q1:Mag1/Dir1, Sign1),
  relative_mag(Q2:Mag2/Dir2, Sign2),
  qder(Dir3, Sign3),
  lookup_table(Sign1, Sign2, Sign3, Rel).

mult_proportionality(Q1:_/Dir1, Q2:_/Dir2, Q3:_/Dir3, Rel):-
  Q1 \== Q2,
  Q2 \== Q3,
  Q3 \== Q1,
  qder(Dir1, Sign1),
  qder(Dir2, Sign2),
  qder(Dir3, Sign3),
  lookup_table(Sign1, Sign2, Sign3, Rel).

lookup_table(1,1,1,pospos).
lookup_table(1,1,-1,negneg).
lookup_table(1,1,0,posneg).
lookup_table(1,1,0,negpos).
lookup_table(1,-1,0,pospos).
lookup_table(1,-1,1,posneg).
lookup_table(1,-1,-1,negpos).
lookup_table(1,-1,0,negneg).
lookup_table(-1,1,0,pospos).
lookup_table(-1,1,-1,posneg).
lookup_table(-1,1,1,negpos).
lookup_table(-1,1,0,negneg).
lookup_table(-1,-1,-1,pospos).
lookup_table(-1,-1,0,posneg).
lookup_table(-1,-1,0,negpos).
lookup_table(-1,-1,1,negneg).
lookup_table(0,1,1,pospos).
lookup_table(0,1,-1,posneg).
lookup_table(0,1,1,negpos).
lookup_table(0,1,-1,negneg).
lookup_table(0,-1,-1,pospos).
lookup_table(0,-1,1,posneg).
lookup_table(0,-1,-1,negpos).
lookup_table(0,-1,1,negneg).
lookup_table(1,0,1,pospos).
lookup_table(1,0,1,posneg).
lookup_table(1,0,-1,negpos).
lookup_table(1,0,-1,negneg).
lookup_table(-1,0,-1,pospos).
lookup_table(-1,0,-1,posneg).
lookup_table(-1,0,1,negpos).
lookup_table(-1,0,1,negneg).
lookup_table(0,0,0,pospos).
lookup_table(0,0,0,posneg).
lookup_table(0,0,0,negpos).
lookup_table(0,0,0,negneg).

relative_mag(Q1:Mag1/_, Sign):-
  quantity_space(Q1:mag/_, List),
  nth1(ZeroPos, List, zero),
  nth1(Number, List, Mag1),
  (
    ZeroPos == Number, Sign = 0
  ;
    ZeroPos > Number, Sign = -1
  ;
    ZeroPos < Number, Sign = 1
  ).

qcorrespondenceplus(Q1:Mag1/_,Q2:Mag2/_):-
  Q1 \== Q2,
  quantity_space(Q1:mag/_, [M1 | QSpace1]),
  quantity_space(Q2:mag/_, [M2 | QSpace2]),
  length([M1 | QSpace1],L),
  length([M2 | QSpace2],L),
  magnitude_value_type(Q1, M1, MType),
  magnitude_value_type(Q2, M2, MType),
  nth1(MagPos, [M1 | QSpace1], Mag1),
  nth1(MagPos, [M2 | QSpace2], Mag2).

qcorrespondencemin(Q1:Mag1/_,Q2:Mag2/_):-
  Q1 \== Q2,
  quantity_space(Q1:mag/_, [M1 | QSpace1]),
  quantity_space(Q2:mag/_, [M2 | QSpace2]),
  length([M1 | QSpace1],L),
  length([M2 | QSpace2],L),
  magnitude_value_type(Q1, M1, MType),
  magnitude_value_type(Q2, M2, MType),  
  nth1(MagPos1, [M1 | QSpace1], Mag1),
  nth1(MagPos2, [M2 | QSpace2], Mag2),
  Sum is MagPos1 + MagPos2,
  Length is L + 1,
  Sum == Length.

vcorrespondence(Q1:Mag1/_,Q2:Mag2/_, MagRel):-
  Q1 \== Q2,
  (
    magnitude_value_type(Q1, Mag1, MType),
    magnitude_value_type(Q2, Mag2, MType),
    magrel_table(Mag1, Mag2, MagRel)
  ;
    magrel_table(M1, M2, MagRel),
    M1 \== Mag1,
    M2 \== Mag2 
  ).
