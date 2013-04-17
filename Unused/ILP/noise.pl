:-consult(['percentageOff.pl', 'noiseCategory.pl']).

noise(File):-
  Counter = 90,
  doPercentage(pos, File, Counter),
  doPercentage(neg, File, Counter),
  doNoiseCat(pos, File, Counter),
  doNoiseCat(neg, File, Counter).
  
doPercentage(_,_,0):-!.  
  
doPercentage(PosNeg, File, Counter):-
  percentageOff(File, PosNeg, Counter),
  NewCounter is Counter - 10,
  doPercentage(PosNeg, File, NewCounter).
  
doNoiseCat(_,_,0):-!.

doNoiseCat(PosNeg, File, Counter):-
  noiseCategory(File, PosNeg, Counter),
  NewCounter is Counter - 10,
  doNoiseCat(PosNeg, File, NewCounter).