/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Reads a file and randomly removes a given percentage of the lines
saves the file with filename + percentage off and copies other 
files with same name under new name

percentageOff(+FileStem, +PosNeg, +Percentage).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

percentageOff(FileStem, PosNeg, Percentage):-
    (
		PosNeg == pos,
		concat(FileStem, '.f',File),
		OtherExamples = '.n',
		Extension = 'pos.f',
		Extension2 = 'pos.n',
		Extension3 = 'pos.b'
	;
		PosNeg == neg,
		concat(FileStem, '.n',File),
		OtherExamples = '.f',
		Extension = 'neg.n',
		Extension2 = 'neg.f',
		Extension3 = 'neg.b'
	),
    open(File, read, Stream1),
    read_file(Stream1,Lines),
	close(Stream1),
	!,
    length(Lines,ListSize),
	Counter is round(ListSize * (Percentage/100)),
	removePercentage(Counter, Lines, NewLines),
	concat(FileStem, Percentage, Temp),
	concat(Temp, Extension, NewFile),
	open(NewFile, write, Stream2),
    maplist(format(Stream2, '~w.\n'), NewLines),
    flush_output(Stream2),
    close(Stream2),
	concat(FileStem, OtherExamples, File2),
	concat(FileStem, '.b', File3),
	concat(FileStem, Percentage, Temp2),
	concat(Temp2, Extension2, NewFile2),
	concat(FileStem, Percentage, Temp3),
	concat(Temp3, Extension3, NewFile3),
	copy(File2, NewFile2),
	copy(File3, NewFile3).
	
read_file(Stream,[]):-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]):-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

removePercentage(0, Lines, Lines):-!.
	
removePercentage(Counter, Lines, Result):-
	random(Rand),
	length(Lines, ListSize),
	Element is round(((ListSize-1) * Rand)),
	nth0(Element, Lines, _, NewLines),
	NewCounter is Counter - 1,
	removePercentage(NewCounter, NewLines, Result).
	
copy(File1, File2):-
	open(File1, read, Stream1),
	open(File2, write, Stream2),
	copy_stream_data(Stream1, Stream2),
	close(Stream1),
	close(Stream2).