/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Reads a positive and negative example file and randomly duplicates an 
example from one into the other
saves the file with filename + noiseCatPercentage and copies other 
files with same name under new name

noiseCategory(+FileStem, +PosNeg, +Percentage).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

noiseCategory(FileStem, PosNeg, Percentage):-
    (
		PosNeg == pos,
		concat(FileStem, '.f',File),
		concat(FileStem, '.n', OtherFile),
		Extension = 'posCat.f',
		Extension2 = 'posCat.n',
		Extension3 = 'posCat.b',
	    open(File, read, Stream1),
		read_file(Stream1,Lines),
		length(Lines, PosList),
		close(Stream1),	
		open(OtherFile, read, Stream2),
		read_file(Stream2, OtherLines),
		length(OtherLines, NegList),
		close(Stream2),
		Counter is round((NegList / PosList) * (PosList * (Percentage/100)))
	;
		PosNeg == neg,
		concat(FileStem, '.n',File),
		concat(FileStem, '.f', OtherFile),
		Extension = 'negCat.n',
		Extension2 = 'negCat.f',
		Extension3 = 'negCat.b',
		open(File, read, Stream1),
		read_file(Stream1,Lines),
		length(Lines, NegList),
		close(Stream1),
		open(OtherFile, read, Stream2),
		read_file(Stream2, OtherLines),
		length(OtherLines, PosList),
		close(Stream2),
		Counter is round((PosList / NegList) * (NegList * (Percentage/100))),
		write(Counter)

	),
    
	!,
	duplicateLines(Counter, Lines, NewLines),
	concat(FileStem, Percentage, Temp),
	concat(Temp, Extension, NewFile1),
	concat(FileStem, '.b', File3),
	concat(FileStem, Percentage, Temp2),
	concat(Temp2, Extension2, NewFile2),
	concat(FileStem, Percentage, Temp3),
	concat(Temp3, Extension3, NewFile3),
	copy(File, NewFile1),
	copy(OtherFile, NewFile2),
	copy(File3, NewFile3),
	open(NewFile2, append, Stream3),
	maplist(format(Stream3, '\n~w.'), NewLines),
    flush_output(Stream3),
    close(Stream3).

	
read_file(Stream,[]):-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]):-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).

copy(File1, File2):-
	open(File1, read, Stream1),
	open(File2, write, Stream2),
	copy_stream_data(Stream1, Stream2),
	close(Stream1),
	close(Stream2).
	
duplicateLines(0, _, []):-!.
	
duplicateLines(Counter, Lines, [NewHead|NewLines]):-
	random(Rand),
	length(Lines, ListSize),
	Element is round(((ListSize-1) * Rand)),
	nth0(Element, Lines, NewHead, _),
	NewCounter is Counter - 1,
	duplicateLines(NewCounter, Lines, NewLines).