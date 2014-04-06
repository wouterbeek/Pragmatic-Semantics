:- module(
  io_ext,
  [
    atom_to_file/2, % +Atom:atom
                    % +File:atom
    codes_to_file/2, % +Codes:list(code)
                     % +File:atom
    copy_stream_line/2, % +From:stream
                        % +To:stream
    file_to_atom/2, % +File:atom
                    % -Atom:atom
    file_to_codes/2, % +File:atom
                     % -Codes:list(code)
    read_terms/3, % +Stream:stream
                  % -Terms:list(term)
                  % +Options:list(atom)
    stream_to_atom/2, % +Stream:stream
                      % -Atom:atom
    stream_to_file/2, % +Stream:stream
                      % +File:atom
% PEEKING
    peek_atom/2, % +Stream:stream
                 % +Atom:atom
    peek_length/3 % +Stream:stream
                  % +Length:integer
                  % ?Codes:list(integer)
  ]
).

/** <module> IO_EXT

Predicates that extend the swipl builtin I/O predicates operating on streams.

@author Wouter Beek
@version 2013/01, 2013/06, 2013/08, 2014/01, 2014/03
*/

:- use_remote_module(generics(codes_ext)).
:- use_module(library(readutil)).



%! atom_to_file(+Atom:atom, -File:atom) is det.
% Stores the given atom in the given file.
%
% @arg Atom An atom.
% @arg File An atomic file name.

atom_to_file(Atom, File):-
  access_file(File, write),
  setup_call_cleanup(
    open(File, write, Stream, [encoding(utf8),type(test)]),
    format(Stream, '~w', [Atom]),
    close(Stream)
  ).

codes_to_file(Codes, File):-
  access_file(File, write),
  setup_call_cleanup(
    open(File, write, Stream, [encoding(utf8),type(test)]),
    put_codes(Stream, Codes),
    close(Stream)
  ).

%! copy_stream_line(+From:stream, +To:stream) is det.
% Copy the next line on the former stream to the latter stream.
% @see Uses put_code/2.

copy_stream_line(From, To):-
  read_line_to_codes(From, Codes),
  put_codes(To, Codes),
  flush_output(To).

%! file_to_atom(+File:file, -Atom:atom) is det.
% Turns the given file's contents into a string.
%
% @arg File The file whose contents are put in a string.
% @arg Atom The atom containing the contents of the given file.

file_to_atom(File, Atom):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    stream_to_atom(Stream, Atom),
    close(Stream)
  ).

file_to_codes(File, Codes):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(test)]),
    read_stream_to_codes(Stream, Codes),
    close(Stream)
  ).

%! read_terms(+Stream:stream, -Terms:list(term), +Options:list(atom)) is det.
% Returns the terms as they occur on the given stream.
%
% @arg Stream
% @arg Terms
% @arg Options

read_terms(Stream, Terms, Options):-
  read_term(Stream, Term, Options),
  read_terms0(Stream, Term, Terms, Options).

read_terms0(_Stream, end_of_file, [], _Options):- !.
read_terms0(Stream, Term, [Term | Terms], Options):-
  read_terms(Stream, Terms, Options).

%! stream_to_atom(+Stream:stream, -Content:atom) is det.
% Stores the contents of an atom stream to an atom.

stream_to_atom(Stream, Atom):-
  % First we convert to 'codes', and then to an atom.
  read_stream_to_codes(Stream, Codes),
  % An atom is enough like a string to be used in string concatenation etc.
  atom_codes(Atom, Codes).

%! stream_to_file(+Stream:stream, +File:atom) is det.
% Stores an atomic stream to the given file.

stream_to_file(Stream, File):-
  stream_to_atom(Stream, Atom),
  atom_to_file(Atom, File).



% PEEKING %

%! peek_atom(+Stream:stream, +Atom:atom) is semidet.
% Succeeds if the given atom can be peeked at in the given stream,
% i.e. without changing the stream pointer.

peek_atom(Stream, Atom):-
  atom_codes(Atom, Codes),
  length(Codes, Length),
  peek_length(Stream, Length, Codes).


%! peek_length(
%!   +Stream:stream,
%!   +Length:integer,
%!   -Codes:list(integer)
%! ) is semidet.
% Returns the next =Length= number of =Codes= without changing the stream
% pointer.

peek_length(Stream, Length, Codes):-
  Length >= 0,
  stream_property(Stream, position(OriginalPosition)),
  % If peek_length0/3 fails directly, then the stream position will not be
  % restored to the original position. Therefore, we store the exist code in
  % =Status= and call this after restoring the stream to its original
  % position.
  (
    peek_length0(Stream, Length, Codes)
  ->
    Status = true
  ;
    Status = fail
  ),
  % Move back to the original position in the stream.
  set_stream_position(Stream, OriginalPosition),
  call(Status).

peek_length0(_Stream, 0, []):- !.
peek_length0(Stream, Length, [Code | Codes]):-
  get_code(Stream, Code),
  NewLength is Length - 1,
  peek_length0(Stream, NewLength, Codes).

