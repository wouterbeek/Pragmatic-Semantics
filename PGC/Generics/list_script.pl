:- module(
  list_script,
  [
    file_script/4, % :Goal
                   % +Message:atom
                   % +TODO_File:atom
                   % DONE_File:atom
    list_script/4, % :Goal
                   % +Message:atom
                   % +TODO:list(term)
                   % -NOT_DONE_SOL:list(term)
    list_script/6 % :Goal
                  % +Message:atom
                  % +TODO:list(term)
                  % +DONE_INIT:list(term)
                  % -DONE:list(term)
                  % -NOT_DONE_SOL:list(term)
  ]
).

/** <module> List script

List scripting is the practice of running some arbitrary goal on items
that are read in from a list that may be stored in a file.

There are two lists:
  * =TODO.txt=
    Contains all items the goal has to be run on.
  * =DONE.txt=
    Contains only those items for which goal was run at some point
    in the past.

The main method reads both of these files, applies a given goal to
the members of (TODO minus DONE), and adds the processed items to DONE.

The list stored in TODO changes accordingly,
i.e. denoting the remaining items.

To process the same items again one should have copied the orginal TODO list.

@author Wouter Beek
@version 2013/06, 2014/01
*/

:- use_remote_module(generics(atom_ext)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(readutil)).

:- meta_predicate(file_script(1,+,+,+)).
:- meta_predicate(list_script(1,+,+,-)).
:- meta_predicate(list_script(1,+,+,+,-,-)).
:- meta_predicate(list_script(1,+,+,+,+,-,+,-)).



%! file_script(:Goal, +Message:atom, +TODO_File:atom, +DONE_File:atom) is det.
% Applies the given goal to items that occur in the TODO file,
% but that do not occur in the DONE file.
%
% @arg Any unary predicate letter.
% @arg TodoFile The TODO file must exist.
% @arg DoneFile The DONE file may exist.

file_script(Goal, Msg, TODO_File, DONE_File):-
  maplist(file_to_items, [TODO_File,DONE_File], [TODO,DONE_INIT]),
  list_script(Goal, Msg, TODO, DONE_INIT, DONE_SOL, NOT_DONE_SOL),
  maplist(items_to_file, [DONE_SOL,NOT_DONE_SOL], [DONE_File,TODO_File]).

%! file_to_items(+File:atom, -Items:list) is det.
% Reads a list of items from the given text file.
%
% An item is considered to be the atomic contents of a single line
% in the text file. (There are thus as many items as there are lines.)

file_to_items(File, Items):-
  setup_call_cleanup(
    open(File, read, Stream, [encoding(utf8),type(text)]),
    stream_to_items(Stream, Items),
    close(Stream)
  ).

item_to_stream(Stream, Item):-
  format(Stream, '~a\n', [Item]).

items_to_file(Items, File):-
  setup_call_cleanup(
    open(File, write, Stream, [encoding(utf8),type(text)]),
    maplist(item_to_stream(Stream), Items),
    close(Stream)
  ).

%! list_script(
%!   :Goal,
%!   +Message:atom,
%!   +TODO:list(term),
%!   -NOT_DONE_SOL:list(term)
%! ) is det.
%! list_script(
%!   :Goal,
%!   +Message:atom,
%!   +TODO:list(term),
%!   +DONE_INIT:list(term),
%!   -DONE_SOL:list(term),
%!   -NOT_DONE_SOL:list(term)
%! ) is det.
% Processes the items in `TODO` using the given goal
% and places the items either in the `DONE` or in the `NOT_DONE` list.
% The `DONE` list can be pre-instantiated.
%
% @arg Goal
% @arg Message
% @arg TODO
% @arg DONE_INIT
% @arg DONE_SOL
% @arg NOT_DONE_SOL

list_script(Goal, Msg, TODO, NOT_DONE_SOL):-
  list_script(Goal, Msg, TODO, [], _, NOT_DONE_SOL).

list_script(_, _, [], DONE_SOL, DONE_SOL, []):- !.
list_script(Goal, Msg, TODO, DONE_INIT, DONE_SOL2, NOT_DONE_SOL):-
  length(TODO, L),
  % We instantiate the initial index to `0`, but this will be incremented before
  % it is used, thus effectively counting from `1` onwards.
  list_script(Goal, Msg, 0-L, TODO, DONE_INIT, DONE_SOL1, [], NOT_DONE_SOL),
  length(DONE_SOL1, L1),
  progress_bar(L1, L, Bar),
  debug(high, '[EVAL] ~w: ~w', [Msg,Bar]),
  ord_union(DONE_INIT, DONE_SOL1, DONE_SOL2).

% Nothing `TODO`.
list_script(_, _Msg, L-L, [], DONE_SOL, DONE_SOL, NOT_DONE_SOL, NOT_DONE_SOL):- !.
% A `TODO` items that is already in `DONE`.
list_script(Goal, Msg, I1-L, [X|TODO], DONE, DONE_SOL, NOT_DONE, NOT_DONE_SOL):-
  memberchk(X, DONE), !,
  I2 is I1 + 1,
  debug(high, '[DONE] ~a ~:d/~:d', [Msg,I2,L]),
  list_script(Goal, Msg, I2-L, TODO, DONE, DONE_SOL, NOT_DONE, NOT_DONE_SOL).
% Could process a `TODO` item, pushed to `DONE`.
list_script(Goal1, Msg, I1-L, [X|TODO], DONE1, DONE_SOL, NOT_DONE, NOT_DONE_SOL):-
  strip_module(Goal1, Module, Goal2),
  Goal2 =.. [Pred|Args1],
  append(Args1, [X], Args2),
  Goal3 =.. [Pred|Args2],
  Module:call(Goal3), !,
  % Retrieve the current index, based on the previous index.
  I2 is I1 + 1,
  debug(high, '[TODO] ~a ~:d/~:d', [Msg,I2,L]),
  ord_add_element(DONE1, X, DONE2),
  list_script(Goal1, Msg, I2-L, TODO, DONE2, DONE_SOL, NOT_DONE, NOT_DONE_SOL).
% Could not process a `TODO` item, pushed to `NOT_DONE`.
list_script(Goal, Msg, I1-L, [X|TODO], DONE, DONE_SOL, NOT_DONE1, NOT_DONE_SOL):-
  I2 is I1 + 1,
  debug(high, '[NOT-DONE] ~a ~:d/~:d', [Msg,I2,L]),
  ord_add_element(NOT_DONE1, X, NOT_DONE2),
  list_script(Goal, Msg, I2-L, TODO, DONE, DONE_SOL, NOT_DONE2, NOT_DONE_SOL).

%! stream_to_items(+Stream:stream, -Items:list) is det.
% @see Helper predicate for file_to_items/2.

stream_to_items(Stream, []):-
  at_end_of_stream(Stream), !.
stream_to_items(Stream, [H|T]):-
  read_line_to_codes(Stream, Codes),
  atom_codes(H, Codes),
  stream_to_items(Stream, T).

