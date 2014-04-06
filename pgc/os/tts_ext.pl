:- module(
  tts_ext,
  [
    text_to_speech/1 % +Contents
  ]
).

/** <module> TTS

Text-to-speech predicates.

@author Wouter Beek
@version 2013/06-2013/07
*/

:- use_remote_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(process)).
:- use_remote_module(os(os_ext)).
:- use_remote_module(os(run_ext)).

:- db_add_novel(user:module_uses_program(tts_ext, espeak)).



%! text_to_speech(+Contents) is det.
% Creates a process for a program that can turn text into speech.
%
% This is currently implemented for Unix systems with espeak in the PATH.
%
% @tbd Add support for Windows.
% @tbd Test support on Mac OS-X.

text_to_speech(Format-Arguments):- !,
  format(atom(Atom), Format, Arguments),
  text_to_speech(Atom).
text_to_speech(Terms):-
  is_list(Terms), !,
  maplist(term_to_atom, Terms, Atoms),
  atomic_list_concat(Atoms, '\n', Atom),
  text_to_speech(Atom).
text_to_speech(Atom):-
  atom(Atom), !,
  os_dependent_call(text_to_speech(Atom)).

:- if(is_unix).
text_to_speech_unix(Atom):-
  process_create(path(espeak), [Atom], [detached(true)]).
:- endif.

:- if(is_windows).
text_to_speech_windows(_Atom).
:- endif.

