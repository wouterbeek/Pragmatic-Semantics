:- module(
  user_input,
  [
    user_input/3, % +Message:atom
                  % :LegalAnswer:dcg
                  % -Answer:atom
    user_input_directory/2, % +RelativeFile:atom
                            % -AbsoluteFile:atom
    user_input_file/3, % +Message:atom
                       % +Directory:atom
                       % -Filepath:atom
    user_input_filepath/2, % +Message:atom
                           % -Answer:atom
    user_input_password/2, % +Message:atom
                           % -UnencryptedPassword:list(code)
    user_interaction/5 % +Options:list(nvpair)
                       % +Action:atom
                       % :Goal
                       % +Headers:list(atom)
                       % +Tuples:list(list)
  ]
).

/** <module> User input

Handles user input and sequences in which user input is needed continuously
(called "user interaction").

@author Wouter Beek
@version 2013/10-2013/12
*/

:- use_remote_module(ap(ap_stat)).
:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_meta)).
:- use_remote_module(dcg(dcg_multi)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_remote_module(os(dir_ext)).

:- meta_predicate(user_input(+,3,+)).
:- meta_predicate(user_interaction(+,+,:,+,+)).
:- meta_predicate(user_interaction(+,+,:,+,+,+,+)).
:- meta_predicate(user_interaction(+,+,+,:,+,+,+,+)).



%! legal_directory(-Dir:atom)// is semidet.
% Parses legal directory names,
% where both Unix and Windows formats are supported.
%
% ## Example
%
% Unix directory input:
% ~~~
% /home/wbeek/Dropbox/IOTW
% ~~~
%
% Windows directory input:
% ~~~
% C:\Users\Quirinus\Dropbox\IOTW
% ~~~

% Relative directory with respect to the home directory.
legal_directory(Dir) -->
  % Unix home.
  "~/",
  legal_filepath_segments(L2),
  {
    expand_file_name('~', [PrefixPath]),
    directory_subdirectories(PrefixPath, L1),
    append(L1, L2, L),
    subdirectories_to_directory(L, Dir)
  }.
% Absolute directory.
legal_directory(Dir) -->
  (
    % Unix root.
    "/"
  ;
    % Windows root.
    "C:\\"
  ),
  legal_filepath_segments(L),
  {subdirectories_to_directory(L, Dir)}.

legal_file(File) -->
  dcg_multi1(legal_file_char, 1-_, Codes),
  {atom_codes(File, Codes)}.

legal_file_char(C) --> ascii_alpha_numeric(C).
legal_file_char(C) --> dot(C).

%! legal_filepath(-Filepath:atom)// is semidet.

% Directory relative to the user's home directory.
legal_filepath(Path) -->
  legal_directory(Dir),
  legal_file(File),
  {directory_file_path(Dir, File, Path)}.

legal_filepath_segment(Segment) -->
  dcg_multi1(legal_filepath_char, 1-_, Codes),
  {atom_codes(Segment, Codes)}.

legal_filepath_segments([H|T]) -->
  legal_filepath_segment(H),
  (
    % Unix
    "/"
  ;
    % Windows
    "\\"
  ),
  legal_filepath_segments(T).
legal_filepath_segments([H]) -->
  legal_filepath_segment(H).
legal_filepath_segments([]) --> [].

legal_filepath_char(C) --> ascii_letter(C).
legal_filepath_char(C) --> decimal_digit(C).
legal_filepath_char(C) --> dot(C).
legal_filepath_char(C) --> minus_sign(C).
legal_filepath_char(C) --> plus_sign(C).
legal_filepath_char(C) --> underscore(C).

legal_password(Codes) -->
  dcg_multi1(ascii_graphic, 7-_, Codes).

%! legal_user_interaction(-LegalUserInput:char)// is semidet.

legal_user_interaction(Char) -->
  ( a_uppercase(Code)
  ; n_lowercase(Code)
  ; q_lowercase(Code)
  ; y_lowercase(Code)),
  {char_code(Char, Code)}.

%! user_input(+Message:atom, :LegalAnswer:dcg, -Answer:atom) is det.

user_input(Msg, Legal, Answer):-
  repeat,
  format(user_output, '~w\n', [Msg]),
  read_line_to_codes(user_input, Codes),
  (
    once(phrase(dcg_call(Legal, Answer), Codes))
  ->
    !
  ;
    fail
  ).

%! user_input_directory(+RelativeFile:atom, -AbsoluteFile:atom) is det.
% This assumes that the file base name and type are known,
% but the encloding directory is not.
%
% @arg RelativeFile
% @arg AbsoluteFile

user_input_directory(RelativeFile, AbsoluteFile):-
  repeat,
  format(atom(Msg), 'Enter the directory holding file ~w.', [RelativeFile]),
  user_input(Msg, legal_directory, Dir),
  (
    absolute_file_name(
      RelativeFile,
      AbsoluteFile,
      [access(read),file_errors(fail),relative_to(Dir)]
    )
  ->
    !
  ;
    fail
  ).

%! user_input_file(+Message:atom, +Directory:atom, -Filepath:atom) is det.

user_input_file(Msg, Dir, Path):-
  repeat,
  user_input(Msg, legal_file, File),
  (
    directory_file_path(Dir, File, Path)
  ->
    !
  ;
    fail
  ).

%! user_input_filepath(+Message:atom, -Filepath:atom)

user_input_filepath(Msg, Filepath):-
  user_input(Msg, legal_filepath, Filepath).

%! user_input_password(+Message:atom, -UnencryptedPassword:list(code)) is det.

user_input_password(Message1, UnencryptedPassword):-
  atomic_list_concat(
    [
      Message1,
      'The password must consist of 7 or more ASCII graphic characters.'
    ],
    '\n',
    Message2
  ),
  user_input(Message2, legal_password, UnencryptedPassword).

%! user_interaction(
%!   +Options:list(nvpair),
%!   +ActionDescription:atom,
%!   :Goal,
%!   +Headers:list(atom),
%!   +Tuples:list(term)
%! ) is det.
% The generic predicate for executing arbitray Prolog goals for arbitrary
% sequences of Prolog terms under user-interaction.
%
% One of the use cases is cleaning a database, where a list of =Tuples=
% has been identified for removal by =Goal=, but a user is required to
% assent to each removal action.
%
% Receiving input from the user does not work in threads!
%
% The following options are supported:
%   * =|answer(+Answer:oneof(['A',n,q,y]]))|=
%
% @arg Options A list of name-value pairs.
% @arg ActionDescription An atomic description of the action
%        that is performed by the goal.
% @arg Goal An arbitrary Prolog goal that takes the number of elements
%        in each tuple as the number of arguments.
% @arg Headers A list of atoms describing the entries in each tuple.
%        The number of headers and the number of elements in each
%        tuple are assumed to be the same.
% @arg Tuples A list of tuples. These are the element lists for which goal
%        is executed after user-confirmation.

user_interaction(O1, Act, G, Hs, Ts):-
  length(Ts, NumberOfTs),

  % STATS
  ap_stage_init(NumberOfTs),

  user_interaction(O1, Act, G, 1, NumberOfTs, Hs, Ts).

%! user_interaction(
%!   +Options:list(nvpair),
%!   +ActionDescription:atom,
%!   :Goal,
%!   +IndexOfTuple:positive_integer,
%!   +NumberOfTuples:positive_integer,
%!   +Headers:list(atom),
%!   +Tuples:list(term)
%! ) is det.
% The following options are supported:
%   * =|answer(+Answer:oneof(['A',n,q,y]]))|=

user_interaction(_O1, Act, _G, _I, _L, _Hs, []):-
  format(user_output, '\n-----\nDONE! <~w>\n-----\n', [Act]), !.
user_interaction(O1, Act, G, I, L, Hs, Ts):-
  option(answer(UserAtom), O1), !,
  user_interaction(UserAtom, O1, Act, G, I, L, Hs, Ts).
user_interaction(O1, Act, G, I, L, Hs, Ts):-
  % Construct the message.
  nth1(I, Ts, T),
  findall(
    HeaderedElement,
    (
      nth0(J, Hs, H),
      nth0(J, T, Element),
      format(atom(HeaderedElement), '~w: <~w>', [H,Element])
    ),
    HeaderedElements
  ),
  atomic_list_concat(HeaderedElements, '\n\t', TAtom),
  format(atom(Msg), '[~w/~w] ~w\n\t~w\n(y/n/q)\n?: ', [I,L,Act,TAtom]),

  % Ask for legal user input.
  user_input(Msg, legal_user_interaction, Answer),

  % Execute the goal based on the user input.
  user_interaction(Answer, O1, Act, G, I, L, Hs, Ts).

%! user_interaction(
%!   +Answer:oneof(['A',n,q,y]),
%!   +Options:list(nvpair),
%!   +ActionDescription:atom,
%!   :Goal,
%!   +IndexOfTuple:positive_integer,
%!   +NumberOfTuples:positive_integer,
%!   +Headers:list(atom),
%!   +Tuples:list(term)
%! ) is det.
% @tbd Reimplement percentage done in timed thread.

user_interaction('A', _O1, _Act, G, I1, L, _Hs, Ts):- !,
  forall(
    between(I1, L, J),
    (
      nth1(J, Ts, Juple),
      apply(G, Juple),

      % STATS
      ap_stage_tick
    )
  ).
user_interaction(n, O1, Act, G, I1, L, Hs, Ts):- !,
  I2 is I1 + 1,
  user_interaction(O1, Act, G, I2, L, Hs, Ts).
user_interaction(q, _O1, _Act, _G, _I, _L, _Hs, _Ts):- !.
user_interaction(y, O1, Act, G, I1, L, Hs, Ts):- !,
  nth1(I1, Ts, T),
  apply(G, T),
  I2 is I1 + 1,
  user_interaction(O1, Act, G, I2, L, Hs, Ts).

