:- module(
  cowspeak,
  [
    cowspeak/1, % +Content
    cowspeak/2, % +Options
                % +Content
    cowspeak_web/2, % +Content
                    % -Markup
    cowspeak_web/3 % +Options
                   % +Content
                   % -Markup
  ]
).

/** <module> Cowspeak

A funny cow for communicating with the user.

Based on `cowsay` by Tony Monroe,
 using the open source speech synthesizer `eSpeak`.

@author Wouter Beek
@see http://en.wikipedia.org/wiki/Cowsay pointers to cowsay resources.
@see http://espeak.sourceforge.net/ home of eSpeak.
@version 2012/09-2012/10, 2013/05-2013/09, 2014/01
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(dcg(dcg_wrap)).
:- use_module(generics(codes_ext)).
:- use_module(generics(option_ext)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(plunit)).
:- use_module(library(settings)).
:- use_module(os(tts_ext)).

% The automated finding of meta-predicates seems to be over-eager.
:- meta_predicate(dcg_speech_bubble_line(+,+,?,?)).

:- setting(
  default_max_width,
  integer,
  40,
  'The default width of the speech bubble in the number characters.'
).



%! cow_atom(+In:or([atom,pair(atom,list),term]), -Out:atom) is det.

cow_atom(Format-Arguments, Atom):- !,
  format(atom(Atom), Format, Arguments).
cow_atom(Atom, Atom):-
  atom(Atom), !.
cow_atom(Term, Atom):-
  term_to_atom(Term, Atom).


%! cowspeak(+Content:or([term,list(terms)])) is det.
%! cowspeak(+Options, +Content:or([term,list(terms)])) is det.
% Turns the given text into a cowified message, displaying the given
% text in the cow's speech bubble.
%
% The cow, as it reflects upon its own cow life:
% ~~~{.txt}
% /--------------------------------------\
% |     ^__^                             |
% |     (oo)\_______                     |
% |     (__)\       )\/\                 |
% |         ||----w |                    |
% |         ||     ||                    |
% \--------------------------------------/
%         \   ^__^
%          \  (oo)\_______
%             (__)\       )\/\
%                 ||----w |
%                 ||     ||
% ~~~
%
% `Content` can be any term or list of terms.
% Terms of the form =|Format-ListOfArguments|= are treated specially
%  and are passed on to format/3.
%
% The following options are supported:
%   * =|eyes(+Eyes:or([atom,list(code)])|=
%     Either a list of character codes or an atom, the first 2 characters
%      of which will replace the eyes of the cow.
%   * =|maximum_width(?MaximumWidth:integer)|=
%     The maximum number of characters the speech bubble is allowed to have.
%     If the maximum width is exceeded by any content line, then the
%      wrap option -- if set -- is used.
%   * =|mode(+Mode:oneof(['Borg',dead,greedy,paranoia,stoned,tired,wired,youth]))|=
%     The following process_modes are supported: `Borg`, `dead`, `greedy`,
%      `paranoia`, `stoned`, `tired`, `wired`, `youth`.
%   * =|output(+Output)|=
%     The same output alternatives that apply to with_output_to/2.
%     The default value is =|stream(user_output)|=.
%   * =|speech(+OnOrOff:boolean)|=
%   * =|wrap_mode(+WrapMode:oneof([line,none,word]))|=
%     Whether `line` wrapping or =word= wrapping (default)
%     is applied, or neither of those (=none=, e.g. for ASCII art).
%
% @arg Options A list of name-value pairs.
% @arg Contents Either a term or a list of terms.
%        Processes terms of the form =|Format-ListOfArguments|= specially.
%
% @tbd Split lines by words (in whitespace). Add this to module ATOM_EXT.
% @tbd When tabs are used in cowspeak/2 the width of the speech balloon
%      cannot be reliable ascertained right now.

cowspeak(Content):-
  cowspeak([], Content).
cowspeak(O1, Contents):-
  is_list(Contents), !,
  maplist(cow_atom, Contents, Atoms),
  % Cut off the choicepoints that are due to the various DCGs
  % that are used to draw the cow and its speech bubble and contents.
  once('_cowspeak'(O1, Atoms)).
% Since we work with lists, we create a singleton list for single terms.
cowspeak(O1, Content):-
  cowspeak(O1, [Content]).

% All content is atomic by now.
'_cowspeak'(O1, Atoms):-
  % The default wrap mode is wrapping words.
  add_default_option(O1, wrap_mode, word, O2),

  % Determine the maximum width of the speech bubble.
  setting(default_max_width, DefaultMaxWidth),
  option(maximum_width(MaximumWidth), O2, DefaultMaxWidth),

  % Some characters are needed to display the speech bubble itself.
  MaximumEffectiveWidth is MaximumWidth - 4,
  merge_options(
    [padding(true),separator(newline),wrap_margin(MaximumEffectiveWidth)],
    O2,
    O3
  ),
  findall(
    CodeLine3,
    (
      member(Atom, Atoms),
      
      % A single atom may contain multiple lines.
      atomic_list_concat(Lines1, '\n', Atom), % split
      
      % Now we are taling about individual lines proper.
      member(Line1, Lines1),
      
      % Some lines may exceed the maximum allowed width,
      %  so they are  split up further.
      % The way in which this is done depends on
      %  the type of wrapping that is used.
      atom_codes(Line1, CodeLine1),
      phrase(dcg_wrap(O3), CodeLine1, CodeLine2),
      
      % We need a list for each line in order to determine
      % the speech bubble width.
      phrase(dcg_separated_list(newline, CodeLines1), CodeLine2),
      member(CodeLine3, CodeLines1)
    ),
    CodeLines2
  ),

  % Establish the width of the speech bubble.
  maplist(length, CodeLines2, LineLengths),
  max_list(LineLengths, LineWidth),

  % Cow DCG.
  phrase(dcg_cow(O3, LineWidth, CodeLines2), CowCodes),

  % Write to the given stream.
  option(output(Output), O3, user_output),
  atom_codes(CowAtom, CowCodes),
  with_output_to(Output, write(CowAtom)),

  % It can talk!
  option(speech(Speech), O3, true),
  (
    Speech == true
  ->
    text_to_speech(Atoms)
  ;
    true
  ).


%! dcg_cow(
%!   +Options:list(nvpair),
%!   +LineWidth:nonneg,
%!   +CodeLines:list(list(code))
%! )// is det.

dcg_cow(O, LineWidth, CodeLines) -->
  newline,
  dcg_speech_bubble(LineWidth, CodeLines),
  dcg_cow(O),
  newline.

%! dcg_cow(+Options:list(nvpair))// is det.
% Emits the actual cow.

dcg_cow(O1) -->
  {
    process_modes(O1, O2),
    Indent = 8,
    AddToIndent = 4,
    AddedIndent is Indent + AddToIndent,
    CowLength = 4
  },

  % First line.
  dcg_multi(space, Indent), backslash, "   ^__^", newline,

  % Second line.
  dcg_multi(space, Indent), space, backslash,
  "  (", dcg_cow_eyes(O2), ")",
  backslash, "___", dcg_multi(underscore, CowLength), newline,

  % Third line.
  dcg_multi(space, AddedIndent),
  "(__)", backslash, "   ", dcg_multi(space, CowLength), ")",
  dcg_cow_tail, newline,

  % Fourth line.
  dcg_multi(space, AddedIndent),
  " ", dcg_cow_tongue(O2),
  " ||", dcg_multi(hyphen, CowLength), "w |", newline,

  % Fifth line.
  dcg_multi(space, AddedIndent),
  "    ", "||", dcg_multi(space, CowLength), " ||", newline.

dcg_cow_eyes(O) -->
  {
    option(eyes(Eyes1), O, "oo"),
    % Enusre that the eyes are codes
    % (i.e., apply atom2code conversion if needed).
    once(atomic_codes(Eyes1, Eyes2)),
    Eyes2 = [X,Y|_], !
  },
  [X,Y].
dcg_cow_eyes(_O) --> "oo".

dcg_cow_tail -->
  backslash, forward_slash, backslash.

dcg_cow_tongue(O) -->
  {
    option(tongue(Tongue1), O, "  "),
    % Enusre that the eyes are codes
    % (i.e., apply atom2code conversion if needed).
    once(atomic_codes(Tongue1, Tongue2)),
    Tongue2 = [X,Y|_], !
  },
  [X,Y].
dcg_cow_tongue(_O) --> "  ".

%! dcg_speech_bubble(+LineWidth:integer, +CodeLines:list(list(code)))//
% Draws a speech bubble with the given content,
% and whose content lines have the given length.

dcg_speech_bubble(LineWidth, CodeLines) -->
  dcg_speech_bubble_top(LineWidth), newline,
  dcg_speech_bubble_lines(LineWidth, CodeLines),
  dcg_speech_bubble_bottom(LineWidth), newline.

dcg_speech_bubble_bottom(LineWidth) -->
  `\\-`,
  dcg_multi(`-`, LineWidth),
  `-/`.

dcg_speech_bubble_line(LineWidth, CodeLine) -->
  `| `,
  CodeLine,
  {
    length(CodeLine, ContentLength),
    NumberOfSpaces is LineWidth - ContentLength
  },
  dcg_multi(` `, NumberOfSpaces),
  ` |`,
  newline.

dcg_speech_bubble_lines(_LineWidth, []) --> !, [].
dcg_speech_bubble_lines(LineWidth, [CodeLine|CodeLines]) -->
  dcg_speech_bubble_line(LineWidth, CodeLine),
  dcg_speech_bubble_lines(LineWidth, CodeLines).

dcg_speech_bubble_top(LineWidth) -->
  `/-`,
  dcg_multi(`-`, LineWidth),
  `-\\`.

%! cowspeak_web(+Content, -Markup:list) is det.
%! cowspeak_web(+Options, +Content, -Markup:list) is det.
% @see Like cowspeak/2, but returns the result in markup.

cowspeak_web(Content, Markup):-
  cowspeak_web([], Content, Markup).
cowspeak_web(
  O1,
  Content,
  [element(title, [], ['Cow says'])]/[element(pre, [], [Atom])]
):-
  select_option(output(_Output), O1, O2),
  merge_options([output(atom(Atom))], O2, O3),
  cowspeak(O3, Content).

mode('Borg', [eyes("==")]).
mode(dead, [eyes("XX"), tongue("U")]).
mode(greedy, [eyes("$$")]).
mode(paranoia, [eyes("@@")]).
mode(stoned, [eyes("**"), tongue("U")]).
mode(tired, [eyes("--")]).
mode(wired, [eyes("OO")]).
mode(youth, [eyes("..")]).

process_modes(O1, O3):-
  option(mode(Mode), O1),
  mode(Mode, O2), !,
  merge_options(O2, O1, O3).
process_modes(O1, O1).



:- begin_tests(cowspeak).

cow_sentence(test).
cow_sentence('Interpret the next argument as a character code and add it to the output. This argument must be a valid Unicode character code. Note that the actually emitted bytes are defined by the character encoding of the output stream and an exception may be raised if the output stream is not capable of representing the requested Unicode character. See section 2.18.1 for details.').
cow_sentence('%! cowspeak(+Options, +Content:or([term,list(terms)])) is det.').
cow_sentence('    ^__^
    (oo)\\_______
    (__)\\       )\\/\\
        ||----w |
        ||     ||').

test(cowspeak, [forall(cow_sentence(S)),true]):-
  cowspeak(S).

:- end_tests(cowspeak).

