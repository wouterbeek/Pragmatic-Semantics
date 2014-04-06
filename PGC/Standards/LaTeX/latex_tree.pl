:- module(
  latex_tree,
  [
    latex_to_tree/2 % +File:atom
                    % -Tree:compound
  ]
).

/** <module> LaTeX to tree

Parses a LaTeX file and returns its syntax tree.

@author Wouter Beek
@version 2014/03
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_meta)).
:- use_module(library(pure_input)).

:- meta_predicate(latex_block(?,//,?,?)).



latex_to_tree(File, Tree):-
  phrase_from_file(latex_tree(Tree), File).

latex_tree([section(Level,Name)|T]) -->
  latex_section(Level, Name),
  (ascii_whites, latex_label(_) ; ``),
  ascii_whites,
  latex_tree(T).
latex_tree([H|T]) -->
  latex_text(H),
  ascii_whites,
  latex_tree(T).


latex_argument(Arg) -->
  bracketed(curly, latex_text(Arg)).

latex_block(Name, DCG) -->
  latex_block_begin(Name),
  (latex_label(_) ; ``),
  dcg_call(DCG),
  latex_block_end(Name).

latex_block_begin(Name) -->
  latex_command(begin),
  latex_argument(Name), !.

latex_block_end(Name) -->
  latex_command(end),
  latex_argument(Name), !.

latex_bold(Content) -->
  latex_command(textbf),
  latex_argument(Content), !.

latex_citation(Source) -->
  latex_command(cite),
  latex_argument(Source), !.

latex_command(Name) -->
  backslash,
  word(Name), !.

latex_definition(Name, Content) -->
  latex_block(Name, latex_definition_inner(Name,Content)).
latex_definition_inner(Name, Content) -->
  latex_option(Name),
  latex_text(Content).

latex_emphasis(Content) -->
  latex_command(emph),
  latex_argument(Content).

latex_label(Name) -->
  latex_command(label),
  latex_argument(Name).

latex_math(Math2) -->
  dcg_between(dollar_sign, codes(Math1)),
  {atom_codes(Math2, Math1)}.

latex_option(Name) -->
  bracketed(square, latex_text(Name)).

latex_quote(Content) -->
  latex_block(quote, Content).

latex_reference(Name) -->
  latex_command(ref),
  latex_argument(Name).

latex_section(Level,Name) -->
  latex_command(Command),
  {section_level(Command, Level)},
  latex_argument(Name).

latex_subsection(Name) -->
  latex_command(subsection),
  latex_argument(Name).

latex_text([quote_inline(Inner)|T]) -->
  grave_accent,
  grave_accent, !,
  latex_text(Inner),
  apostrophe,
  apostrophe,
  ascii_whites,
  latex_text(T).
latex_text(T) -->
  latex_citation(_), !,
  ascii_whites,
  latex_text(T).
latex_text([math(H)|T]) -->
  latex_math(H), !,
  ascii_whites,
  latex_text(T).
latex_text([quote_block(Content)|T]) -->
  latex_quote(Content), !,
  ascii_whites,
  latex_text(T).
latex_text([definition(Name,Content)|T]) -->
  latex_definition(Name, Content), !,
  ascii_whites,
  latex_text(T).
latex_text([emphasis(Content)|T]) -->
  latex_emphasis(Content), !,
  ascii_whites,
  latex_text(T).
latex_text([bold(Content)|T]) -->
  latex_bold(Content), !,
  ascii_whites,
  latex_text(T).
latex_text(T) -->
  latex_reference(_), !,
  ascii_whites,
  latex_text(T).
latex_text([word(Word)|T]) -->
  word(Word),
  {Word \== ''}, !,
  ascii_whites,
  latex_text(T).
latex_text([]) --> [].

section_level(section, 1).
section_level(subsection, 2).
section_level(subsubsection, 3).

