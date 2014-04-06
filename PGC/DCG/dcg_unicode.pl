:- module(
  dcg_unicode,
  [
    u_alpha_numeric//0,
    u_alpha_numeric//1,
    character_tie//0,
    character_tie//1,
    u_graphic//0,
    u_graphic//1,
    u_letter//0,
    u_letter//1,
    u_letter_lowercase//0,
    u_letter_lowercase//1,
    u_letter_uppercase//0,
    u_letter_uppercase//1,
    middle_dot//0,
    middle_dot//1,
    u_punctuation//0,
    u_punctuation//1,
    undertie//0,
    undertie//1,
    zero_width_joiner//0,
    zero_width_joiner//1,
    zero_width_non_joiner//0,
    zero_width_non_joiner//1
  ]
).

/** <module> DCG support for Unicode character set

DCG rules that encode characters from the UNICODE standard.

Characters that are within the ASCII range are defined
in module [[dcg_ascii]].

@author Wouter Beek
@version 2013/07, 2013/09
*/



%! u_alpha_numberic// is semidet.
%! u_alpha_numberic(?Code:code)// is nondet.
% Notice that there is a string asymmetry between the generative and
% the semidet case here.
%
% @see http://www.swi-prolog.org/pldoc/doc_for?object=char_type/2

u_alpha_numeric --> u_alpha_numeric(_).
u_alpha_numeric(C) -->
  [C],
  {code_type(C, alnum)}.

character_tie --> [8256].
character_tie(8256) --> [8256].

u_graphic --> u_graphic(_).
u_graphic(C) -->
  [C],
  {code_type(C, graph)}.

u_letter --> u_letter(_).
u_letter(C) -->
  [C],
  {code_type(C, alpha)}.

u_letter_lowercase --> u_letter_lowercase(_).
u_letter_lowercase(C) -->
  [C],
  {code_type(C, lower)}.

u_letter_uppercase --> u_letter_uppercase(_).
u_letter_uppercase(C) -->
  [C],
  {code_type(C, upper)}.

middle_dot --> [183].
middle_dot(183) --> [183].

u_punctuation --> u_punctuation(_).
u_punctuation(C) -->
  [C],
  {code_type(C, punct)}.

undertie --> [8255].
undertie(8255) --> [8255].

zero_width_joiner --> [8203].
zero_width_joiner(8203) --> [8203].

zero_width_non_joiner --> [8204].
zero_width_non_joiner(8204) --> [8204].

