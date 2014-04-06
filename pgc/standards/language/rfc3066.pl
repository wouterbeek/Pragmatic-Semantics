:- module(
  rfc3066,
  [
    rfc3066_language_tag//3 % -Tree:compound
                            % -Primary:atom
                            % -Secondary:list(atom)
  ]
).

/** <module> RFC 3066

Language tag parsing.

@author Wouter Beek
@version 2013/02, 2013/06-2013/07
*/

:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(lang('iso639-1')). % Used in meta-call.
:- use_remote_module(lang('iso639-2')). % Used in meta-call.



rfc3066_language_tag(
  rfc3066_language_tag(primary(Primary),secondary(Secondary)),
  Primary,
  Secondary
) -->
  rfc3066_language_tag(Primary),
  blank,
  rfc3066_language_tag(Secondary).

rfc3066_language_tag(Tag) -->
  word(Word),
  {atom_length(Word, Length)},
  dcg_switch(
    Length,
    % Length-2 codes must be ISO 639-1.
    % Length-3 codes must be ISO 639-2.
    [2-'iso639-1'(Tag), 3-'iso639-2'(Tag)]
  ).

