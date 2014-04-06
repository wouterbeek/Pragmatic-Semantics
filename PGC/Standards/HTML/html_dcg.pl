:- module(
  html_dcg,
  [
    html_dcg//1, % +Content:list(or([atom,compound,list(code)]))
    html_element//2, % +ElementName:atom
                     % +Attributes:list(nvpair)
    html_element//3, % +ElementName:atom
                     % +Attributes:list(nvpair)
                     % :Content:dcg
    html_entity//0,
    html_entity//1, % +EntityName:atom
    html_graph//0,
    html_graph//1, % ?Code:code
    html_string//0,
    html_style//1 % ?NVPairs:list(nvpair)
  ]
).

/** <module> HTML_DCG

DCG rules for HTML expressions.

@author Wouter Beek
@version 2013/09
*/

:- use_remote_module(dcg(dcg_ascii)).
:- use_remote_module(dcg(dcg_content)).
:- use_remote_module(dcg(dcg_generic)).
:- use_remote_module(dcg(dcg_unicode)).

:- meta_predicate(html_element(+,+,//,?,?)).



%! html_attribute//
% Used for *checking* GraphViz HTML-like labels.

html_attribute -->
  word(_),
  "=",
  dcg_between(double_quote, html_string).

%! html_attribute(+Name:atom, +Value:atom)//
% Used for *generating* GraphViz HTML-like labels.

html_attribute(N, V) -->
  atom(N),
  "=",
  dcg_between(double_quote, atom(V)).

html_attributes --> [].
html_attributes -->
  " ",
  html_attribute,
  html_attributes.

html_attributes([]) --> [].
html_attributes([N=V|T]) -->
  " ",
  html_attribute(N, V),
  html_attributes(T).

html_attributes_(Attrs) -->
  {var(Attrs)}, !,
  html_attributes.
html_attributes_(Attrs) -->
  html_attributes(Attrs).

%! html_dcg(+Content:list(or([atom,compound,list(code)])))//

% Done.
html_dcg([]) --> !, [].
% Tag with no content.
html_dcg([tag(Name,Attrs)|T]) --> !,
  html_element(Name, Attrs),
  html_dcg(T).
% Tab with content.
html_dcg([tag(Name,Attrs,Contents)|T]) --> !,
  html_element(Name, Attrs, html_dcg(Contents)),
  html_dcg(T).
% Atom.
html_dcg([H|T]) -->
  {atom(H)}, !,
  atom(H),
  html_dcg(T).
% Codes list.
html_dcg([H|T]) -->
  html_string(H), !,
  html_dcg(T).

html_entity -->
  "&",
  word(_),
  ";".

html_entity(EntityName) -->
  "&",
  atom(EntityName),
  ";".

html_element(ElementName, Attrs) -->
  "<",
  atom(ElementName),
  html_attributes_(Attrs),
  "/>".

html_element(ElementName, Attrs, Content) -->
  % Opening tab.
  "<",
  atom(ElementName),
  html_attributes_(Attrs),
  ">",

  % Content.
  phrase(Content),

  % Closing tab.
  "</",
  atom(ElementName),
  ">".

%! html_graph//
% HTML reserves the following ASCII characters:
%   * Ampersand
%   * Apostrophe
%   * Greater-than
%   * Less-than
%   * Quotation mark

html_graph -->
  white.
html_graph -->
  u_alpha_numeric.
html_graph -->
  html_punctuation.

html_graph(C) -->
  white(C).
html_graph(C) -->
  u_alpha_numeric(C).
html_graph(C) -->
  html_punctuation(C).

html_punctuation -->
  u_punctuation(C),
  {\+ member(C, [34,60,62,68])}.

% First come the translations for escaped punctuation characters.
html_punctuation(34) --> "&quot;". % Double quotes (").
html_punctuation(60) --> "&lt;".   % Smaller than (<).
html_punctuation(62) --> "&gt;".   % Greater than (>).
html_punctuation(68) --> "&amp;".  % Ampersand (&).
html_punctuation(C) -->
  u_punctuation(C),
  {\+ member(C, [34,60,62,68])}.

%! html_string//
% A _string_ is any collection of printable characters, including all spaces.

html_string -->
  html_graph,
  html_string.
html_string -->
  html_graph.

html_string([]) --> [].
html_string([H|T]) -->
  html_graph(H),
  html_string(T).

html_style([]) --> [].
html_style([NVPair|NVPairs]) -->
  {var(NVPair)}, !,
  html_style_word(Name1),
  colon,
  (space ; ""),
  html_style_word(Value1),
  semi_colon,
  {
    atom_codes(Name2, Name1),
    atom_codes(Value2, Value1),
    NVPair =.. [Name2, Value2]
  },
  html_style(NVPairs).
html_style([NVPair|NVPairs]) -->
  {nonvar(NVPair)}, !,
  {
    NVPair =.. [Name1, Value1],
    atom_codes(Name1, Name2),
    atom_codes(Value1, Value2)
  },
  html_style_word(Name2),
  colon,
  (space ; ""),
  html_style_word(Value2),
  semi_colon,
  html_style(NVPairs).

html_style_word([H|T]) -->
  (ascii_letter(H) ; underscore(H)),
  html_style_word(T).
html_style_word([]) --> [].

