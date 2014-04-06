:- module(
  record_jar,
  [
    'record-jar'//2 % ?Encoding:atom
                    % ?Records:list(list(nvpair))
  ]
).

/** <module> RECORD_JAR

Support for the =|record-jar|= format for storing multiple records with a
variable repertoire of fields in a text format.

## Syntax

~~~{.abnf}
record-jar   = [encodingSig] [separator] *record
record       = 1*field separator
field        = ( field-name field-sep field-body CRLF )
field-name   = 1*character
field-sep    = *SP ":" *SP
field-body   = *(continuation 1*character)
continuation = ["\"] [[*SP CRLF] 1*SP]
separator    = [blank-line] *("%%" [comment] CRLF)
comment      = SP *69(character)
character    = SP / ASCCHAR / UNICHAR / ESCAPE
encodingSig  = "%%encoding" field-sep
                *(ALPHA / DIGIT / "-" / "_") CRLF
blank-line   = WSP CRLF

; ASCII characters except %x26 (&) and %x5C (\)
ASCCHAR      = %x21-25 / %x27-5B / %x5D-7E
; Unicode characters
UNICHAR      = %x80-10FFFF
ESCAPE       = "\" ("\" / "&" / "r" / "n" / "t" )
            / "&#x" 2*6HEXDIG ";"
~~~

@author Wouter Beek
@see Originally described in *The Art of Unix Programming*.
@see Latest description was found at
     http://tools.ietf.org/html/draft-phillips-record-jar-02
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_multi)).
:- use_module(flp(rfc4234_basic)).
:- use_module(library(apply)).
:- use_module(library(plunit)).



%! 'ASCCHAR'(?Code:code)//
% ASCII characters except %x26 (&) and %x5C (\).
%
% ~~~{.abnf}
% ASCCHAR = %x21-25 / %x27-5B / %x5D-7E
% ~~~

'ASCCHAR'(C) -->
  [C],
  {(between(33, 37, C)
  ; between(39, 91, C)
  ; between(93, 126, C)
  )}.

%! 'blank-line'//
%
% ~~~{.abnf}
% blank-line = WSP CRLF
% ~~~

'blank-line' -->
  'WSP',
  'CRLF'.

%! character(?Code:code)//
%
% ~~~{.abnf}
% character = SP / ASCCHAR / UNICHAR / ESCAPE
% ~~~
%
% Note that ampersand// and backslash// are explicitly excluded.
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

character(C) -->
  'ASCCHAR'(C).
character(C) -->
  'WSP'(C).
character(C) -->
  'ESCAPE'(C).
character(C) -->
  'UNICHAR'(C).

%! comment(?Comment:atom)//
%
% ~~~{.abnf}
% comment = SP *69(character)
% ~~~
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

comment(Comment) -->
  'WSP',
  dcg_multi1(character, 0-69, Comment, [convert(codes_atom)]).

%! continuation//
%
% ~~~{.abnf}
% continuation = ["\"] [[*SP CRLF] 1*SP]
% ~~~
%
% ## Inconsistency
%
% I assume the horizontal tab is also allowed in comments, as is space.

continuation -->
  (backslash ; ""),
  (
    (dcg_multi('WSP'), 'CRLF' ; ""),
    dcg_multi('WSP', 1-_)
  ;
    ""
  ).

%! encodingSig(?Encoding:atom)//
%
% ~~~{.abnf}
% encodingSig  = "%%encoding" field-sep *(ALPHA / DIGIT / "-" / "_") CRLF
% ~~~

encodingSig(Encoding) -->
  "%%encoding",
  'field-sep',
  dcg_multi1(encodingSig_, _N, Encoding, [convert(codes_atom)]),
  'CRLF'.
encodingSig_(C) --> 'ALPHA'(C).
encodingSig_(C) --> 'DIGIT'(C, _).
encodingSig_(C) --> hyphen(C).
encodingSig_(C) --> underscore(C).

%! 'ESCAPE'(?Code:code)//
%
% ~~~{.abnf}
% ESCAPE = "\" ("\" / "&" / "r" / "n" / "t" ) / "&#x" 2*6HEXDIG ";"
% ~~~

'ESCAPE'(C) -->
  backslash,
  ( backslash(C)
  ; ampersat(C)
  ; r_lowercase(C)
  ; n_lowercase(C)
  ; t_lowercase(C)
  ).
'ESCAPE'(DecimalNumber) -->
  "&#x",
  dcg_multi1('HEXDIG', 2-6, DecimalNumber, [convert(digits_to_decimal)]).

%! field(?Field:nvpair)//
%
% ~~~{.abnf}
% field = ( field-name field-sep field-body CRLF )
% ~~~

field(Name=Body) -->
  'field-name'(Name),
  'field-sep',
  'field-body'(Body),
  'CRLF'.

%! 'field-body'(?Body:list(atom))//
% The field-body contains the data value. Logically, the field-body
% consists of a single line of text using any combination of characters
% from the Universal Character Set followed by a `CRLF` (newline).
%
% ## Escaping
%
% The carriage return, newline, and tab characters, when they occur in the
% data value stored in the field-body, are represented by their common
% backslash escapes (=\r=, =\n=, and =\t= respectively).
%
% ~~~{.abnf}
% field-body   = *(continuation 1*character)
% ~~~
%
% ## Folding
%
% To accommodate line limits (enforced by another standard or implementation),
% readability, and presentational purposes, the field-body portion of a field
% can be split into a multi-line representation; this is called *folding*.
%
% @tbd It is RECOMMENDED that folding not occur between characters inside a
%      Unicode grapheme cluster (since this will alter the display of
%      characters in the file and might result in unintentional alteration
%      of the file's semantics).
% @see Information on grapheme clusters, UAX29.

'field-body'(Body) -->
  dcg_multi1('field-body_', _Rep, Body, [convert(atomic_list_concat)]).
'field-body_'(Body) -->
  continuation,
  dcg_multi1(character, 1-_, Body, [convert(codes_atom)]).

%! 'field-name'(-Tree:compound, ?Name:atom)//
% The field-name is an identifer. Field-names consist of a sequence of
% Unicode characters. Whitespace characters and colon (=:=, =%x3A=) are
% not permitted in a field-name.
%
% ## Case
%
% Field-names are case sensitive.  Upper and lowercase letters are
% often used to visually break up the name, for example using
% CamelCase.  It is a common convention that field names use an initial
% capital letter, although this is not enforced.
%
% ~~~{.abnf}
% field-name = 1*character
% ~~~
%
% ## Inconsistency
%
% The ABNF seems to be wrong. Th working draft states the following:
% ~~~{.txt}
% Whitespace characters and colon (":", %x3A) are not permitted in a
% field-name.
% ~~~
% We therefore introduce the extra DCG rule 'field-name-character'//1.

'field-name'(Name) -->
  dcg_multi1('field-name-character', 1-_, Name, [convert(codes_atom)]).

'field-name-character'(C) -->
  character(C),
  % Explicitly exclude space// and colon//.
  {\+ memberchk(C, [32, 58])}.

%! 'field-sep'//
% The field separator is the colon character (=:=, =%x3A=).
% The separator MAY be surrounded on either side by any amount of
% horizontal whitespace (tab or space characters). The normal
% convention is one space on each side.
%
% ~~~{.abnf}
% field-sep = *SP ":" *SP
% ~~~
%
% ## Inconsistency
%
% The ABNF does not mention the (horizontal) tab, only the space character.
% We solve this by using 'WSP'// instead of 'SP'//.

'field-sep' -->
  dcg_multi('WSP'),
  ":",
  dcg_multi('WSP').

%! 'record-jar'(?Encoding:atom, ?Records:list(list(nvpair)))//
%
% ~~~{.abnf}
% record-jar = [encodingSig] [separator] *record
% ~~~

'record-jar'(Encoding, Records) -->
  (encodingSig(Encoding) ; ""),
  % The disjunction with the empty string is not needed here,
  % since the production of the separator can process
  % the empty string as well.
  separator(_Comments),
  dcg_multi1(record, Records).

%! record(?Fields:list(nvpair))//
% ~~~{.abnf}
% record = 1*field separator
% ~~~

record(Fields) -->
  dcg_multi1(field, 1-_, Fields),
  separator(_Comments).

%! separator(?Comments:list(atom))//
%
% ~~~{.abnf}
% separator = [blank-line] *("%%" [comment] CRLF)
% ~~~

separator(Comments) -->
  ('blank-line' ; ""),
  dcg_multi1(separator_, _N, Comments, [convert(exclude(var))]).
separator_(Comment) -->
  "%%",
  (comment(Comment) ; ""),
  'CRLF'.

%! 'UNICHAR'(?Code:code)//
%
% ~~~{.abnf}
% UNICHAR = %x80-10FFFF
% ~~~

'UNICHAR'(C) -->
  [C],
  {between(128, 1114111, C)}.



:- begin_tests(record_jar, [blocked('Takes too long to run each time.')]).

:- use_module(library(apply)).
:- use_module(library(pio)).

test(record_jar, []):-
  absolute_file_name(
    lang(rfc5646_iana_registry),
    File,
    [access(read), file_type(text)]
  ),
  setup_call_cleanup(
    open(File, read, Stream, [type(binary)]),
    once(phrase_from_stream('record-jar'(E, Rs), Stream)),
    close(Stream)
  ),
  maplist(formatnl, [E|Rs]).

:- end_tests(record_jar).

