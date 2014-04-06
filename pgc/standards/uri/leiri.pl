:- module(
  leiri,
  []
).

/** <module> LEIRI

Legacy extended IRIs for XML resource identification

LEIRIs have to be used with care; they require further processing before being
fully interchangeable as IRIs.

Examples:
  * XML system identifiers
  * XML Schema anyURIs

[?] For consistency with RFC3987 for IRIs, generic LEIRI software *|should not|*
check LEIRIs for conformance to this syntax. [?]

~~~{.bnf}
LEIRI           ::= scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
ihier-part      ::= "//" iauthority ipath-abempty
                    / ipath-absolute
                    / ipath-rootless
                    / ipath-empty
LEIRI-reference ::= LEIRI / irelative-ref
absolute-LEIRI  ::= scheme ":" ihier-part [ "?" iquery ]
irelative-ref   ::= irelative-part [ "?" iquery ] [ "#" ifragment ]
irelative-part  ::= "//" iauthority ipath-abempty
                    / ipath-absolute
                    / ipath-noscheme
                    / ipath-empty
iauthority      ::= [ iuserinfo "@" ] ihost [ ":" port ]
iuserinfo       ::= *( iunreserved / pct-encoded / sub-delims / ":" )
ihost           ::= IP-literal / IPv4address / ireg-name
ireg-name       ::= *( iunreserved / pct-encoded / sub-delims )
ipath           ::= ipath-abempty ; begins with "/" or is empty
                    / ipath-absolute ; begins with "/" but not "//"
                    / ipath-noscheme ; begins with a non-colon segment
                    / ipath-rootless ; begins with a segment
                    / ipath-empty ; zero characters
ipath-abempty   ::= *( "/" isegment )
ipath-absolute  ::= "/" [ isegment-nz *( "/" isegment ) ]
ipath-noscheme  ::= isegment-nz-nc *( "/" isegment )
ipath-rootless  ::= isegment-nz *( "/" isegment )
ipath-empty     ::= 0<ipchar>
isegment        ::= *ipchar
isegment-nz     ::= 1*ipchar
isegment-nz-nc  ::= 1*( iunreserved / pct-encoded / sub-delims / "@" )
                    ; non-zero-length segment without any colon ":"
ipchar          ::= iunreserved / pct-encoded / sub-delims / ":"
/ "@"
iquery          ::= *( ipchar / iprivate / "/" / "?" )
ifragment       ::= *( ipchar / "/" / "?" )
iunreserved     ::= ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
iprivate        ::= %xE000-F8FF / %xE0000-E0FFF / %xF0000-FFFFD
                    / %x100000-10FFFD
~~~

--

@author Wouter Beek
@see http://www.w3.org/TR/2008/NOTE-leiri-20081103/
@version 2013/08, 2014/03
*/

:- use_remote_module(dcg(dcg_multi)).
:- use_remote_module(flp(rfc4234_abnf)).
:- use_remote_module(http(rfc2616_basic)).



'LEIRI' -->
  scheme,
  `:`,
  'ihier-part',
  (`?`, iquery ; ``),
  (`#`, ifragment; ``).

'ihier-part' -->
  `//`,
  iauthority,
  ( 'ipath-abempty'
  ; 'ipath-absolute'
  ; 'ipath-rootless'
  ; 'ipath-empty').

'LEIRI-reference' -->
  'LEIRI'.
'LEIRI-reference' -->
  'irelative-ref'.

'absolute-LEIRI' -->
  scheme,
  `:`,
  'ihier-part',
  (`?`, iquery ; ``).

'irelative-ref' -->
  'irelative-part',
  (`?`, iquery ; ``),
  (`#`, ifragment ; ``).

'irelative-part' -->
  `//`,
  iauthority,
  ( 'ipath-abempty'
  ; 'ipath-absolute'
  ; 'ipath-noscheme'
  ; 'ipath-empty').

iauthority -->
  (iuserinfo, `@` ; ``),
  ihost,
  (`:`, port ; ``).

iuserinfo -->
  dcg_multi(iuserinfo_).
iuserinfo_ --> iunreserved.
iuserinfo_ --> 'pct-encoded'.
iuserinfo_ --> 'sub-delims'.
iuserinfo_ --> ":".

ihost --> 'IP-literal'.
ihost --> 'IPv4address'.
ihost --> 'ireg-name'.

'ireg-name' --> dcg_multi('ireg-name_').
'ireg-name_' --> iunreserved.
'ireg-name_' --> 'pct-encoded'.
'ireg-name_' --> 'sub-delims'.

% Begins with "/" or is empty.
ipath -->
  'ipath-abempty'.
% Begins with "/" but not "//".
ipath -->
  'ipath-absolute'.
% Begins with a non-colon segment
ipath -->
  'ipath-noscheme'.
% Begins with a segment.
ipath -->
  'ipath-rootless'.
% Zero characters.
ipath -->
  'ipath-empty'.

'ipath-abempty' -->
  dcg_multi('ipath-abempty_').
'ipath-abempty_' -->
  `/`,
  isegment.

'ipath-absolute' -->
  `/`,
  ('isegment-nz', dcg_multi('ipath-absolute_') ; ``).
'ipath-absolute_' -->
  `/`,
  isegment.

'ipath-noscheme' -->
  'isegment-nz-nc',
  dcg_multi('ipath-noscheme_', _).
'ipath-noscheme_' -->
  `/`,
  isegment.

'ipath-rootless' -->
  'isegment-nz',
  dcg_multi('ipath-rootless_').
'ipath-rootless_' -->
  `/`,
  isegment.

'ipath-empty' --> [].
'ipath-empty' -->
  ipchar,
  'ipath-empty'.

isegment -->
  dcg_multi(ipchar).

'isegment-nz' -->
  dcg_multi(ipchar, 1-_).
% Non-zero-length segment without any colon ":".
'isegment-nz-nc' -->
  dcg_multi('isegment-nz-nc_', 1-_).
'isegment-nz-nc_' --> iunreserved.
'isegment-nz-nc_' --> 'pct-encoded'.
'isegment-nz-nc_' --> 'sub-delims'.
'isegment-nz-nc_' --> `@`.

ipchar --> iunreserved.
ipchar --> 'pct-encoded'.
ipchar --> 'sub-delims'.
ipchar --> `:`.
ipchar --> `@`.

iquery --> dcg_multi(iquery_).
iquery_ --> ipchar.
iquery_ --> iprivate.
iquery_ --> `/`.
iquery_ --> `?`.

ifragment --> dcg_multi(ifragment_).
ifragment_ --> ipchar.
ifragment_ --> `/`.
ifragment_ --> `?`.

iunreserved --> 'ALPHA'.
iunreserved --> 'DIGIT'.
iunreserved --> `-`.
iunreserved --> `.`.
iunreserved --> `_`.
iunreserved --> `~`.
iunreserved --> ucschar.

%! iprivate//
% | *Hexadecimal*  | *Decimal*           |
% | xE000-F8FF     |    57,344-   63,743 |
% | xE0000-E0FFF   |   917,504-  921,599 |
% | xF0000-FFFFD   |   983,040-1,048,573 |
% | x100000-10FFFD | 1,048,576-1,114,109 |

iprivate -->
  [C],
  { between(57344, 63743, C)
  ; between(917504, 921599, C)
  ; between(983040, 1048573, C)
  ; between(1048576, 1114109, C) }.

