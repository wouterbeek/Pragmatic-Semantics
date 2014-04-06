:- module(rfc4234_abnf, []).

/** <module> RFC 4234 grammar

DCGs implementing the ABNF grammar rules defined in RFC 4234 (ABNF).

# RFC 4234

## ABNF & BNF

Differences between ABNF and standard BNF:
  * Alternatives
  * Angle brackets
    BNF requires angle brackets around rule names. ABNF allows this.
  * Naming rules
  * Order-independence
  * Repetition
  * Value ranges

## Concepts

* *Character*
  A non-negative integer.

## Rule names

Rule names are case insensitive.

Angle brackets may be used around rule names
  if this facilitates in discerning the use of a rule name.
This is typically restricted to rule name references in free-form prose,
  or to distinguish partial rules that combine into a string
  that is not separated by white space (see *repetition*).

## Rule form

~~~
name = elements crlf
~~~

For visual ease, rule definitions are left aligned.
When a rule requires multiple lines,
  the continuation lines are indented.
The left alignment and indentation are relative to
  the first lines of the ABNF rules
  and need not match the left margin of the document.

# Terminal values

Rules resolve into a string of terminal values,
  sometimes called characters.
A character is a non-negative integer.
A specific mapping (encoding) of values into a character can be specified.

### Single terminal value

~~~
percent base integer
~~~

Bases:
  * =b=
    binary
  * =d=
    decimal
  * =x=
    hexadecimal

Examples:
~~~{.abnf}
CR = %d13
CR = %x0D
~~~

### Multiple terminal values

A concatenated string of such values is specified compactly,
  using a period to separate the characters.

Example:
~~~{.abnf}
CRLF = %d13.10
~~~

Alternatively, a literal string that consists of US-ASCII characters only,
  can be given directly.
Such strings are case insensitive.

Example:
~~~{.abnf}
command = "command string"
~~~

@see Module DCG_CONTENT supports case-insensitive string parsing
     with ci_string//1.

### External encodings

External representations of terminal value characters will vary
  according to constraints in the storage or transmission environment.

By separating external encoding from the syntax,
  it is intended that alternate encoding environments can be used
  for the same syntax.

## Operations

### Concatenation

Space-separated elements.

### Alternatives

Forward-slash separated elements.

Example:
~~~{.abnf}
ruleset = alt1 / alt2 / alt3 / alt4 / alt5
~~~

### Incremental alternatives

It is sometimes convenient to specify a list of alternatives in fragments.
This is particularly useful for otherwise independent specifications that
derive from the same parent ruleset.

Format:
~~~
oldrule =/ additional-alternatives
~~~

Example:
~~~{.abnf}
ruleset =  alt1 / alt2
ruleset =/ alt3
ruleset =/ alt4 / alt5
~~~

### Value range alternatives

A range of alternative numeric values can be specified compactly,
  using a dash to indicate the range of alternative values.

Example:
~~~{.abnf}
DIGIT = %x30-39
~~~

Concatenated numeric values and numeric value ranges
  cannot be specified in the same string.

Example:
~~~{.abnf}
char-line = %x0D.0A %x20-7E %x0D.0A
~~~

### Sequence Group

Elements enclosed in parentheses are treated as a single element,
  whose contents are strictly ordered.

The sequence group notation is also used within free text
  to set off an element sequence from the prose.

### Variable Repetition:

The operator "=*=" preceding an element indicates repetition.
The full form is:
~~~
<a>*<b>element
~~~
where =|<a>|= and =|<b>|= are optional decimal values, indicating the least
and the most occurrences of the element.
Default values are =0= and =infinity=.

### Specific Repetition:

Rules of the form
~~~
<n>*<n>element
~~~
can be abbreviated with rules of the form:
~~~
<n>element
~~~

### Optional Sequence

Rules of the form:
~~~
*1(foo bar)
~~~
can be abbreviated by rules of the form:
~~~
[foo bar]
~~~

### Comment

A semicolon starts a comment that continues to the end of line.

### Precedence

From binding tightest to binding loosest:
  * Rule name, prose-val, Terminal value
  * Comment
  * Value range
  * Repetition
  * Grouping, Optional
  * Concatenation
  * Alternative

For example, alternatives are looser bound than concatenations.
Therefore [2] and [3] match the same strings, but [1] and [2] do not.

~~~{.abnf}
[1] elem (foo / bar) blat
[2] elem foo / bar blat
[3] (elem foo) / (bar blat)
~~~

Use of the alternative operator, freely mixed with concatenations, can be
confusing.
It is recommended that the grouping operator be used to make explicit
concatenation groups (as in [1] and [3]).

## ABNF ABNF

~~~{.abnf}
rulelist       =  1*( rule / (*c-wsp c-nl) )
rule           =  rulename defined-as elements c-nl
                      ; continues if next line starts
                      ;  with white space
rulename       =  ALPHA *(ALPHA / DIGIT / "-")
defined-as     =  *c-wsp ("=" / "=/") *c-wsp
                      ; basic rules definition and
                      ;  incremental alternatives
elements       =  alternation *c-wsp
c-wsp          =  WSP / (c-nl WSP)
c-nl           =  comment / CRLF
                      ; comment or newline
comment        =  ";" *(WSP / VCHAR) CRLF
alternation    =  concatenation
                 *(*c-wsp "/" *c-wsp concatenation)
concatenation  =  repetition *(1*c-wsp repetition)
repetition     =  [repeat] element
repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)
element        =  rulename / group / option /
                 char-val / num-val / prose-val
group          =  "(" *c-wsp alternation *c-wsp ")"
option         =  "[" *c-wsp alternation *c-wsp "]"
char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
                       ; quoted string of SP and VCHAR
                       ;  without DQUOTE
num-val        =  "%" (bin-val / dec-val / hex-val)
bin-val        =  "b" 1*BIT
                  [ 1*("." 1*BIT) / ("-" 1*BIT) ]
                       ; series of concatenated bit values
                       ;  or single ONEOF range
dec-val        =  "d" 1*DIGIT
                  [ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]
hex-val        =  "x" 1*HEXDIG
                  [ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]
prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
                       ; bracketed string of SP and VCHAR
                       ;  without angles
                       ; prose description, to be used as
                       ;  last resort
~~~

@author Wouter Beek
@version 2013/07-2013/08, 2013/12, 2014/03
*/

