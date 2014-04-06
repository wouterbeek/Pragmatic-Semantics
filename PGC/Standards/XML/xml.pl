:- module(
  xml,
  [
    xml_namespace//1 % :DCG_Namespace
  ]
).

/** <module> XML

The XML (Extensible Markup Language) is a subset of
SGML (Standard Generalized Markup Language).

# Design goals

* Straightforwardly usable over the Internet.
* Supporting a wide variety of applications.
* Compatible with SGML.
* Easy to write programs that process XML documents.
* Minimum number of optional features.
* XML documents should be human-legible and reasonably clear.
* "The XML design should be prepared quickly." [???]
* The design of XML shall be formal and concise.
* XML documents shall be easy to create.
* Terseness is of minimal importance.

# Concepts

  * **Document element**
    The single element in an XML document that has no parent element.
  * **Logical structure**
    The declarations, elements, comments, characters references, and
    processing instructions of an XML document.
  * **Physical structure**
    The entities / units that compose and XML document.
  * *Root*
    Synonym of _|document element|_.
  * *Validity*
    The property that an XML document complies with the constraints
    expressed in the document type declaration is references.
  * *Well-formedness*
    The property that an XML document matches the productions in the XML
    specification, meets all the well-formedness constraints, and contains
    only parsed entities that are well-formed.
  * **XML document**
    Can be split up in logical and physical structure.
  * **XML processor**
    A software module that can access the content and structure of
    XML documents.

@author Wouter Beek
@compat XML 1.0 (Fifth Edition)
@see http://www.w3.org/TR/2008/REC-xml-20081126/
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso, 'http://www.iso.org/').
:- xml_register_namespace(std, 'http://www.example.org/standards/').

:- meta_predicate(xml_namespace(//,?,?)).

/*
%! xml_document(-Tree:compound)//
% A data object is an **XML document** if it is *well-formed*.
%
% ~~~{.bnf}
% document ::= prolog element Misc*
% ~~~
%
% ## Logical and physical structure
%
% Each XML document has both a logical and a physical structure.
% *Physically*, the document is composed of units called entities.
% *Logically*, the document is composed of declarations, elements, comments,
% character references, and processing instructions.
%
% ## Validity
%
% It is *valid* if:
%   1. Taken as a whole, it matches the production xml_document//.
%   2. It meets all the well-formedness constraints.
%   3. Each of the parsed entities which is referenced directly or indirectly
%      within the document is well-formed.

xml_document(
  xml_prolog,
  xml_element,
  ("" ; xml_miscs).
*/

/*
xml_miscs --> [].
xml_miscs -->
  xml_misc,
  xml_miscs.
*/

%! xml_comment(-Tree:compound, ?Comment:atom)//

xml_comment(comment(Comment), Comment) -->
  {nonvar(Comment)}, !,
  {atom_codes(Comment, Codes)},
  less_than_sign, exclamation_mark, hyphen_minus, hyphen_minus, space,
  Codes,
  space, hyphen_minus, hyphen_minus, greater_than_sign.
xml_comment(comment(Comment), Comment) -->
  less_than_sign, exclamation_mark, hyphen_minus, hyphen_minus, space,
  Codes,
  space, hyphen_minus, hyphen_minus, greater_than_sign,
  {atom_codes(Comment, Codes)}.

xml_namespace(DCG_Namespace) -->
  {phrase(DCG_Namespace, "xml")},
  void.
xml_namespace(DCG_Namespace) -->
  DCG_Namespace.



/*
## Character references

Refer to specific characters in the ISO/IEC 10646 character set.

~~~{.txt}
CharRef ::= '&#' [0-9]+ ';'	| '&#x' [0-9a-fA-F]+ ';'
~~~

Must match the production for =Char=.


## Comments

*Comments* may appear outside other markup and in some locations of the DTD.


## Declarations


## Document type declarations

### External subset

A pointer to a special kind of _|external entity|_ containing
_|markup declarations|_.

### Internal subset

Direct inclusion of _|markup declarations|_ in an XML document.


## Markup declaration

**External markup declaration|*: A _|markup declaration|_ that occurs in the
_|external subject|_ or in an (internal or external) _|parameter entity|_.

### Attribute-list declaration

Attribute declaration:

~~~{.txt}
AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
AttDef      ::= S Name S AttType S DefaultDecl
~~~

##++ Attribute type

~~~{.txt}
AttType       ::= StringType | TokenizedType | EnumeratedType
StringType    ::= 'CDATA'
TokenizedType ::= 'ID'       |   // An XML name that is unique in the
                                 // XML document.
                  'IDREF'    |   // XML name, refering to the ID type
                                 // attribute of some element in the
                                 // XML document.
                  'IDREFS'   |   // Separated by whitespace.
                  'ENTITY'   |   // The name of an unparsed ENTITY defined
                                 // elsewhere in the DTD.
                  'ENTITIES' |   // Separated by whitespace.
                  'NMTOKEN'  |   // Like a name, but with no extra
                                 // restrictions on the first letter.
                  'NMTOKENS'     // Separated by whitespace.
~~~

Example of ID and IDREF attribute types:

~~~{.dtd}
<!ATTLIST employee social_security_number ID    #REQUIRED>
<!ATTLIST project  project_id             ID    #REQUIRED>
<!ATTLIST team_member person              IDREF #REQUIRED>
<!ATTLIST assignment  project_id          IDREF #REQUIRED>
~~~

##++ Enumerated attribute types

Enumerated attribute types:

~~~{.txt}
EnumeratedType ::= NotationType | Enumeration
NotationType   ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
~~~

Example of attribute type NOTATION:

~~~{.dtd}
<!NOTATION gif  SYSTEM "image/gif">
<!NOTATION tiff SYSTEM "image/tiff">
<!NOTATION jpeg SYSTEM "image/jpeg">
<!NOTATION png  SYSTEM "image/png">
<!ATTLIST  image type NOTATION (gif | tiff | jpeg | png) #REQUIRED>
~~~

##++ Attribute default values

Attribute defaults:
  * =#FIXED=
    Constant and immutable.
  * =#IMPLIED=
    Optional without default.
  * Literal
    Default given as quoted string.
  * =#REQUIRED=
    Requried without default.

Grammar:

~~~{.txt}
DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
~~~

Examples:

~~~{.dtd}
<!ATTLIST termdef
          id      ID      #REQUIRED
          name    CDATA   #IMPLIED>
<!ATTLIST list
          type    (bullets|ordered|glossary)  "ordered">
<!ATTLIST form
          method  CDATA   #FIXED "POST">
~~~

### Element type declaration

Element type declarations constrain the element's content and attribute
values.

An element must not be declared more than once.

~~~{.txt}
elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
~~~

Examples:

~~~{.dtd}
<!ELEMENT br EMPTY>
<!ELEMENT p (#PCDATA|emph)* >
<!ELEMENT %name.para; %content.para; >
<!ELEMENT container ANY>
~~~

##++ Element content

An element has **element content** if its content must only contain child
elements and no character data. The constrain on the element's content is
then a **content model|*.

Examples:
~~~{.dtd}
<!ELEMENT spec (front, body, back?)>
<!ELEMENT div1 (head, (p | list | note)*, div2*)>
<!ELEMENT dictionary-body (%div.mix; | %dict.mix;)*>
~~~

### Entity declaration

### Notation declaration

## XML declaration

Specifying version and optionally encoding.

### Standalone declaration

Optionally specified as part of the _|XML declaration|_.

Signals whether there are external declarations.

Nota that _|external entities|_ are not considered in the
standalone declaration.

### Mixed content

An element has **mixed content** if it may contain character data
and child elements. _|In this case the order and the number of occurrences
of child elements cannot be constrained.|_

~~~{.txt}
Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' |
          '(' S? '#PCDATA' S? ')'
~~~

Examples:
~~~{.dtd}
<!ELEMENT p (#PCDATA|a|ul|b|i|em)*>
<!ELEMENT p (#PCDATA | %font; | %phrase; | %special; | %form;)* >
<!ELEMENT b (#PCDATA)>
~~~

## Processing instructions

**Processing instructions** must be passed through to applications.



---


# Physical structure

The actual data used in an XML document.


## Entities

### Parsed entities / text entities

**Parsed entities** contain text data that becomes part of the XML document
after processing.

##++ Text

*Text* is any sequence of characters.

##+++ End-of-line handling

Normalize all line endings
(i.e. occurrences and/or combinations of #xD (carriage return) and #xA)
to #xA / line feeds.

##+++ CDATA

*CDATA* may occur anywhere _|character data|_ may occur.
In CDATA the less than sign and ampersand need not be escaped.

Anywhere character data can occur, CDATA can be used to escape blocks of
text containing characters which would otherwise be recognized as markup.

Delimiters:
  * Start tag: <![CDATA[
  * End tag: ]]>

~~~{.txt}
CDSect  ::= CDStart CData CDEnd
CDStart ::= '<![CDATA['
CData   ::= (Char* - (Char* ']]>' Char*))
CDEnd   ::= ']]>'
~~~

##+++ Character data

_Text_ that is not _markup_ is **character data|*.

~~~{.txt}
CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
~~~

Ampersand and left angle bracket must not occur except when used in markup
delimiters. Otherwise, use numeric character references or strings =&amp;=,
=&lt;=.

Right bracket must not occur in string "]]>" when not marking the end of a
CDATA section. Use numberic character reference or string =&gt;=.

Attribute values can contain single and double quotes, using =&apos;= and
=&quot;=.

##+++ Comments

~~~{.txt}
Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
~~~

Compatibility: Double hyphen must not occur in comments.

##+++ Literal data

**Literal data** is any quoted string not containing the quotation mark that
is used as the delimiter for that string.

**Literal data|*: Any quoted string not containing the quotation mark that
is used as the delimiter for that string.

Used for:
  * Content of internal entities. [???]
  * Values of attributes.
  * External identifiers. [???]

~~~{.txt}
EntityValue   ::= '"' ([^%&"] | PEReference | Reference)* '"' |
                  "'" ([^%&'] | PEReference | Reference)* "'"
AttValue      ::= '"' ([^<&"] | Reference)* '"' |
                  "'" ([^<&'] | Reference)* "'"
SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
PubidLiteral  ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
PubidChar     ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
~~~

##+++ Markup

The following _text_ is *markup*:
  * Start-tags
  * End-tags
  * Empty-element tags
  * Entity references
  * Character references
  * Comments
  * CDATA section delimiters
  * Document type declarations
  * Processing instructions
  * XML declarations
  * Text declarations
  * White space that is at the top level of the document entity.

##+++ Name

## Names

*Nmtoken*: Any mixture of name characters.

*Name*: An _Nmtoken_ with a restricted set of initial characters.
Disallowed: digits, diacritics [???], full stop [???], hyphen.

Reserved names:
  * Names beginning with =xml=.
  * Strings that would match =|(('X'|'x')('M'|'m')('L'|'l'))|=.

Avoid the use of colon in names except for namespace purposes.

~~~{.txt}
NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] |
                  [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] |
                  [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
                  [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
                  [#x10000-#xEFFFF]
NameChar      ::= NameStartChar | "-" | "." | [0-9] | #xB7 |
                  [#x0300-#x036F] | [#x203F-#x2040]
Name          ::= NameStartChar (NameChar)*
Names         ::= Name (#x20 Name)*
Nmtoken       ::= (NameChar)+
Nmtokens      ::= Nmtoken (#x20 Nmtoken)*
~~~

##+++ PI, Processing Instruction

Instructions for applications (specified by =PITarget=).

~~~{.txt}
PI        ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
PITarget  ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
~~~

Target names =xml= and =XML= are reserved.

Since Processing Instruction targets must be an XML name, they cannot contain
certain characters, e.g. slashes. A workaround is to define a NOTATION which
can identify a short XML name with more complicated content.

Example:

~~~{.dtd}
<!NOTATION tex SYSTEM "/usr/local/bin/tex">
~~~





##++ Attribute

Attribute value normalization algorithm:
  1. All line breaks are normalized to #xA.
  1. Begin with a normalized value consisting of the empty string.
  1. For each character, entity reference, or character reference in the
     unnormalized attribute value, do:
     1. For a character reference, append the referenced character to the
        normalized value.
     1. For an entity reference, recursively apply step 3 to the
        replacement text of the entity.
     1. For a white space character (#x20, #xD, #xA, #x9), append a
        space character (#x20) to the normalized value.
     1. For another character, append the character to the normalized value.
  1. If the attribute type is not CDATA, then discard leading and trailing
     spaces, and replace sequences of spaces by a single space character.

##++ Element

The boundaries of non-empty elements are delimited by start- and end-tags.

**GI, Generic Indentifier|*: the name of an element's type.

An element type may have associated attribute specifications.

~~~{.txt}
element      ::= EmptyElemTag | STag content ETag

// Empty element tag.
EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'

// Non-empty element tag.
STag         ::= '<' Name (S Attribute)* S? '>'
content      ::= CharData?
                 ((element | Reference | CDSect | PI | Comment) CharData?)*
ETag         ::= '</' Name S? '>'

// Attributes
Attribute    ::= Name Eq AttValue
~~~

##+++ Element content

~~~{.txt}
children ::= (choice | seq) ('?' | '*' | '+')?
cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'
~~~

### Unparsed entities

A container whose content may be anything but XML text.

### Predefined entities

### Internal entities

An entity in which no separate physical storage exists.
The content is provided in its declaration.

Example:
~~~{.dtd}
<!ENTITY Publisher1 "McGrawHill Publishing Company.">
~~~

### External entities

Refers to a storage unit.

Example:
~~~{.dtd}
<ENTITY FirstImg SYSTEM "www.books.com/images/book1.gif" NDATA GIF>
~~~

## Conditional sections

Portions of the Document Type Declaration external subset or of external
parameter entities that are included/excluded from the logical
structure of the DTD.

~~~{.txt}
conditionalSect    ::= includeSect | ignoreSect
includeSect        ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
ignoreSect         ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
Ignore             ::= Char* - (Char* ('<![' | ']]>') Char*)
~~~

Parameter entities can be redefined in the itnernal DTD subset of a document.

Example parameter entity:

~~~{.dtd}
<!ENTITY % notes_allowed "INCLUDE">
~~~

Example parameter entity reference:

~~~{.dtd}
<![%notes_allowed;[
  <!ELEMENT production_note (#PCDATA)>
]]>
~~~

This allows declarations to be turned on/off from outside of a DTD document.

Another example:

~~~{.dtd}
<!ENTITY % draft 'INCLUDE' >
<!ENTITY % final 'IGNORE' >

<![%draft;[
<!ELEMENT book (comments*, title, body, supplements?)>
]]>
<![%final;[
<!ELEMENT book (title, body, supplements?)>
]]>
~~~

## Entity

  * **Parsed entity**
    The contents of a parsed entity (called 'replacement text') are an
    integral part of an XML document.
    Invoked by name using **entity references|*.
  * **Unparsed entity**
    The contents of an unparsed entity need not be XML and need not even be
    text. The notation of an unparsed entity is identityfied by name.
    Invoked by name, given in the value of ENTITY or ENTITIES attributes. [???]
  * **General entities**
    Entities for use within document content.
  * **Parameter entities**
    Parsed entities for use in the DTD.

General and parameter entities:
  * use different forms of reference
  * are recognized in different contexts
  * occur in different namespaces

### Entity declaration

~~~{.txt}
EntityDecl ::= GEDecl | PEDecl
GEDecl     ::= '<!ENTITY' S Name S EntityDef S? '>'
PEDecl     ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
EntityDef  ::= EntityValue | (ExternalID NDataDecl?)
PEDef      ::= EntityValue | ExternalID
~~~

In case the same name is used multiple times, all but the first encountered
entity is dicarded.

### Entity reference

Refers to the contents of a named entity.

Parsed general entity references use ampersand and semicolon delimiters.

Parameter-entity references use percent and semicolon delimiters.

General entity declarations must precede any reference to it.

Example:
~~~{.dtd}
<!ENTITY super "supercalifragilisticexpialidocious">
~~~

Entity references can only refer to parsed entities.

Grammar:
~~~{.txt}
Reference   ::= EntityRef | CharRef
EntityRef   ::= '&' Name ';'
PEReference ::= '%' Name ';'
~~~

### External entity reference

Grammar:
~~~{.txt}
ExternalID ::= 'SYSTEM' S SystemLiteral |
               'PUBLIC' S PubidLiteral S SystemLiteral
NDataDecl  ::= S 'NDATA' S Name
~~~

Example:
~~~{.dtd}
<!ENTITY footer SYSTEM "http://www.oreilly.com/boilerplate/footer.xml">
~~~

The external file starts with a **text declaration|*.
This is like an XML declaration, but with required encoding declaration,
optional version information, and absent standalone declaration.

### External unparsed entity [NOT OFTEN USED?]

Non-XML formatted content stored in external files.

Example:
~~~{.dtd}
<!ENTITY turing_getting_off_bus
         SYSTEM "http://www.turing.org.uk/turing/pi1/bus.jpg"
         NDATA jpeg>
~~~

The =NDATA= declaration specifies the data type. These are defined in the
DTD using a NOTATION declaration.

Example:
~~~{.dtd}
<!NOTATION jpeg SYSTEM "image/jpeg">
~~~

Since entity references can only refer to parsed entities, external unparsed
entities cannot be included in an XML document by using an entitiy reference.
Instead, an element with an ENTITY type attribute whose value is the name
of the unparsed external entity must be defined.

Example:
~~~{.dtd}
<!ELEMENT image EMPTY>
<!ATTLIST image source ENTITY #REQUIRED>

<image source="turing_getting_off_bus"/>
~~~

### Internal entity

An entity with an =EntityValue=.

An internal entity is a parsed entity.

Example:
~~~{.dtd}
<!ENTITY Pub-Status "This is a pre-release of the specification.">
~~~

## Entity reference

A way of escaping characters.

Predefined entity references:
  * &lt;
  * &amp;
  * &gt;
  * &quot;
  * &apos;

## Language identification

Attribute =|xml:lang|= specifies the natural or formal language used in
the contents and attribute values of the element for which the attribute
is declared.

The values that may be declared for this attribute must come from BCP 47
and the empty string.

Example of a collection of French poems for English students, with glosses
and notes in English:
~~~{.dtd}
<!ATTLIST poem   xml:lang CDATA 'fr'>
<!ATTLIST gloss  xml:lang CDATA 'en'>
<!ATTLIST note   xml:lang CDATA 'en'>
~~~

## Parameter entity

Multiple elements may (partially) share the same attributes.

**Parameter entities** can be using in a DTD to introduce the same text
at multiple locations.

Example of a parameter entity specification:
~~~{.dtd}
<!ENTITY % residential_content "address, footage, rooms, baths">
~~~

Example of parameter entity references, using a parameter entity:
~~~{.dtd}
<!ELEMENT apartment (%residential_content;, %rental_content;)>
<!ELEMENT sublet    (%residential_content;, %rental_content;)>
~~~

Parameter entities can be redefined in the internal DTD subset of a document.

### external parameter entity

Parameter entity references to external parameter entities cause the contents
of external DTDs to be inserted.

Example:
~~~{.dtd}
<!ENTITY % names SYSTEM "names.dtd">
%names;
~~~

## PCDATA

Parsed character data.

## Prolog

An XML document must begin with an XML declaration specifying the XML version.

*Validity*: An XML document is valid if it has an associated DTD and
the document complies with the constraints in the DTD.

The Document Type Declaration that identifies a DTD, Document Type Definition,
must appear before the first element in the XML document.

~~~{.txt}
prolog      ::= XMLDecl? Misc* (doctypedecl Misc*)?
XMLDecl     ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
Eq          ::= S? '=' S?
VersionNum  ::= '1.' [0-9]+
Misc        ::= Comment | PI | S
~~~

A DTD consists of **markup declarations|*:
  * Element type declarations
  * Attribute-list declarations
  * Entity declarations
  * Notation declarations
  * The above may be enclosed within parameter entities.

MIME media type: =|application/xml-dtd|=

~~~{.txt}
doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
DeclSep     ::= PEReference | S
intSubset   ::= (markupdecl | DeclSep)*
markupdecl  ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
~~~

Example:
~~~{.dtd}
<!DOCTYPE person SYSTEM "http://www.cafeconleche.org/dtds/person.dtd">
<!DOCTYPE rss PUBLIC "-//Netscape Communications//DTD RSS 0.91//EN"
              "http://my.netscape.com/publish/formats/rss-0.91.dtd">
~~~

* =Name= must match the element type of the root element.

**Entity references** provide replacement text for parts of the DTD
that will be included in the XML document.

**Parameter entities** provide replacement text for excluse use inside a DTD.

## External subset

~~~{.txt}
extSubset     ::= TextDecl? extSubsetDecl
extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
~~~

**External markup declaration|*:
A markup declaration occurring in an external subset or
in a parameter entity. [???]

~~~{.txt}
SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
~~~

# Text

*Text*: A sequence of characters that may represent markup or character data.

## White space preservation

Attribute =|xml:space|= indicates that applications should preserve white
space occurring in the element for which the attribute is defined.

The attribute must be declared by being given an enumerated type with at
least one of the values =default= and =preserve=.

Examples:
~~~{.dtd}
<!ATTLIST poem  xml:space (default|preserve) 'preserve'>
<!ATTLIST pre xml:space (preserve) #FIXED 'preserve'>
~~~
*/

