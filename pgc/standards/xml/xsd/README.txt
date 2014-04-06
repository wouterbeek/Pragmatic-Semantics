XML Schema 2: Datatypes (Second Edition)

# Choices I made while implementing this standard

The *|value space|* of each XSD datatype is a subset of 
the values that can be expressed directly in SWI-Prolog.

The *|lexical mapping|* is a function that reads
codes from a stream that contains an XSD-supporting serialization format,
and that creates the corresponding SWI-Prolog value.

The *|canonical mapping|* is a function that takes a SWI-Prolog value
and returns a code list representing that value's equivalent in XML Schema
format, ready to be plugged into a DCG.

The *|lexical space|* of each XSD datatype is implemented by
SWI-Prolog lists of codes.

# Standards version 1.0 or 1.1

In principle, the XSD version should follow the XML version that is used.
But the user should always be able to override this choice.

# Datatype

A datatype is defined as a triple that consists of:
  * A *|value space|* which is a set of distinct abstract values.
  * A *|Lexical space|* which is a set of concrete lexical representations
    or literals which denote values in the value space.
  * A collection of *facets* which are characterizing properties
    of the datatype's value space, individual values, or its lexical space.
  * Together with the facets comes a collection of
    *functions*, *relations*, and *operations*.
    The following are always included:
      * A many-to-one _|lexical mapping|_
        from the lexical space to the value space.
      * A one-to-one _|canonical mapping|_
        from the value space to the lexical space.
      * An _|identity relation|_.
      * An _|equality relation|_.
      * An _|order relation|_.

# Value space

Each value in the value space of a datatype is denoted by
at least one literal in the lexical space of the same datatype.

The value space can be definition in the following ways:
  1. *Intensional*
     Axiomatic definition from fundamental notions.
  2.  *Extensional*
     Enumeration of the values.
  3.  *Derived*
     Defined by restricting an existing value space.
  4. *Combined* out of existing value spaces, according to some
     construction procedure (e.g., list, union).

An *|ineffable value|* ia a value in a value space that is not mapped to
by any literal from the corresponding lexical space.

## Identity

I do not understand what the following means:
"The identity relation is always defined.
Every value space inherently has an identity relation.
Two things are identical if and only if they are actually the same thing:
i.e., if there is no way whatever to tell them apart." [XSD v1.1 sec2.2.1]

Lists =A= and =B= are identical iff they have the same length and
their items are pairwise identical.

Note that all the empty lists are identical.

Values from the value spaces of different primitive datatypes are
never identical.

## Equality

Each primitive datatype has prescribed an equality relation for its
value space.

The equality relation is not always the same as the identity relation.
Examples:
  * Float and double =-0= and =+0= are equal but not identical.
  * Two dateTime values may denote the same moment in time,
    but doing so with different local times and different (correcting)
    time zone offsets.
    These values are equal but not identical.

The equality relation is not always complete.
Example:
  * Float and double =NaN= is not even equal to itself,
    but is identical to itself.

Lists =A= and =B= are equal iff they have the same length and
their items are pairwise identical.

Note that all the empty lists are equal.

A list of length one containing value =V1= is equal to an atomic value =V2=
iff =V1= is equal to =V2=. (I.e., =|[V1] = V2|= iff =|V1 = V2|=.)

Values from the value spaces of different primitive datatypes are
never equal.

## Order

Values =a= and =b= are *incomparable* iff
$a \nleq b \land a \neq b \land a \ngeq b$.
Values are comparable if there are not incomparable.

The *|incomparable relation|* is denoted =|<>|=.

The weak order $\leq$ means $<$ or $=$ and _|one can tell which|_.
For example, the duration =P1M= (one month) is not $\leq$ =P31D=
(thirty-one days) because =P1M= $<$ =P31D= nor =P1M= $=$ =P31D= is the case.
Instead, =P1M= is incomparable with =P31D=.

Values from the value spaces of different primitive datatypes are always
incomparable.

# Lexical space

The *|lexical mapping|* for a datatype is a prescribed relation which maps
from the lexical space of the datatype into its value space.

The *|lexical space|* of a datatype is the prescribed set of strings which
the lexical mapping for that datatype maps to values of that datatype.

The members of the lexical space are *|lexical representations|* of the
values to which they are mapped.

Characteristics:
  * Interoperability: minimum number of literals for the same value.
  * Readability: non-binary; text.
  * Parsing and serialization: taken from common languages and libraries.

Functional nature of lexical mappings:
  * The lexical mapping for primitive and ordinary datatypes are onto
    (and into) (defined in module [function.pl]).
  * The lexical mapping for special datatypes are into (not onto),
    i.e., some values are ineffable (defined in module [function.pl]).
  * The lexical mapping for union datatypes need not be functional, since
    the same literal may map to different values in different member types.
  * The lexical mapping for list datatypes is a function iff the lexical mapping
    for each list item type is a function.

A sequence of zero or more characters in the Universal Character Set (UCS)
which may or may not prove upon inspection to be a member of the lexical space
of a given datatype and thus a lexical representation of a given value in that
datatype's value space, is referred to as a *literal*.

### Pre-lexical facet values [?]

If a derivation introduces a _|pre-lexical facet value|_ (a new value for
whiteSpace or an implementation-defined pre-lexical facet), the corresponding
_|pre-lexical transformation of a character string|_, if indeed it changed
that string, could prevent that string from ever having the lexical mapping
of the derived datatype applied to it. Character strings that a pre-lexical
transformation blocks in this way (i.e., they are not in the range of the
pre-lexical facet's transformation) are always dropped from the derived
datatype's lexical space.

One should be aware that in the context of XML schema-validity assessment,
there are pre-lexical transformations of the input character string
(controlled by the whiteSpace facet and any implementation-defined pre-lexical
facets) which result in the intended literal. Systems other than XML
schema-validity assessment utilizing this specification may or may not
implement these transformations. If they do not, then input character strings
that would have been transformed into correct lexical representations, when
taken "raw", may not be correct lexical representations.

### Derivations removing lexical representations

Should a derivation be made using a derivation mechanism that removes
lexical representations from the lexical space to the extent that one or more
values cease to have any lexical representation, then those values are dropped
from the value space. This could happen by means of a pattern or other
lexical facet, or by a pre-lexical facet as described above.

### Derivations removing values

Conversely, should a derivation remove values then their lexical
representations are dropped from the lexical space unless there is a facet
value whose impact is defined to cause the otherwise-dropped lexical
representation to be mapped to another value instead. There are currently no
facets with such an impact. There may be in the future.

## Canonical mapping

The *|canonical mapping|* is a prescribed subset of the inverse of a
lexical mapping which is one-to-one and whose domain (where possible) is the
entire range of the lexical mapping (the value space). Thus a canonical
mapping selects one lexical representation for each value in the value space.

The *|canonical representation|* of a value in the value space of a datatype
is the lexical representation associated with that value by the datatype's
canonical mapping.

Canonical mappings are not available for datatypes whose lexical mappings are
_|context dependent|_.

## Dimensions of datatype definitions

### Atomic, list, union datatypes

#### Atomic datatype

An *|atomic value|* is an elementary value, not constructed from simpler
values by any user-accessible means defined by this specification.

Atomic values are sometimes regarded, and described, as "not decomposable",
but in fact the values in several datatypes defined here are described with
internal structure, which is appealed to in checking whether particular values
satisfy various constraints (e.g. upper and lower bounds on a datatype).
Other specifications which use the datatypes defined here may define
operations which attribute internal structure to values and expose or act
upon that structure.

An *|atomic datatype|* is a datatype whose value space contains only atomic
values. Atomic datatypes are =anyAtomicType= and all datatypes derived from
it.

The lexical space of an atomic datatype is a set of literals whose internal
structure is specific to the datatype in question.

There is one special atomic datatype (=anyAtomicType=), and a number of
_|primitive atomic datatypes|_ which have =anyAtomicType= as their base type.
All other atomic datatypes are derived either from one of the primitive atomic
datatypes or from another ordinary atomic datatype. No user-defined datatype
may have =anyAtomicType= as its base type.

#### List datatype

A *|list datatype|* is a datatype that has values which consist of a
finite-length (possibly empty) sequence of atomic values.
The values in a list are drawn from some atomic datatype (or from a union
of atomic datatypes), which is the item type of the list.

The item type of a list may be any atomic datatype, or any union datatype
whose basic members are all atomic datatypes (so a list of a union of atomic
datatypes is possible, but not a list of a union of lists).
The item type of a list must not itself be a list datatype.

List datatypes are always constructed from some other type;
they are never primitive. The value space of a list datatype is the set of
finite-length sequences of zero or more atomic values where each atomic value
is drawn from the value space of the lists's item type and has
a lexical representation containing no whitespace. The lexical space of a list
datatype is a set of literals each of which is a space-separated sequence of
literals of the item type.

The atomic or union datatype that participates in the definition of a list
datatype is the *|item type|* of that list datatype. If the item type is
a union, each of its basic members must be atomic.

A list datatype can be constructed from an ordinary or primitive atomic
datatype whose lexical space allows whitespace (such as =string= or =anyURI=)
or a union datatype any of whose member type definitions' lexical space
allows space. Since list items are separated at whitespace before the
lexical representations of the items are mapped to values, no whitespace will
ever occur in the lexical representation of a list item, even when the item
type would in principle allow it.

Constraint facets that restrict list datatypes:
  * =length=, =maxLength=, =minLength=
    The (minimum/maximum) number of list items.
  * =enumeration=
    Enumerated values are compared to the entire list, not to list items.
  * =pattern=
    Patterns apply to the memvers of the list datatype's lexical space,
    not to the members of the lexical space of the item type.
  * =whiteSpace=
    Fixed to the value =collapse=.
  * =assertions=

The canonical mapping of a list datatype maps each value onto the
space-separated concatenation of the canonical representations of all the
items in the value (in order), using the canonical mapping of the item type.

Examples:
~~~
<simpleType name='sizes'>
  <list itemType='decimal'/>
</simpleType>
<cerealSizes xsi:type='sizes'> 8 10.5 12 </cerealSizes>
~~~

~~~
<simpleType name='listOfString'>
  <list itemType='string'/>
</simpleType>
<someElement xsi:type='listOfString'>
this is not list item 1
this is not list item 2
this is not list item 3
</someElement>
~~~

~~~
<xs:simpleType name='myList'>
  <xs:list itemType='xs:integer'/>
</xs:simpleType>
<xs:simpleType name='myRestrictedList'>
  <xs:restriction base='myList'>
    <xs:pattern value='123 (\d+\s)*456'/>
  </xs:restriction>
</xs:simpleType>
<someElement xsi:type='myRestrictedList'>123 456</someElement>
<someElement xsi:type='myRestrictedList'>123 987 456</someElement>
<someElement xsi:type='myRestrictedList'>123 987 567 456</someElement>
~~~

#### Union datatype

A *|union datatype|* is (1) a datatype whose value space, lexical space, and
lexical mapping is the ordered union of the value spaces, lexical spaces, and
lexical mappings of one or more other datatypes, which are the member types
of the union, or (2) a datatype that is derived by facet-based restriction
from another union datatype.

Any primitive or ordinary datatype may occur among the member types of a
union. (In particular, union datatypes may themselves be members of unions,
as may lists.) The only prohibition is that no special datatype may be a
member of a union.

The lexical mapping of a union is not necessarily a function, since a given
literal may map to several values of different primitive datatypes.

~~~
<attributeGroup name="occurs">
  <attribute name="minOccurs" type="nonNegativeInteger"
    use="optional" default="1"/>
  <attribute name="maxOccurs"use="optional" default="1">
    <simpleType>
      <union>
        <simpleType>
          <restriction base='nonNegativeInteger'/>
        </simpleType>
        <simpleType>
          <restriction base='string'>
            <enumeration value='unbounded'/>
          </restriction>
        </simpleType>
      </union>
    </simpleType>
  </attribute>
</attributeGroup>
~~~

The datatypes that participate in the definition of a union datatype are
the *|member types|* of that union datatype. Any nonnegative number of
primitive datatypes can participate in a union type.

The *|transitive membership|* of a union is the set of its own member types,
and the member types of its members, and so on.

The *|basic members|* of a union datatype U are those members of
the transitive membership of U which are not union datatypes.

For a datatype M that is in the transitive membership of a union datatype U,
there is a sequences $\langle N_1, \ldots, N_n \rangle$ such that
$N_{i+1}$ is a datatype that is one of the member types for
the union datatype $N_i$, and $N_1 = U$ and $N_n = M$.
$N_2$ through $N_{n-1}$ are the *|intervening unions|*.

In a valid instance of any union, the first of its members in order which
accepts the instance as valid is the *|active member type|*.

If the active member type is itself a union, one of its members will be
its active member type, and so on, until finally a basic (non-union) member
is reached. That basic member is the *|active basic member|* of the union.

During validation an element or attribute's value is validated against
the member types in the order in which they appear in the definition
until a match is found. This can be overridden with =|xsi:type|=.

Example:
~~~{.xml}
<xs:element name='size'>
  <xs:simpleType>
    <xs:union>
      <xs:simpleType>
        <xs:restriction base='integer'/>
      </xs:simpleType>
      <xs:simpleType>
        <xs:restriction base='string'/>
      </xs:simpleType>
    </xs:union>
  </xs:simpleType>
</xs:element>

<size>1</size>
<size>large</size>
<size xsi:type='xs:string'>1</size>
~~~

Constraint facets that restrict union datatypes:
  * =assertions=
  * =enumeration=
  * =pattern=

### Special, primitive, ordinary datatypes

#### Special datatypes

The *|special datatypes|* are =anySimpleType= and =anyAtomicType=.
They are special by virtue of their position in the type hierarchy.

#### Primitive datatypes

*|Primitive datatypes|* are those datatypes that are not special
and are not defined in terms of other datatypes; they exist ab initio.
All primitive datatypes have =anyAtomicType= as their base type, but their
value and lexical spaces must be given in prose; they cannot be described
as restrictions of =anyAtomicType= by the application of particular
constraining facets.

#### Ordinary datatypes

*|Ordinary datatypes|* are all datatypes other than the special and
primitive datatypes. Ordinary datatypes can be understood fully in terms of
their Simple Type Definition and the properties of the datatypes
from which they are constructed.

A datatype is defined by *|facet-based restriction|* of another datatype
(its *|base type|*), when values for zero or more constraining facets
are specified that serve to constrain its value space and/or its lexical space
to a subset of those of the base type. The base type of a facet-based
restriction must be a primitive or ordinary datatype.

### Definition, derivation, restriction, construction

#### Definition

By *definition* is meant the explicit identification of the relevant
properties of a datatype, in particular its value space, lexical space,
and lexical mapping.

#### Derivation

A datatype T is *|immediately derived|* from another datatype X if and only if
X is the base type of T.

Every datatype other than =anySimpleType= is derived from =anySimpleType=.

A datatype R is *derived* from another datatype B if and only if:
  * B is the base type of R, or
  * There is some datatype X such that X is the base type of R,
    and X is derived from B.

#### Restriction

A datatype R is a *restriction* of another datatype B when:
  * The value space of R is a subset of the value space of B, and
  * The lexical space of R is a subset of the lexical space of B.

All datatypes are restrictions of =anySimpleType=.

#### Construction

All ordinary datatypes are defined in terms of, or *constructed* from,
other datatypes, either by (1) restricting the value space or lexical space
of a base type using zero or more constraining facets or
(2) by specifying the new datatype as a list of items of some item type,
or (3) by defining it as a union of some specified sequence of member types.

All ordinary datatypes are constructed, and all constructed datatypes are
ordinary.

### Built-in, user-defined datatypes

A datatype which is not available for use is said to be *unknown*.

#### Built-in

*|Built-in datatypes|* are those which are defined in this specification;
they can be special, primitive, or ordinary datatypes.

#### User-defined

*|User-defined datatypes|* are those datatypes that are defined by
individual schema designers.

## Namespace

The base URI is the URI of the XML Schema namespace:
  * For use in the XML Schema definition language:
    =|http://www.w3.org/2001/XMLSchema#|=.
  * For use in other contexts:
    =|http://www.w3.org/2001/XMLSchema-datatypes#|=.

The fragment identifier is either the name of a datatype, the name of a facet,
or the name of a Simple Type Definition followed by a dot followed by
the name of a facet.

## Infinite datatypes

Some primitive datatypes defined in this specification have infinite
value spaces; no finite implementation can completely handle all their
possible values. For some such datatypes, minimum implementation limits
are specified below. For other infinite types such as =string=, =hexBinary=,
and =base64Binary=, no minimum implementation limits are specified.

When presented with a literal or value exceeding the capacity of its partial
implementation of a datatype, a minimally conforming implementation of this
specification will sometimes be unable to determine with certainty whether
the value is datatype-valid or not. Sometimes it will be unable to represent
the value correctly through its interface to any downstream application.

When either of these is so, a conforming processor must indicate to the
user and/or downstream application that it cannot process the input data
with assured correctness (much as it would indicate if it ran out of memory).
When the datatype validity of a value or literal is uncertain because it
exceeds the capacity of a partial implementation, the literal or value must
not be treated as invalid, and the unsupported value must not be quietly
changed to a supported value.

Minimally conforming processors which set an application- or
implementation-defined limit on the size of the values supported must clearly
document that limit.

These are the partial-implementation minimal conformance requirements:
  * Must support decimal values whose absolute value can be expressed as
    =|i / 10k|=, where =i= and =k= are nonnegative integers such that
    =|i < 1016|= and =|k ≤ 16|= (i.e., those expressible with sixteen total
    digits).
  * Must support nonnegative year values less than =10000=
    (i.e., those expressible with four digits) in all datatypes which use
    the seven-property model and have a non-absent value for year
    (i.e. =dateTime=, =dateTimeStamp=, =date=, =gYearMonth=, and =gYear=).
  * Must support second values to milliseconds (i.e. those expressible with
    three fraction digits) in all datatypes which use the seven-property
    model and have a non-absent value for second (i.e. =dateTime=,
    =dateTimeStamp=, and =time=).
  * Must support fractional-second duration values to milliseconds
    (i.e. those expressible with three fraction digits).
  * Must support duration values with months values in the range =−119999=
    to =119999= months (=9999= years and =11= months) and seconds values
    in the range =−31622400= to =31622400= seconds (one leap-year).

## Property models

For some datatypes the values are described as objects that have various
properties, which in turn have more primitive values.

An *|optional property|* is one that is permitted but not required to have
the distinguished value *absent*.

The value *absent* is used as a distinguished value to indicate that
a given instance of a property "has no value" or "is absent".

More primitive values that are used to construct object value spaces:
  * A *number* (without precision) is an ordinary mathematical number;
    =1=, =1.0=, and =1.000000000000= are the same number.
  * A *|special value|* is an object whose only relevant properties for
    purposes of this specification are that it is distinct from, and
    unequal to, any other values (special or otherwise).
    Special values can be distinguished from each other in the general case
    by considering both the name and the primitive datatype of the value.
    E.g., =float='s =positiveInfinity= is not the same special value as
    =double='s =positiveInfinity=.

## Datatypes

[[xsd_hierarchy.jpg]]

### =anySimpleType=

=anySimpleType= is a special restriction of =anyType=.
The lexical space of =anySimpleType= is the set of all sequences of Unicode
characters(xml_char// as defined by XML 1.0 or XML 1.1),
and its value space includes all atomic values and all
finite-length lists of zero or more atomic values.

The lexical space of =anySimpleType= is the union of the lexical spaces
of all primitive and all possible ordinary datatypes.

The lexical mapping of =anySimpleType= is the union of the lexical mappings
of all primitive datatypes and all list datatypes. It is not a function.

Some values have no lexical representation.

Constraining facets are not directly applicable to =anySimpleType=.

### =anyAtomicType=

=anyAtomicType= is a special restriction of =anySimpleType=.
The value and lexical spaces of =anyAtomicType= are the unions of the value
and lexical spaces of all the primitive datatypes (either defined here or
supplied as implementation-defined), and =anyAtomicType= is their base type.

## Common definitions

#### =div=

If =m= and =n= are numbers, then =|m div n|= is the greatest integer less
than or equal to =|m / n|=.

=|n div 1|= is a convenient and short way of expressing
"the greatest integer less than or equal to n".

#### =mod=

If =m= and =n= are numbers, then =|m mod n|= is  =|m − n × (m div n)|=.

## Facets

### =ordered=

For some datatypes, this document specifies an order relation for their
value spaces; the =ordered= facet reflects this. It takes the values =total=,
=partial=, and =false=, with the meanings described below.

For the *|primitive datatypes|*, the value of the =ordered= facet is specified
in xsd_facets.txt.
For *|ordinary datatypes|*, the value is inherited without change from the
base type. For a *list*, the value is always false; for a *union*,
the value is computed as described below.

Values:
  * =false=, no order.
  * =partial=, partial order.
  * =total=, total order.

The value =false= in the ordered facet does not mean no partial or
total ordering exists for the value space, only that none is specified
by this document for use in checking upper and lower bounds.
Mathematically, any set of values possesses at least one trivial partial
ordering, in which every value pair that is not equal is incomparable.

When new datatypes are derived from datatypes with partial orders,
the constraints imposed can sometimes result in a value space for which the ordering is total, or trivial. The value of the ordered facet is not, however, changed to reflect this. The value partial should therefore be interpreted with appropriate caution.

