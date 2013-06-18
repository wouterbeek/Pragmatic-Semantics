Component Definition Hierarchy

# Introduction

This directory contains the component definitions for a cognitive diagnosis
domain.
Adding a file to this directory means adding a component definition to the
GDE component library.
The component definitions that are added to this file should adhere to a
specific component definition specification language that allows components
from disparate domains to be defined.
This means that the cognitive diagnosis can be run on solving arithmatic
problems by, among other things, adding new definitions to this directory.
(The other two things to do are adding expression definitions to the
expression directory, and adding behavioral predicates to the CCM behavior
module.)

There are two types of component definitions:

1. Higher component definitions that only have other component defintions as
   their subclasses. Higher component definitions are used to structure the
   component definition hierarchy and to register defining properties at a
   higher level.
2. Lower component definitions that can have instantiations in the form of
   component instances. It is possible for a lower component definition to
   also have subclass component definitions of both the higher and lower
   component definition type.

correction

# Component fitting algorithm

correction

1. For every support expression, find the component definitions that go with
   it.
2. For every component definition, do the following:
    1. Retrieve the condition that should be met for the component definition
       to apply.
    2. Check whether the requirements for the component definition are met.
    3. Check whether the condition of the component definition holds:
        4. Decompose the sequence of function applications to the deepest
           level, i.e. to the level of constants.
        5. Resolve the constants.
        6. Apply arbitrary sequences of stores, registered operations, and
           linking functions.
        7. If the all function applications succeed, then the store that has
           been build in the mean time is gathered in the results for the
           given support expression and component.

Because of Prolog's backtracking behavior, all possible solutions to the
linking functions and registered operations are considered. For every
succesful sequence of fucntion applications a list of stored information is
returned.

### Linking functions

Much of the reasoning that is required to fit components involves information
from the component connection model.

A function which returns a resource that is directly linked
to the argument resource.
Applying linking functions to the
component definition returns the properties of that component.
This is why we use the =|in1|=, =|in2|= and =|out1|= links in our
condition specification. In this way we can reuse the designation of
in- and output points.
Applying linking functions to the support expression returns the arguments and
the relation type of the support expression.
Linking functions can be applied to any resource in the CCM, allowing the
whole representation to be traversed.

Every function application that is not =|store|= nor one of the
registered required functions for the component definition under
consideration, is considered to involve a linking function.

### Constants

All reasoning regarding the fitting of components needs to be grounded in the
component connection model. We use two constant in our component definition
specification language to allow for this grounding:
* =|comp|= returning the component definition.
* =|supp|= returning the support expression.

The component definition constant is needed to incorporate the knowledge of
the component under consideration. It allows us to ask for the generic
properties that every component definition should have:

* What are the conditions under which a component can be added?
* What are the specifications for the various in- and output points?

In addition to that, the component gives us information that is peculiar to
that component type (or family of types for higher-level components):

* What is the sign of a causal component.
* Is the deduction the component represents directed or undirected with
  respect to its in- and output points?

The support expression is needed to incorporate the knowledge of the specific
expressions between which a component can be fitted.
The support knowledge relates 

### Example condition satisfaction

Operation = and(neq(has_in2(comp), 0), neq(has_in1(comp), has_out1(comp)))

A condition for a component definition is a compound term of function
applications that should hold in the component connection model in order for
the component to apply.

# COMP-DEF: The Component Definition Specification Language

Component definitions are given in RDF. For this purpose any RDF serialization
that can be translated to Prolog can be used. At this point we support the
RDF/XML and Turtle serializations, but adding aditional ones should be
trivial.

## Component Definition Properties and Relations

* =|rdfs:subClassOf, Class|= The RDFS property defining the parent/child relation
between the defined component and existing higher definitions. Every
component definition is, according to RDFS custom, automatically a child of
=|rdfs:Class|=.
* =|rdfs:label, Literal|= The RDFS property that relates a literal naming
the component definition in human-readable terms. This label is used to
display the component definition as well as its instances in the various
graph outputs. As is the case with any literal, the natural language in which
the label should be read can be provided (default is =|en|= for US English).
(Some of the graph outputs are natural language dependent and thus utilize
this feature.)
* =|component:has_support, ExpressionDefinition|= Marks the given expression
definition as providing the support knowledge for the component definition.
Only a single support expression can be specified for a component definition.
For combinations of support expressions (of arbitrary complexity) seperate
expression definitions should be defined. (See the expression hierarchy
documentation for more information.)
* =|component:has_sign, Integer|= Sets the sign of the component, if any.
The values have three interpretations |=<-inf, 0>|=, =|[0]|=, and
|=<0, inf>|=.
* =|component:has_in#, Code|= Specifies the value of the #th input gate.
For =|#|= a positive integer should be added.
There can be any positive number of input gates.
Although the distinction between in- and output gates is arbitrary from the
standpoint of consistency-based diagnosis, the distinction is used by other
modules, e.g. the one for causal explanation (which distinguishes forward from
backward reasoning).
* =|component:has_out#, Code|= Specifies the value of the #th output gate.
For =|#|= a positive integer should be added.
There can be any positive number of output gates.
Although the distinction between in- and output gates is arbitrary from the
standpoint of consistency-based diagnosis, the distinction is used by other
modules, e.g. the one for causal explanation (which distinguishes forward from
backward reasoning).
* =|component:has_condition, Code|= Specifies the conditions that should be
met in order for instantiations of this component definition to apply.
Either multiple conditions can be attributed, or combined using the =|and|=
connective.
Conditions are defined in terms of the various in- and output gates.
No condition can use a gate that is not defined using =|component:has_in#|=
or =|component:has_out#|=. A gate need not be used by any condition, making it
an idle gate.
(See the documentation for the CCM-CONDITION specification language.)
(Code is a datatype we defined for easy recognition by Prolog's term store.)

## Lower Component Definition Specifications

* =|component:can_instantiate|= The property stating that this is a lower
component definition. Setting this property to =|1|= allows the CCM to look
for instantiations of this component.

# Store

The store collects the points between which a component could be asserted.
The store is extended by applying store operations.
Store operations have syntax =|store(Label:atom, Point:uri)|=.
* =|Label|= Either =|in|= or =|out|=, followed by an integer.
  The integers should reflect the integer identifier of the
  =|has_input|= and =|has_output|= properties.
* =|Point|= The URI of a point. This point is stored under the given label.

# Modularity, Extensibility, and Conceptual Reuse of the Component Definition Hierarchy

The strength of the component hierarchy is optimally utilized by specifying
properties and relations at the right level of generality/specificity. E.g.
causal components share the condition

==
eq(has_in1(comp), q_mult(has_sign(comp), has_out1(comp)))
==

as well as the output gate specification

==
normalize(has_to_argument(store(out1, has_derivative_expression(has_to_argument(supp)))))
==

but not the input gate specification. Proportionality and influence components
are distinguised by their input gate specifications:

==
normalize(has_to_argument(store(in1, has_magnitude_expression(has_from_argument(supp)))))
==

and

==
normalize(has_to_argument(store(in1, has_derivative_expression(has_from_argument(supp)))))
==
