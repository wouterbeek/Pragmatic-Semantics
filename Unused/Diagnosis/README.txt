---+ Diagnosis

Many systems for cognitive diagnosis have been implemented over the years.
The present implementation aims at establishing a framework for cognitive
diagnosis, rahter than a dedicated or `single-purpose' diagnostic system.
In order to do this, we have gone to great lengths to separate out data from
procedures.

The big differentiator between various tasks of cognitive diagnosis mainly
consists of two things:
  1. The expressions that express the beliefs or cognitve state of the learner.
     These are the annotations that are given to the various points in the
     component connection model on which the diagnosis is run.
  2. The components that are used to connect the various points.

In the above we collated points and expressions (in 1). Also, we collated
components and component definitions (in 2). We begin now with distinguishing
between a definition and expression level, and an implementation and point
level. The reason for this is that the complexity of QR as a 

The basic idea behind this distinction is that we do not want to make the same
derivation twice.
In QR it is very common to make the same derivation twice because of two
reasons:
  1. Due to the sparsity of the value spaces (using few labels instead of many
     numberic values) the same value assignment is more likely to occur.
  2. Because of the explicit representation of time, it is common to have
     knowledge replicated across multiple behavioral states.
     Since a behavioral state is distinct if it only differs in one respect
     from all other states, there may be a lot of identical reasoning steps
     between two subsequent states.

Because of the above two reasons we want to find components at a more
general level than the state graph. We call this level the global space.
In the global space we assert every expression that occurs in the simulation.

---+ TODO

  1. There is a problem with support expressions for the QC component.
     When the component has a quantity relation support expression in
     the simulation results, then a user cannot give a probe point
     answer in which the component has a value relation support
     expression. This would need: (1) a to magnitude field, (2) a to
     derivative field, (3) a relocated relations field (rhs), (4) methods
     for updating the various lists upon selecting from/to quantities, (5)
     activation updates for the new fields, (6) a rewite method in
     formulate_expectation for converting value relation expressions to
     quantity relation ones. The other way round is underdetermined and
     would need additional information from the user (to be given via these
     new fields), and - finally - (7) methods for asking for probe pont
     information (by highlighting fields).
