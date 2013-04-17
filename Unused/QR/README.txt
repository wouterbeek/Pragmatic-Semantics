---+ Overview

The GARP engine uses various badly designed database formats. Moreover, the
various data formats are not shielded off in any meaningful way, which makes
it a very messy intermingling of data and methods. Luckily though, none of
this has been documented. This folder is an attempt to create as much clarity
as is possible, makeing it an aesthetic undertaking in the first place.

---++ Modules

The module [eng_api.pl] is an attempt to extract as much information from the
GARP simualtion results as possible, and translate this into a more meaningful
format.

---+ Simulation result elements

The simulation results consist of various elements. This section provides an
overview of each of those.

---++ States

States are qualitatively distinct situations that the simulated system can be
in. States either correspond with a time point or with a temporal interval of
indefinite length. It is part of the QR approach that the temporal extension
of interval states is unknown. States consist of information regading the
structure of the system as that point or interval in time. This structure
consists of the agents and entities, including their configuration relations
and attributes. Agents and entities may have quantities associated with them,
that are also part of the structure of the state. Then there is behavioral
information (in addition to structure), including causal relations (direct and
indirect ones), correspondences, calculi, and explicit inequalities. Other
elements that are part of a state are identity relationships, imported model
fragments, quantity spaces (every quantity has one), and assumptions

A state is represented by GARP as a pair compound term of the following form:

==
state(ID, SMD)
==

The former argument is an integer uniquely identifying a state in the
simulation results.
The latter argument is a compound term; either a 6- or 7-tuple compound term:

==
smd(
    input_system(InputSystem),
    system_elements(SystemElements),
    parameters(Parameters),
    parameter_values(ParameterValues),
    parameter_relations(ParameterRelations),
    system_strucutres(SystemStructures)
).
==

Nobody knows what the term =|smd|= stands for.

See [smd_example.txt] for a sample state description.

---++ Input system / scenario

The input system is the name of the scenario that initiated the simulation.
In general, the scenario is the model fragment that states which strucutre to
use and what values to start from. The other model fragments are then used to
predict the rest of the behavior over time.

---++ System elements / instances and definitions

The system elements are a list of instantiations that are used in the
simulation. Each system element is a compound term of the following form:

==
instance(InstanceName, DefinitionName)
==

There are various types of elements that can be defined and instantiated:
* agents
* entities

Unknown instances:
* =|garp_scenario_scenario|= is an instance of =|garp_internal|=

---++ Parameters / entity-quantity-quantity space relations

The paramters are a list of compound terms that relate the following three
things:
1. entity
2. quantity
3. quantity space

The compound terms are quaternary though, being of the following form:

==
QuantityExternalName(EntityName, QuantityInternalName, continuous, QuantitySpace)
==

The functor and arguments are as follows:
* =|QuantityExternal|= is the external name of the quantity that the compound
term is about. This is the name that the user provided and that is displayed
in the UI, except for some minor alterations such as the removal of capital
letters.
It is unclear why something that is clearly data (the name of the quantity)
should be a functor.
* =|Entity|= The name of the entity that the quantity belongs to.
  The entity name has the same formatting conventions as the external quantity
  name.
* =|QuantityExternal|= is the name of the quantity as used within the
  simulation engine.
  There can be any number of internal engine names for a single external
  quantity name.
  An internal quantity name is derived from the external quantity name that it
  is associated with by appending an integer.
* =|continuous|= Unknown argument, seems to be a constant.
* =|QuantitySpace|= The name of the quantity space of the quantity.
  The quantity space name has the same formatting conventions as the external
  quantity name.

---++ Parameter values / magnitudes and derivatives

Each quantity has two values associated with it: a discrete value and the
derivative of that discrete value. The parameter values are a list of
statements asserting those values for every quantity.
These statements have the following form:

==
value(Quantity, unk, Magnitude, Derivative)
==

The arguments are as follows:
* =|QuantityInternal|= The internal simulation name of the quantity.
* =|unk|= This is a constant which is brought in intentionally, but has no
  function. It was meant to be able to extend the representation with
  quantitative information in the future [verbal communication with BB].
  The audacity of treating the fundamental data structure of a crucial
  software component like this still baffles me.
* =|Magnitude|= The name of the magnitude value of the quantity.
  This is a label from the quantity's quantity space.
* =|Derivative|= The name of the derivative value of the quantity.
  This is a label from the derivative quantity space, i.e.
  =|[min, zero, plus]|=.
  All derivatives have the same quantity space.

---++ Parameter relations / behavioral relations

The parameter relations can be devided into three groups:
1. causal relations
2. inequalities
3. correspondences

Paramter relations have the following form:

==
Relation(FromArgument, ToArgument)
==

The arguments are as follows:
* =|Relation|= is the name of the relation. In the following subsections we
  will detail each relation.
* =|FromArgument|= The first argument in the relation.
  The type of this argument depends on the type of the relation (see below).
* =|ToArgument|= The second argument in the relation.
  The type of this argument depends on the type of the relation (see below).

---+++ Causal relations

| GARP name            | proper relation name               |
| inf_neg_by           | negative quantity influence        |
| inf_pos_by           | positive quantity influence        |
| prop_neg             | negative quantity proportionality  |
| prop_pos             | positive quantity proportionality  |

Not that the influence relations are inverted (the latter argument influences
the former).

---+++ Inequality relations

| GARP name            | proper relation name               |
| d_equal              | derivative equal to                |
| d_greater            | derivative greater than            |
| d_smaller            | derivative smaller than            |
| equal                | mangitude equal to                 |
| greater              | magnitude greater than             |
| greater_or_equal     | magnitude greater than or equal to |
| smaller              | magntiude smaller than             |
| smaller_or_equal     | magnitude smaller than or equal to |

---+++ Correspondences

| GARP name            | proper relation name               |
| dir_q_correspondence | directed quantity correspondence   |
| q_correspondence     | undirected quantity correspondence |

---++ System structures

TODO

---++ Nested SMD

TODO

The nested SMD has an extra =|store|= argument. TODO
