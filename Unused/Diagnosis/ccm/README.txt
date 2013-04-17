---+ Component Connection Model

The Component Connection Model (CCM) is the explicit representation of the simulation results on the basis of which the various post-simulation use cases operate. The post-simulation use cases are currently:

* Automatic Repair
* Causal Explanation
* Diagnosis

---++ Data types

The CCM uses the following domain-independent data types:

* *component*
  A functional entity. A component has ports. A port can be connected to any number of points.
* *expression*
  An informational entity.
* *point*
  An informational entity in time. A point can be connected to any number of components.
* *|point_cloud|*
  A collection of points that share the same unchangeable properties. Unchangeable properties are properties that would change the type of point when changed. Changeable properties may change without chaning the type of point. An example will illustrate this. _|X = 1|_ and _|X = 2|_ can be expressions of points that belong to the same point cloud. Buy _|X = 1|_ and _|Y = 1|_ cannot.
* *space*
  A set of points that are temporarily related.
* *state*
  A space that has temporal duration.
* *|state_transition|*
  The space between two states.

The CCM uses the following domain-dependent data types:

* *attribute*
  ...
* *entity*
  ...
* *quantity*
  ...
* *quantity space*
  ...
