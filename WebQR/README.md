# WebQR

Semantic Web versions of Qualitative Reasoning components.

These QR components have the following properties:
  * Behavior graph and input model export through RDF graph export methods (SVG output).
  * RDF(S) internal data model.

In the future we want to add the following properties as well:
  * The data model is updated to OWL (i.e., more explicit semantics).
  * Simulation works using native OWL DL reasoners (i.e., add semantic descriptions of reasoning rules).
  * Allow model building and behavior graph inspection by adding JavaScript to the SVG outputs. (This may require integration into the PraSem Web server.)
  * CHR implementation of simulator.
  * Diagnosis, fully integrated with CHR simulator.

## Supported engines

  * QSIM, partially implemented.

# Discussions

## Identifiers, names, concepts

The following would be the standard approach of identifying the ontological
primitives in QR. An ontological primitive (e.g., a quantity) has:
  * Exactly one **identifier**, consisting of an integer that is
    automatically assigned by an internal counter.
    The integer is appended to the path of the circle URI.
    Example: `localhost:5000/circle/17`.
    This is also used for dereferencing the circle and for sending HTTP
    requests from the client to the server.
  * Zero or more **descriptive label**s.
    The most recently assigned descriptive label is set as the `rdfs:label`
    of the identifier and is displayed in the User Interface.
    All other descriptive labels are asserted as `qsim:old_label` literals
    (possibly including the timestamp of its abolition).
    Example: `< localhost:5000/circle/17, rdfs:label, "boom" >`,
    `< localhost:5000/circle/17, qsim:old_label, 'Tree' >`.
    If the user types text in a circle that is not a URI,
    then we assume it is a descriptive label.
  * Zero or more **concept name**s that are existing URIs in the LOD.
    If the user types text in a circle that is a URI, this is assumed
    to be a concept name.
    An `owl:sameAs` relation with the identifier is asserted.

There are some open issues with this 'standard' approach:
  1. We have to decide what happens if the user changes the concept name
     (a URI) to a label (a non-URI). The old URI has to be removed
     'de-grounding' the ontological primitive somehow.
  2. We do not know the language of the labels and literals that are entered.
  3. The entered labels may be duplicates of or may conflict with existing
     labels and literals of the concept name (a URI) in the LOD cloud.
     This is also true for other information asserted about the ontological
     primitive: the LOD cloud holds more information than the average
     QR user is prepared to type.

The biggest problem with the 'standard' approach is that the user is
creating his/her own metadata locally, while *at the same time* a
whole lot of metadata resides in the LOD cloud.
This is a waste of effort.
Associating an ontological primitive with a concept name after the fact,
i.e., after having created it with a textual (non-URI) label,
means that the local metadata of the ontological primitive
now has to be synched with the remote metadata that is attached to the
concept name (a URI from the LOD cloud). This may give conflicts and
requires quite advanced data synchronization and curation tools.

This is the reason why WebQR does *not* assign non-URI identifiers or textual
labels to the ontological primitives in its QR model.
In other words, WebQR only allows **concept names** to be entered, but no
**descriptive names**.
This is very different from existing approaches / the 'standard' approach.

In WebQR one cannot name ontological primitives with a custom,
textual label.
Only URIs are accepted as legal input.
This may seen very strange and 'anti-user'. 
As a user, you have to do quite a lot of work in WebQR: entering URIs
all the time!
But the advantage is that now your QR model resides in the the LOD
context.
For the developers of WebQR, the advantage is that now all of the LOD
data is at their disposal to enrich the user's modeling activity.
If we let the user enter a short label (save some time),
then we have a very local QR model that is not connected to the LOD at all.

If we look at the matter from a slightly different angle, then it is
exactly the 'standard' approach that requires the user to do more work.
First adding a label and then later adding URIs to the QR model as well
(some people have called this 'grounding') is actually the wrong way of doing
things, since there are only two possibilities:
  1. You add metadata that is in line with the LOD metadata of the URI
     that you will add in the future. You have added this metadata
     **in vain**. The data was already there!
  2. You add metadata that is not in line with the LOD metadata of the URI
     that you will add in the future. Now there are **conflicts**.
     You now have to integrate this metadata. You need a data integration UI!
     Or you have to create a new URI for yourself (some people have called
     this an 'anchor term'). Now you need a generic (meta)data publishing UI!

These features (metadata integration and metadata publishing) should not
be part of WebQR at all. Projects that have invested hundreds on man-months
in these areas already exist. Instead, we stick to the following principle:

> *WebQR ought not to become a platform for metadata-creation*.

@author Sander Latour
@author Wouter Beek
@version alpha1

