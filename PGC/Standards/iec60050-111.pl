:- module('iec60050-111', []).

/** <module> IEC 60050-111

# Time and related concepts

## Space-time

Conceptual model having properties of a four-dimensional mathematical space
and used to describe everything existing physically.

## Space

Three-dimensional mathematical space, which is a subspace of space-time and
which can locally be considered as a Euclidean space.

## Time (1)

One-dimensional mathematical space, which is a subspace of space-time and
which is locally orthogonal to space.

## Event

Something that happens in time.

In pure physics, an event is considered as a point in space-time.

## Instantaneous

Pertaining to an event that is considered as having no extension in time.

## Process

Sequence in time of interrelated events.

This definition presents a time-related concept of process.
A function-related definition is given in IEC 60050-351.

## Time axis

Mathematical representation of the succession in time of instantaneous events
along a unique axis

According to the special relativity theory, the time axis depends on
the choice of a spatial reference frame.

## Instant

Point on the time axis.

An instantaneous event occurs at a specific instant.

## Simultaneous

Pertaining to two or more events having the same initial instant and
the same final instant.

According to the special relativity theory, the concept of “simultaneous”
depends on the choice of a spatial reference frame.

## Time interval

Part of the time axis limited by two instants.

A time interval comprises all instants between the two limiting instants and,
unless otherwise stated, the limiting instants themselves.

A time interval can be specified by the dates marking the initial instant
and final instant or by one of these dates and the duration of the time
interval.

## Time scale

System of ordered marks which can be attributed to instants on the time axis,
one instant being chosen as the origin.

A time scale may be chosen as:
  * *Continuous*, e.g. international atomic time (TAI).
  * *|Continuous with discontinuities|*, e.g. coordinated universal time
    (UTC), due to leap seconds, standard time due to summer time and winter
    time.
  * *|Successive steps|*, e.g. usual calendars, where the time axis is split
    up into a succession of consecutive time intervals and the same mark is
    attributed to all instants of each time interval.
  * *Discrete*, e.g. in digital techniques.

For physical and technical applications, a time scale with quantitative
marks is preferred, based on a chosen initial instant together with a unit
of measurement.

Customary time scales use various units of measurement in combination,
such as second, minute, hour, or various time intervals of the calendar
such as calendar day, calendar month, calendar year.

A time scale has a reference point which attributes one of the marks of the
time scale to one of the instants, thus determining the attribution of marks
to instants for the time scale.

## Date

Mark attributed to an instant by means of a specified time scale.

On a time scale consisting of successive steps, two distinct instants may
be expressed by the same date (see *|time scale|*).

With respect to the specified time scale, a date may also be considered
as the duration between the origin of the time scale and the considered
instant.

In common language, the term "date" is mainly used when the time scale
is a calendar.

## Duration / Time (2) (for continuous time scale)

Non-negative quantity attributed to a time interval, the value of which is
equal to the difference between the dates of the final instant and the initial
instant of the time interval, when the dates are quantitative marks.

Different time intervals may have the same duration, e.g. the period of a
time-dependent periodic quantity is a duration that is independent of the
choice of the initial instant.

In the case of discontinuities in the time scale, such as a leap second or the
change from winter time to summer time and back, the computation of the
duration requires the subtraction or addition of the change of duration of
the discontinuity.

The duration is one of the base quantities in the International System of
Quantities (ISQ) on which the International System of Units (SI) is based.
The term "time" instead of "duration" is often used in this context and also
for an infinitesimal duration.

For the term "duration", the word expressions as "time" or "time interval"
are often used, but the term "time" is not recommended in this sense and the
term "time interval" is deprecated in this sense to avoid confusion with the
concept of "time interval".

The SI unit of duration and time is the second.

In common language, the word "time" is used with several different meanings.
In technical language, however, more precise terms, e.g. date, duration,
time interval should be used.

## Accumulated duration / Total duration / Accumulated time

Sum of durations characterized by given conditions over a given time interval.

The time intervals related to the different durations may overlap each other
or not. Example for non-overlapping time intervals: accumulated down time.
Example for overlapping time intervals: maintenance man-hours.

## Calendar date

Date on a time scale consisting of the origin of a calendar and a succession
of calendar days.

In the standardized calendar, each calendar day extends from midnight to
midnight for the standard time at a given location. Consecutive calendar days
are usually grouped together in various time intervals, i.e. calendar weeks,
calendar months, calendar years.

In the standardized calendar, a calendar date is expressed by a triple of
numbers consisting of the number of the year relative to a conventional
origin, the number of the month within this year and the number of the day
within this month. The standardized representation (ISO 8601) is in the order
year-month-day.

## Standard time

Time scale derived from coordinated universal time, UTC, by a time shift
established in a given location by the competent authority.

This time shift may be varied in the course of a year.

Examples are Central European Time (CET), Central European Summer Time (CEST),
Pacific Standard Time (PST), Japanese Standard Time (JST), etc.

## Clock time

Quantitative expression marking an instant within a calendar day by the
duration elapsed after midnight in the local standard time.

Usually, clock time is represented by the number of hours elapsed after
midnight, the number of minutes elapsed after the last full hour, and, if
necessary, the number of seconds elapsed after the last full minute, possibly
with decimal parts of a second.

# Physics and chemistry

## (Physical) quantity / (Measurable) quantity

Attribute of a phenomenon, body or substance that may be distinguished
qualitatively and determined quantitatively.

The term quantity may refer to a quantity in a general sense (e.g., length,
time, mass, temperature, electrical resistance, amount-of-substance
concentration) or to a particular quantity (e.g., length of a given rod,
electrical resistance of a given specimen of wire, amount-of-substance
concentration of ethanol C_2H_50H in a given sample of wine).

Quantities that can be placed in order of magnitude relative to one another
are called quantities of the same kind.

Quantities of the same kind may be grouped together into categories of
quantities, for example:
  * work, heat, energy
  * thickness, circumference, wavelength

## Quantity equation

Equation expressing the relation among physical quantities.

%% Base quantity

One of the quantities which, in a set of quantities, are by convention
accepted as independent of one another.

## Derived quantity

Quantity which, in a set of quantities, is related to the base quantities by
a quantity equation.

## System of quantities

Set of base quantities together with aii derived quantities defined from the
base quantities in accordance with a given set of equations.

## Dimension of a quantity

Expression that represents a quantity of a system of quantities as the product
of powers of factors that represent the base quantities of the system.

Examples: In a system having base quantities length, mass and time, the
dimensions of which are denoted by *L*, *M* and *T* respectively, *LMT^2* is
the dimension of force; in the same system of quantities, *ML^{-3}* is the
dimension of mass concentration as well as of mass density.

The factors that represent the base quantities are called "dimensions" of
these base quantities.

## Quantity of dimension one / Dimensionless quantity

Derived quantity for the dimension of which all exponents of the dimensions
of the base quantities are zero.

## Unit (of measurement)

Particular quantiîy, defined and adopted by convention, with which other
quantities of the same kind are compared in order to express their magnitudes
relative to that quantity.

Units of measurement have conventionally assigned names and symbols.

Units of quantities of the same dimension may have the same names and symbols
even when the quantities are not of the same kind.

## Base unit

Unit of measurement of a base quantity in a given system of quantities.

## Derived unit

Unit of measurement of a derived quantity in a given system of quantities.

## Unit equation

Equation expressing the relation among units of measurement.

## System of units

Set of base units and derived units for a specified system of quantities.

## Coherent system of units

System of units in which all the derived units can be expressed as products
of powers of the base units with the proportionality factors one.

## International System of Units / SI (abbreviation)

The coherent system of units adopted and recommended by the General Conference
on Weights and Measures (CGPM).

The components of the International System of Units are listed in annex B (inform
ative).

## Metre (symbol: m)

SI unit of length, equal to the length of the path travelled by light in
vacuum during a time interval of 1/299.792.458 of a second.

## Kilogram (symbol kg)

SI unit of mass, equal to the mass of the object called the "intemational
prototype of the kilogram" kept at the Bureau International des Poids et
Mesures (BIPM).

## Second (symbol: s)

SI unit of time, equal to the duration of 9.192.631.770 periods of the
radiation corresponding to the transition between the two hyperfine levels of
the ground state of the caesium-133 atom.

## Ampère (symbol: A)

SI unit of electric current, equal to the constant current which,
if maintained in two straight parallel conductors of infinite length, of
negligible circular cross-section, and placed 1 metre apart in vacuum, would
produce between these conductors a lineic force equal to *|2*10^{-7}|*
newton per metre.

## Kelvin (symbol: K)

SI unit of thermodynamic temperature, equal to the fraction 1/273,16 of the
thermodynamic temperature of the triple point of water.

## Mole (symbol: mol)

SI unit of amount of substance, equal to the amount of substance of a system
which contains as many elementary entities as there are atoms in 0,012
kilogram of carbon 12; the elementary entities must be specified and may be
atoms, molecules, ions, electrons, other particles, or specified groups of
such particles.

## Candela (symbol cd)

SI unit of luminous intensity, equal to the luminous intensity, in a given
direction, of a source that emits monochromatic radiation of frequency
*|540*10^{12}|* hertz and that has a radiant intensity in that direction of
(1/683) watt per steradian.

## Value (of a quantity)

Magnitude of a particular quantity generally expressed as a unit of
measurement multiplied by a number.

The value of a quantity may be positive, negative or zero.

The value of a quantity may be expressed in more than one way.
Examples: length of a rod: 5,34 m or 534 cm; mass of a body: 0,152 kg or
152 g; amount of substance of a sample of water (*|H_2O|*): 0,012 mol or
12 mmol.

The values of quantities of dimension one are generally expressed as numbers.

A quantity that cannot be expressed as a unit of measurement multiplied by a
number may be expressed by reference to a conventional reference scale or to
a measurement procedure or to both.

## Numerical value (of a quantity)

The number which multiplies the unit of measurement in the value of a
quantity.

## Numerical value equation

Equation expressing the relation among numerical values of quantities
corresponding to the adopted units of measurement.

--

# Terms used in names and definitions for physical quantities

## Quotient

Result of a division.

In the field of physical quantities, the term quotient is used for defining
new quantities from quantities of the same kind or of different kinds.

The quotient *|a/b|* is expressed by the words: "the quotient of *a* by *b*".

## Ratio
Quotient of two quantities of the same kind.

A ratio is dimensionless and is expressed by a number.

The ratio *|c/d|* is expressed by the words: "the ratio of *c* to *d*".

## Coefficient

Result of division of two quantities of different kinds.

A coefficient is a quantity having a dimension.

## Factor

Number used as a multiplier.

A factor may represent the quotient of two quantities of the same kind and
defines then a quantity of dimension one.

## Massic / Specific

Qualifies the name of a quantity to indicate the quotient of that quantity
by the mass.

Examples: massic volume or specific volume, massic entropy or specific
entropy.

## Volumic / ... Density (1)

Qualifies the name of a quantity to indicate the quotient of that quantity by
the volume.

Examples: volumic mass or mass density, volumic electric charge or
electric charge density.

## Areic / Surface ... density

Qualifies the name of a quantity to indicate the quotient of that quantity
by the surface area.

Examples: areic mass or surface mass density, areic heat flow rate.

## Density of ... / ... Density (2)

Qualifies the name of a quantity expressing a flux or a current to indicate
the quotient of such a quantity by the area.

Examples: density of heat flow rate, electric current density.

In English, density also, and more commonly, means volumic mass.

## Lineic / Linear ... density

Qualifies the name of a quantity to indicate the quotient of that quantity by
the length.

Examples: iineic mass or linear mass density, lineic electric current or
linear electric current density.

The qualifier "linear" is also added to the name of a quantity solely to
distinguish between similar quantities (exampies: linear ionization, linear
expansion coefficient).

## Molar

Qualifies the name of a quantity to indicate the quotient of that quantity
by the amount of substance.

Example: molar volume.

## Concentration

Term added to the name of a quantity, especially for a substance in a mixture,
to indicate the quotient of that quantity by the total volume.

Examples: amount-of-substance concentration of B, molecular concentration of
B, ion concentration.

--

# Concepts of macroscopic physics

## Time scale

System of unambiguous ordering of events in time.

## Instant

Single point on a time scale.

## Date

Quantitative expression of an instant on a specified time scale.

By convention, the date may be expressed in years, months, days, hours,
minutes, seconds and fractions thereof.

## Time interval

Part of a time scale between, and described by, two given instants.

## Duration

Difference between the extreme dates of a time interval.

## Time constant (symbol: $\tau$)

### I

For a quantity growing or decaying exponentially towards a constant value the
duration of a time interval at the end of which the absolute value of the
difference between that constant value and the value of the quantity has
decreased to *|1/e|* of the absolute value of the difference at the
beginning of the interval, where *e* is the base of natural logarithms.

The time constant is the quantity $\tau$ appearing in the function
$F(t) = A + Be^{\frac{-t}\{tau}}$ describing a time-dependent quantity.

### II

Reciprocal of the damping coefficient of a damped oscillation.

The time constant is the quantity $\tau$ appearing in the expression
$F(t) = A + Be^{\frac{-t}{\tau}} f(t)$ of an exponentially damped oscillation,
where $f(t)$ is a periodic function.

## Speed of light in vacuum / Speed of electromagnetic waves in vacuum (symbol: c_0)

Physical constant the value of which has been fixed at exactly 299.792.458
m/s by the definition of the metre.

## Homogeneous

Qualifies a physical medium in which the relevant properties are independent
of the position in the medium.

## Inhomogeneous / Heterogeneous

Qualifies a physical medium in which the relevant properties depend on the
position in the medium.

## Isotropic

Qualifies a physical medium in which the relevant properties are independent
of direction.

## Anisotropic

Qualifies a physical medium in which the relevant properties depend upon
direction.

## Amount of substance (symbol: *n*)

Quantity proportional to the number of elementary entities of a specified
nature which are contained in a given sample of matter.

The elementary entities may be atoms, molecules, ions, electrons, other
particles, or groups of such particles.

The SI unit of amount of substance is the mole.

## Avogadro constant (symbol $N_A$)

Phvsical constant eaual to the number of elementary entities in a given
sample of matter, divided by the amount of substance of that sampíe; its
value is approximately equal to $6,0221 * 10^23 mol^{-1}$.

## Faraday constant (symbol: *F*)

Physical constant equal to the product of the elementary electric charge and
the Avogadro constant; its value is approximately equal to 96.485,3 C/mol.

## Inertia

That property of matter by virtue of which any material body continues in
its existing state of movement or rest in the absence of an external force.

Mass (symbol *m*)

Positive additive scalar quantity, characterizing a sample of matter in the
phenomena of inertia and gravitation.

## Rest mass (symbol: $m_0$)

Intrinsic mass of a body, excluding the increase of mass acquired by the body
due to its motion, according to the theory of relativity.

--

...

--

@author Wouter Beek
@see IEC 60050-111
@tbd Process the rest of the document.
@version 2013/07
*/
