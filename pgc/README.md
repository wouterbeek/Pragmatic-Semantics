# Prolog Generics Collection (PGC)

This is a collection of predicates that I use across projects.
This includes extensions to SWI-Prolog,
or to one of the standard Prolog libraries;
support for modeling languages and visualization techniques.

## Contents

This repository contains the following subcollections of Prolog modules:
  * Datasets
  * DCG
  * Generics
  * Graph theory
    * DGRAPH
    * RDF Graph
    * UGRAPH
  * Inductive Logic Programming (ILP)
  * Logic
    * RDF Model Theory
  * Mathematics
  * Operating System interaction
  * Server
  * Standards
    * DateTime
    * Geography
    * GraphViz
    * HTML
    * HTTP
    * Language
    * OWL
    * RDF
    * RDFS
    * SPARQL
    * SVG
    * Tests
    * URI
    * XML
      * XML Schema (XSD)
  * Truth-Maintenance System (TMS)
  * Vocabularies
    * SKOS

## Dependencies

The following SWI-Prolog Packs are needed in some of the PGC modules:
  * `real`
  * `regex`

## Modes

You can run this codebase in two modes.

### Developer

This loads SWI-Prolog's debug console and writes debug messages to it.

This keeps the SWI-Prolog console in view, event when SWI-Prolog is not
started from within a terminal emulator.

This loads the code documentation server (using swipl package plDoc).
The documentation front-end is accessed using the `documentation` command
in the Web console.

~~~{.sh}
[1]   swipl -s debug_standalone.pl
~~~

Note: `debug.pl` is used for loading PGC from within another project.

### User

This removes the SWI-Prolog console from sight, if possible. This does not
influence the visibility of the terminal emulater that swipl what started
from.

`run.pl`

## Personal directory `.<PROJECT-NAME>`

When PraSem is started for the first time, it creates a directory called
`.PraSem` in the user's home folder (e.g. `/home/wouter/.PraSem`).

This is where PraSem keeps all non-static data.

This personal PraSem directory contains the following subdirectories:

### `data`

Currently contains the following subdirectories:

1. *ILP* Contains training files used by the relational learner.
2. *Standards* Contains standards that are too big to be part of the PraSem
   codebase, e.g. `lexvo.rdf` for language and country URIs based on
   various ISO standards.
3. *STCN* The _redactiebladen_ that are used by the parser in
   =|DB/STCN/|=.
4. *Wordnet* The Prolog version of the _Wordnet_ lexical corpus.

### `debug`

### `log`

The logging system writes files to this subdirectory. Whenever a
PraSem session ends, a file is created. Since many log files will be created in this fashion, they are subdivided into a file hierarchy:
    
1. The mode in which PraSem was run (either `developer`or `user`).
2. The year (four decimal numbers).
3. The month (two decimal numbers).
4. The day (two decimal numbers).

## plDoc online documentation

If you want to make the dynamic tables visible in the plDoc documentation
server, you need to add the following clause to the `take_block` predicate
in `pldoc/doc_wiki.pl`. This code cannot be included in SWI-Prolog
releases because of the security implications of running arbitrary code on
a server.

~~~
take_block(
  [_-['!', '!', w(Module), '!', '!', w(Predicate), '!', '!', w(Arity0), '!', '!'] | Lines],
  _BaseIndent,
  table([class(wiki)], Rows),
  Lines
):-
  atom_number(Arity0, Arity),
  findall(
    tr(Cells),
    (
      length(Vars, Arity),
      Call =.. [Predicate | Vars],
      call(Module:Call),
      findall(
        td([w(Atom)]),
        (
          member(Var, Vars),
          (atomic(Var) -> Atom = Var ; term_to_atom(Var, Atom))
        ),
        Cells
      )
    ),
    Rows
  ).

## Reuse

The following Git commands can be used to include this repository in
other projects:

~~~{.bash}
git remote add generics https://github.com/wouterbeek/PrologGenerics.git
git fetch generics
git merge generics/master
~~~

