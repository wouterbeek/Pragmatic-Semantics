---+ PraSem

This is the codebase for the NWO project PraSem: Pragmatic Semantics for
the Web of Data.

---++ Modes

You can run this codebase in two modes.

---+++ Developer

This loads SWI-Prolog's debug console and writes debug messages to it.

This keeps the SWI-Prolog console in view, event when SWI-Prolog is not
started from within a terminal emulator.

This loads the code documentation server (using swipl package plDoc).
The documentation front-end is accessed using the =documentation= command
in the Web console.

[[developer.pl]]

---+++ User

This removes the SWI-Prolog console from sight, if possible. This does not
influence the visibility of the terminal emulater that swipl what started
from.

[[run.pl]]

---++ Personal directory =.PraSem=

When PraSem is started for the first time, it creates a directory called
=|.PraSem|= in the user's home folder (e.g. =|/home/wouter/.PraSem|=).

This is where PraSem keeps all non-static data.

This personal PraSem directory contains the following subdirectories:
    1. =data=
    2. =debug=
    3. =log=

---+++ The =data= subdirectory

Currently contains the following subdirectories:
    1. *ILP* Contains training files used by the relational learner.
    2. *Standards* Contains standards that are too big to be part of the PraSem
       codebase, e.g. =lexvo.rdf= for language and country URIs based on
       various ISO standards.
    3. *STCN* The _redactiebladen_ that are used by the parser in
       =|DB/STCN/|=.
    4. *Wordnet* The Prolog version of the _Wordnet_ lexical corpus.

---+++ The =debug= subdirectory

Currently the contents of the =db= RDF graph are written in RDF/XML
serialization format at the end of each PraSem session.

---+++ The =log= subdirectory

The logging system writes files to this subdirectory. Whenever a
PraSem session ends, a file is created. Since many log files will be created
in this fashion, they are subdivided into a file hierarchy:
    1. The mode in which PraSem was run (either =developer= or =user=).
    2. The year (four decimal numbers).
    3. The month (two decimal numbers).
    4. The day (two decimal numbers).

---+ plDoc online documentation

If you want to make the dynamic tables visible in the plDoc documentation
server, you need to add the following clause to the =take_block= predicate
in [[pldoc/doc_wiki.pl]]. (This code cannot be included in SWI-Prolog
releases because of the security implications of running arbitrary code on
a server.

==
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
==
