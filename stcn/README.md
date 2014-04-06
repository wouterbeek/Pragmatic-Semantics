# STCN, Short Title Catalogue Netherlands

## Installation

    1. Git clone this codebase.
    2. Install the latest SWI-Prolog development release (tested on 6.3.10).
    3. Create a directory =|.STCN|= in your home folder and copy the
       redactiebladen text file (uncompressed) there. Give the file the name
       =|redactiebladen.txt|=.
    4. In the console run the command =|swipl -s stcn.pl|=. This brings up a
       Web browser tab on localhost port 5000. From the Web interface you can
       run the necessary commands (described in the next section).

### plDoc online documentation

If you want to make the dynamic tables visible in the plDoc documentation
server, you need to add the following clause to the =take_block= predicate
in [[pldoc/doc_wiki.pl]]. (This code cannot be included in SWI-Prolog
releases because of the security implications of running arbitrary code on
a server.

~~~{.pl}
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
~~~

## Commands run from the Web interface

    * =help=, gives an list of the avialable commands.
    * =stcn_parse=, starts the parsing of the STCN redactiebladen.
    * =stcn_statistics=, displays the statistics for the STCN parser.
      This command can be executed during parsing in order to show partial
      results.

