/* Prolog debug

Loads debug tools for Prolog programming.

@author Wouter Beek
@version 2012-2014/03
*/

% This library allows for exploiting the color and attribute facilities
% of most modern terminals using ANSI escape sequences.
% The Windows console (swipl-win) does not (yet) support ANSI (color)
% codes.
:- use_module(library(ansi_term)).

:- use_module(library(apply)).

:- use_module(library(debug)).

% This produces errors if the priority option is omitted,
% due to conflicting HTTP location declarations.
% SWI-Prolog asserts `pldoc=root(.)`.
:- use_module(library(http/http_path)).
:- multifile(http:location/3).
http:location(pldoc, root(help), [priority(10)]).
:- use_module(library(doc_http)).

% GUI tracer support.
:- use_module(library(gui_tracer)).

% We only load this in debug mode,
% since this may give information to hackers.
:- use_module(library(http/http_error)).

:- use_module(library(lists)).

% Forward error messages from consulting Prolog code to PceEmacs.
% Also, ed/1 and edit/1 call PceEmacs.
:- use_module(library(pce_emacs)).

% Run unit tests, unless compiled with optimisation turned on.
:- use_module(library(plunit)).

% Print code strings with their code table replacements.
:- use_module(library(portray_text)).

% Access the RDF library predicates from the top-level.
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(library(swi_ide)).

% plDev
:- use_remote_module(pl_web(pl_dev)).

% Load PGC documentation and debugging tools.
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(rdf_man(rdf_man_duplicates)).
:- use_remote_module(rdf_man(rdf_man_literals)).
:- use_remote_module(rdf_web(rdf_tabular)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(server(pldoc_web)).
:- use_remote_module(server(web_console)).
:- use_remote_module(server(web_message)).
:- use_remote_module(tms(tms_web)).

:- initialization(pl_debug).
pl_debug:-
  % Start the SWI-Prolog thread monitor.
  %%%%prolog_ide(thread_monitor),

  % Before doing much else, we start the documentation server that
  % generates Web sites based on the plDoc commenting in the swipl code files.
  %%%doc_server(4000),

  % Write lists of ASCII numbers as strings to the terminal.
  portray_text(true),
  set_portray_text(ellipsis, 1000),
  set_portray_text(min_length, 2),

  % Enforce more stringent style checking.
  style_check(+atom),
  style_check(+charset),
  style_check(+discontiguous),
  style_check(+no_effect),
  style_check(+singleton),
  %%%%style_check(+var_branches),

  % Prolog unit tests.
  set_test_options([load(normal),run(manual)]),
  %%%%set_test_options([load(normal),run(make(all))]),

  % Debug monitor.
  %%%%prolog_ide(debug_monitor),
  debug(high),

  % Full lDoc.
  %%%%load_modules_for_pldoc,
  true.


%! load_modules_for_pldoc is det.
% Loads all modules in PGC for debugging purposes:
%   1. Early catching of errors.
%   2. Fully browsable plDoc.

load_modules_for_pldoc:-
  forall(
    member(
      DirectoryName,
      [
        ap,
        datasets,
          dbpedia,
        dcg,
          flp,
          plp,
          nlp,
        generics,
        graph_theory,
          dgraph,
          rdf_graph,
          ugraph,
        %ilp, % Many undefined predicates...
        lod,
          ckan,
          owl,
          rdf,
            rdf_conv,
            rdf_man,
            rdf_mt,
            rdf_reasoning,
            rdf_term,
            rdf_web,
          rdfs,
          skos,
          sparql,
          void,
        logic,
        math,
        os,
        programming,
          pl,
            pl_web,
        ps,
          tms,
            atms,
            doyle,
        server,
        standards,
          datetime,
          geo,
          gv,
          html,
          http,
            http_headers,
            http_parameters,
          lang,
          latex,
          svg,
          tests,
          uri,
          xml,
            xsd,
        stat,
        web,
          crawler
      ]
    ),
    (
      Spec =.. [DirectoryName, .],
      absolute_file_name(Spec, Directory, [file_type(directory)]),
      format(atom(Search), '~w/*.pl', [Directory]),
      expand_file_name(Search, Files),
      maplist(use_module, Files)
    )
  ).

