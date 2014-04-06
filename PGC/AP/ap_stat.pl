:- module(
  ap_stat,
  [
    ap_stage_init/1, % +Potential:nonneg
    ap_stage_tick/0
  ]
).

/** <module> Auto-process statistics

Statistics for tracking the progress of automated processes.

@author Wouter Beek
@version 2013/10-2014/03
*/

:- use_module(ap(ap_dir)).
:- use_module(dcg(dcg_content)).
:- use_module(generics(atom_ext)).
:- use_module(generics(thread_ext)).
:- use_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(server(web_modules)).

http:location(ap, root(ap), []).
:- http_handler(ap(stat), ap_stat, []).

user:web_module('AP Stat', ap_stat).



ap_stat(_Request):-
  % The number of automated processes.
  % Used for calculating average values.
  aggregate_all(
    count,
    rdfs_individual_of(_, ap:'AP'),
    N
  ),
  
  % Collect the columns in the AP table, i.e. the AP stages.
  % Each has a distinguished index that we use for sorting.
  % The index is maintained in the statistics table
  %  we are about to generate.
  aggregate_all(
    set(I-Column),
    (
      rdfs_individual_of(ApStage, ap:'AP-Stage'),
      rdf_string(ApStage, ap:name, Column, ap),
      
      % It takes a while before we have the index...
      rdf(AP, P, ApStage, ap),
      rdfs_individual_of(AP, ap:'AP'),
      rdf_global_id(rdf:LocalName, P),
      atom_concat('_', Atom, LocalName),
      atom_number(Atom, I)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Columns),
  
  % Now we create the rows of the table.
  findall(
    [Column,Succeed,Fail],
    (
      % For each column or AP stage type...
      member(Column, Columns),
      % ... we count the number of successful AP stages in this column.
      aggregate_all(
        count,
        (
          rdf_string(ApStage, ap:name, Column, ap),
          rdf_string(ApStage, ap:status, succeed, ap)
        ),
        Succeed0
      ),
      Succeed is Succeed0 / N,
      
      % ... and we count the number of unsuccessful AP stages in this column.
      aggregate_all(
        count,
        (
          rdf_string(ApStage, ap:name, Column, ap),
          rdf_string(ApStage, ap:status, error, ap)
        ),
        Fail0
      ),
      Fail is Fail0 / N
    ),
    Rows
  ),
  
  % Generate the HTML table based on the collected rows.
  reply_html_page(
    app_style,
    title('Automated Processes - Statistics'),
    \html_table(
      [header_row(true)],
      html('AP statistics'),
      [['Process','Succeed','Fail']|Rows]
    )
  ).



:- dynamic(stage_alias/3).

%! ap_stage_eval is det.

ap_stage_eval(AP):-
  ap_stage_directories(AP, StageDirs),
  (
    StageDirs == []
  ->
    debug(ap, 'No stage to evaluate.', [])
  ;
    length(StageDirs, Length),
    forall(
      between(0, Length, Stage),
      (
        ap_stage_eval(Alias, Stage, A, P),
        progress_bar(A, P, Bar),
        debug(ap, '[Alias:~w,Stage:~w] ~w', [Alias,Stage,Bar])
      )
    )
  ).

%! ap_stage_eval(
%!   +Alias:atom,
%!   +Stage:nonneg,
%!   -Actual:nonneg,
%!   -Potential:nonneg
%! ) is det.

ap_stage_eval(Alias, Stage, A, P):-
  atomic_list_concat([Alias,Stage,a], '_', FlagA),
  flag(FlagA, A, A),
  atomic_list_concat([Alias,Stage,p], '_', FlagP),
  flag(FlagP, P, P).



% FLAG INITIALIZATION %

ap_stage_init(Potential):-
  ap_stage_alias(Alias, Stage), !,

  % Create the potential flag.
  atomic_list_concat([Alias,Stage,p], '_', FlagP),
  flag(FlagP, _, Potential),

  % Create the actual flag.
  atomic_list_concat([Alias,Stage,a], '_', FlagA),
  flag(FlagA, _, 0),

  % Create the statistics tracking thread.
  % @tbd Not possible to set this yet.
  %option(stat_lag(Interval), O1, 10),
  intermittent_thread(
    ap_stage_eval,
    ap_stage_done(Alias, Stage),
    10000,
    _Id,
    []
  ).
ap_stage_init(_Potential):-
  debug(ap, 'Unable to initialize stage.', []).



% FLAG UPDATES %

ap_stage_tick:-
  ap_stage_alias(Alias, Stage), !,
  atomic_list_concat([Alias,Stage,a], '_', Flag),
  flag(Flag, Actual, Actual + 1).
ap_stage_tick:-
  debug(ap, 'Unable to tick stage.', []).



% GENERICS %

%! ap_stage_alias(-Alias:atom, -Stage:atom) is det.

ap_stage_alias(Alias, Stage):-
  thread_self(ThisThread),
  stage_alias(ThisThread, Alias, Stage).

