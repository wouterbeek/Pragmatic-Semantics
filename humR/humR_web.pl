:- module(
  humR_web,
  [
    r_comp_web/3, % +Predicates:list(iri)
                  % ?NumberOfColumns:positive_integer
                  % -DOM:list
    r_load_web/2, % +VoID:atom
                  % -DOM:list
    r_temp_web/4, % +Predicate:iri
                  % +Object:or([bnode,iri,literal])
                  % ?NumberOfColumns:nonneg
                  % -DOM:list
    r_web/3 % +Predicate:iri
            % ?NumberOfColumns:positive_integer
            % -DOM:list
  ]
).

/** <module> humR Web front-end

Web-based front-end for humanities-R (humR).

@author Wouter Beek
@version 2013/10, 2013/12-2014/01, 2014/03
*/

:- use_remote_module(dcg(dcg_collection)).
:- use_remote_module(generics(meta_ext)).
:- use_remote_module(humR(humR_generic)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(real)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_remote_module(rdf(rdf_name)).
:- use_remote_module(rdf(rdf_namespace)).
:- use_remote_module(rdf_term(rdf_datatype)).
:- use_remote_module(server(app_ui)).
:- use_remote_module(server(web_modules)).
:- use_remote_module(svg(svg_file)).
:- use_remote_module(void(void_file)).

% @tbd Remove the dependency on STCN.
:- use_remote_module(xml(xml_namespace)).
:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').

:- http_handler(root(humR), humR_web, []).
user:web_module(humR, humR_web).

:- rdf_meta(r_comp_web(t,?,-)).
:- rdf_meta(r_temp_web(r,o,?,-)).
:- rdf_meta(r_web(r,?,-)).



humR_web(_Request):-
  reply_html_page(app_style, [], [p('TODO')]).

%! r_comp_web(
%!   +Predicates:list(iri),
%!   +NumberOfColumns:positive_integer,
%!   -SVG:list
%! ) is det.

r_comp_web(Ps1, N, SVG):-
  length(Ps1, NumberOfCompares),
  NumberOfCompares >= 2,
  maplist(rdf_global_id, Ps1, Ps2),
  default(15, N),

  % For comparisons, we require the domains and ranges to be the same.
  Ps2 = [P1|Ps2_],
  % @tbd Use =|rdfs:domain|= and =|rdfs:range|=.
  once(rdf_has(SomeS1, P1, SomeO1)),
  resource_class(SomeS1, S_Class),
  resource_class(SomeO1, O_Class),
  forall(
    member(P_, Ps2_),
    (
      once(rdf_has(SomeS2, P_, SomeO2)),
      resource_class(SomeS2, S_Class),
      resource_class(SomeO2, O_Class)
    )
  ),

  % Axis labels.
  axis_label(SomeO1, X_Axis),
  axis_label(SomeS1, Y_Axis),

  % Collect the unique object terms shared by the given properties.
  aggregate_all(
    set(O),
    (
      rdf_has(_, P1, O),
      forall(
        member(P_, Ps2_),
        once(rdf_has(_, P, O))
      )
    ),
    Os1
  ),

  % Totally ordered domains need to be discretzed before being represented
  % using bars.
  (
    is_total_order(SomeO1)
  ->
    discretize(Os1, N, Os2)
  ;
    Os2 = Os1
  ),

  % Contruct the two collections of bars by counting the number of
  % subjects for the given discretized object value scale.
  findall(
    Bars_,
    (
      member(P, Ps2),
      construct_bars(P, Os2, Bars_)
    ),
    Bars
  ),

  % Labels for the discretized intervals on the X axis.
  maplist(interval_label, Os2, Os3),

  % Caption for the figure.
  phrase(set(rdf_term_name, Ps2), Codes),
  atom_codes(Main, Codes),

  % Do it!
  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  <- barplot(
    Bars,
    % Columns are not stacked on top of each other,
    % but are placed beside each other.
    beside='TRUE',
    % Scaling of the font size of x-axis labels.
    cex.names=0.8,
    % Colors help distinguish between the compared properties.
    col=rainbow(NumberOfCompares),
    % Labels perpendicular to axis.
    las=2,
    % Logarithmic scale.
    %%%%log=+y,
    % Caption.
    main=+Main,
    % Text labels for x-axis.
    names.arg=Os3,
    ylab=+Y_Axis
  ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+X_Axis, line=5),
  <- legend(
    +topleft,
    bg=+white,
    fill=rainbow(NumberOfCompares),
    legend=PsLabels
  ),
  <- dev.off(.),
  file_to_svg(File, SVG).

%! r_load_web(+VoID_Graph:atom, -DOM:list) is det.
% Loads the dataset described in the VoID file with the given name
% (located in the `Data` subdirectory).

r_load_web(
  G,
  [element(p,[],['The VoID file was loaded in RDF graph ',G,'.'])]
):-
  absolute_file_name(data('VoID'), F, [access(read),file_type(turtle)]),
  void_load(F, G).

r_temp_web(P1, O1, N, SVG):-
  rdf_global_id(P1, P2),
  rdf_global_id(O1, O2),
  default(20, N),

  % Axis labels.
  once(rdf_has(SomeS, P2, _)),
  axis_label(SomeS, Y_Axis),

  %aggregate_all(set(Y1_), rdf_year_interval(_, Y1_, _), [Y1|_]),
  %aggregate_all(set(Y2_), rdf_year_interval(_, _, Y2_), Y2s),
  %reverse(Y2s, [Y2|_]),
  Y1 = 1400,
  Y2 = 1899,
  Diff is floor((Y2 - Y1) / N),

  findall(
    All,
    (
      between(1, N, M),
      YY1 is Y1 + (M - 1) * Diff,
      YY2 is Y1 + M * Diff,
      aggregate_all(
        count,
        (
          rdf_year_interval(PPN, YYY1, YYY2),
          rdf_year_overlap(YY1, YYY1, YY2, YYY2)
        ),
        All
      )
    ),
    Alls
  ),
  
  % Bars.
  findall(
    ProbS,
    (
      between(1, N, M),
      YY1 is Y1 + (M - 1) * Diff,
      YY2 is Y1 + M * Diff,
      aggregate_all(
        count,
        (
          rdf(PPN, P2, O2),
          rdf_year_interval(PPN, YYY1, YYY2),
          rdf_year_overlap(YY1, YYY1, YY2, YYY2)
        ),
        NumberOfS
      ),
      nth1(M, Alls, All),
      ProbS is NumberOfS / All
    ),
    Bars
  ),

  % Labels on the X-axis.
  findall(
    XLabel,
    (
      between(1, N, M),
      YY1 is Y1 + (M - 1) * Diff,
      YY2 is Y1 + M * Diff,
      format(atom(XLabel), '~w-~w', [YY1,YY2])
    ),
    XLabels
  ),

  % Caption for the figure.
  rdfs_label(O2, Main),

  % Colors.
  length(XLabels, NumberOfBars),

  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  <- barplot(
    Bars,
    % Columns are not stacked on top of each other,
    % but are placed beside each other.
    beside='TRUE',
    % Scaling of the font size of x-axis labels.
    cex.names=0.8,
    % Colors help distinguish between the compared properties.
    col=rainbow(NumberOfBars),
    % Labels perpendicular to axis.
    las=2,
    % Logarithmic scale.
    %%%%log=+y,
    % Caption.
    main=+Main,
    % Text labels for x-axis.
    names.arg=XLabels,
    ylab=+Y_Axis
  ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+'Time', line=5),
  <- legend(
    +topleft,
    bg=+white,
    fill=rainbow(NumberOfBars),
    legend=XLabels
  ),
  <- dev.off(.),
  file_to_svg(File, SVG).

rdf_year_interval(S, Y, Y):-
  rdf_datatype(S, stcn:exact_publication_year, dateTime(Y,_,_,_,_,_,_),
      xsd:gYear, _).
rdf_year_interval(S, Y1, Y2):-
  rdf_datatype(S, stcn:earliest_publication_year, dateTime(Y1,_,_,_,_,_,_),
      xsd:gYear, _),
  rdf_datatype(S, stcn:latest_publication_year, dateTime(Y2,_,_,_,_,_,_),
      xsd:gYear, _).

rdf_year_overlap(X1, X2, Y1, Y2):-
  Average is (Y1 + X1) / 2,
  X2 =< Average,
  Average =< Y2.

%! r_web(+Predicate:iri, ?NumberOfColumns:positive_integer, -SVG:list) is det.
% This plots the subject-object pairs that occur in triples
% with the given predicate.
%
% The subjects are plotted on the Y axis.
% The objects are plotted on the X axis.

r_web(P11, N, SVG):-
  rdf_global_id(P11, P12),

  % Settings.
  default(15, N),

  % Axis labels.
  once(rdf_has(SomeS, P12, SomeO)),
  axis_label(SomeO, X_Axis),
  axis_label(SomeS, Y_Axis),

  % Collect the unique object terms.
  aggregate_all(set(O), rdf_has(_, P12, O), Os1),

  % Totally ordered domains need to be discretzed before being represented
  % using bars.
  (
    is_total_order(SomeO)
  ->
    discretize(Os1, N, Os2)
  ;
    top_dogs(Os1, P12, N, Os2)
  ),

  % Contruct the bars by counting the number of subjects for
  % the given discretized object value scale.
  construct_bars(P12, Os2, Bars),

  % Labels for the discretized intervals on the X axis.
  maplist(interval_label, Os2, Os3),

  % Caption for the figure.
  phrase(rdf_term_name(P12), Codes),
  atom_codes(Main, Codes),

  % Colors.
  length(Bars, NumberOfBars),

  % Do it!
  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  <- barplot(
    Bars,
    % Scaling of the font size of x-axis labels.
    cex.names=0.8,
    col=rainbow(NumberOfBars),
    % Number of stripes in columns.
    % Incompatible with logarithmic scale!
    %%%%density=10,
    % Labels perpendicular to axis.
    las=2,
    % Logarithmic scale.
    %log=+y,
    % Caption text.
    main=+Main,
    % Text labels for x-axis.
    names.arg=Os3,
    ylab=+Y_Axis
  ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+X_Axis, line=5),
  <- legend(
    +topleft,
    bg=+white,
    fill=rainbow(NumberOfBars),
    legend=Os3
  ),
  <- dev.off(.),
  file_to_svg(File, SVG).

:- meta_predicate(r_web(-,-,0,-)).

r_web(X, Y, G, L):-
  aggregate_all(
    set(X-Y),
    call(G),
    L
  ).
