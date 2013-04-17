:- module(
  diagnosis_dialog,
  [
% COMMUNICATE
    communicate_candidates_dui/1, % +Diagnosis:diagnosis
    communicate_culprits_dui/1, % +Diagnosis:diagnosis
    communicate_early_culprit_dui/2, % +Diagnosis:diagnosis
                                     % +EarlyCulprit:environment
    communicate_end_dui/1, % +Diagnosis:diagnosis
    communicate_expectation_point_dui/2, % +Diagnosis:diagnosis
                                         % +Expectation:point
    communicate_impossible_stage_dui/3, % +Diagnosis:diagnosis
                                        % +Point:point
                                        % +AlternativePoint:point
    communicate_no_discrepancy_dui/1, % +Diagnosis:diagnosis
    communicate_stage_dui/2, % +Diagnosis:diagnosis
                             % +Point:point
    locate_culprit_dui/1, % +SupportExpression:support_expression

% DIAGNOSIS DIALOG
    create_diagnosis_dialog/1, % +Diagnosis:diagnosis

% QUESTIONS
    ask_question_dui/1 % +Question:point_cloud
  ]
).

/** <module> Diagnosis User Interface (DUI).

This module contains the graphical user interface elements for the diagnosis.

@author Wouter Beek
@version 2011/08-2012/04
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_env)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_build)).
:- use_module(ccm(ccm_db)).
:- use_module(ccm(ccm_verb)).
:- reexport(diagnosis(diagnosis)).
:- use_module(ile(agent)).
:- use_module(library(semweb/rdf_db)).
:- use_module(generic(atom_ext)).
:- use_module(qr(qr_api)).
:- use_module(rdf(rdf_read)).



:- pce_begin_class(diagnoseDialog, assistanceDialog).

variable(diagnosis, object, both).

initialise(This, Diagnosis, Mode : mode = [name]):->
  send(This, slot, diagnosis, Diagnosis),
  send(@app, slot, diagnosis_dialog, This),

  % Mode.
  new(DefaultMode, name(expectations)),
  default(Mode, DefaultMode, SetMode),
  get(SetMode, value, SetMode_),
  add_mode(SetMode_),

  dialog_label(DialogLabel),
  get(@app, currentModel, Model),

  send_super(This, initialise,
    label := DialogLabel,
    model := Model,
    how_to := @on,
    what_is := @off,
    why := @off
  ),

  get(This, gapX, GapX),
  get(This, gapY, GapY),
  get(This, topY, TopY),

  new(StateMenu, extended_list_browser(
    name := space,
    label := 'Space:',
    message := message(This, select_space, @arg1),
    dict := new(dict),
    width := 20,
    height := 16
  )),
  send(This, display, StateMenu, point(
    x := GapX,
    y := TopY
  )),

  new(FromQuantityMenu, extended_list_browser(
    name := from_quantity,
    label := 'From Quantity',
    message := message(This, select_from_quantity, @arg1),
    dict := new(dict),
    width := 20,
    height := 8
  )),
  send(This, display, FromQuantityMenu, point(
    x := StateMenu?right_side + GapX,
    y := TopY
  )),

  new(FromMagnitudeMenu, extended_list_browser(
    name := from_magnitude,
    label := 'From magnitude:',
    message := message(This, select_from_magnitude, @arg1),
    dict := new(dict),
    width := 20,
    height := 8
  )),
  send(This, display, FromMagnitudeMenu, point(
    x := FromQuantityMenu?right_side + GapX,
    y := TopY
  )),

  new(FromDerivativeMenu, extended_list_browser(
    name := from_derivative,
    label := 'From derivative:',
    message := message(This, select_from_derivative, @arg1),
    dict := new(dict),
    width := 20,
    height := 8
  )),
  send(This, display, FromDerivativeMenu, point(
    x := FromMagnitudeMenu?right_side + GapX,
    y := TopY
  )),

  new(ToQuantityMenu, extended_list_browser(
    name := to_quantity,
    label := 'To Quantity:',
    message := message(This, select_to_quantity, @arg1),
    dict := new(dict),
    width := 20,
    height := 8
  )),
  send(This, display, ToQuantityMenu, point(
    x := StateMenu?right_side + GapX,
    y := FromQuantityMenu?bottom_side + GapY
  )),

  new(ToMagnitudeMenu, extended_list_browser(
    name := to_magnitude,
    label := 'To magnitude:',
    message := message(This, select_to_magnitude, @arg1),
    dict := new(dict),
    width := 20,
    height := 8
  )),
  send(This, display, ToMagnitudeMenu, point(
    x := ToQuantityMenu?right_side + GapX,
    y := FromMagnitudeMenu?bottom_side + GapY
  )),
  send(ToMagnitudeMenu, active, @off),

  new(ToDerivativeMenu, extended_list_browser(
    name := to_derivative,
    label := 'To derivative:',
    message := message(This, select_to_derivative, @arg1),
    dict := new(dict),
    width := 20,
    height := 8
  )),
  send(This, display, ToDerivativeMenu, point(
    x := ToMagnitudeMenu?right_side + GapX,
    y := FromDerivativeMenu?bottom_side + GapY
  )),
  send(ToDerivativeMenu, active, @off),

  new(RelationMenu, extended_list_browser(
    name := relation,
    label := 'Relation:',
    message := message(This, select_relation, @arg1),
    dict := new(dict),
    width := 20,
    height := 17
  )),
  send(This, display, RelationMenu, point(
    x := FromDerivativeMenu?right_side + GapX,
    y := TopY
  )),
  send(RelationMenu, active, @off),

  new(ProbeLabel, text_item(name := name(probe_label))),
  send(ProbeLabel, editable, @off),
  send(This, display, ProbeLabel, point(
    x := GapX,
    y := StateMenu?bottom_side + GapY + GapY + GapY
  )),
  send(ProbeLabel, active, @off),

  new(ExpressionLabel, text_item(name := name(expression_label))),
  send(ExpressionLabel, editable, @off),
  send(This, display, ExpressionLabel, point(
    x := GapX,
    y := ProbeLabel?bottom_side + GapY
  )),

  new(ObservationHistory, extended_list_browser(
    name := observationHistory,
    label := 'Observation history:',
    width := 59,
    height := 8
  )),
  send(This, display, ObservationHistory, point(
    x := GapX,
    y := ExpressionLabel?bottom_side + GapY
  )),

  new(CulpritHistory, extended_list_browser(
    name := culpritHistory,
    label := 'Culprit History:',
    width := 59,
    height := 8
  )),
  send(CulpritHistory, select_message, message(This, showCulprit, @arg1)),
  send(This, display, CulpritHistory, point(
    x := ObservationHistory?right_side + GapX,
    y := ExpressionLabel?bottom_side + GapY
  )),

  new(CancelButton, button(
    name := cancel,
    message := message(This, onClose),
    label := 'Cancel'
  )),
  send(This, display, CancelButton, point(
    x := GapX,
    y := ObservationHistory?bottom_side + GapY
  )),

  new(StageButton, button(
    name := stage,
    message := message(This, onStage),
    label := 'Stage'
  )),
  send(This, display, StageButton, point(
    x := CancelButton?right_side + GapX,
    y := ObservationHistory?bottom_side + GapY
  )),

  new(ContinueButton, button(
    name := continue,
    message := message(This, onContinue),
    label := 'Continue'
  )),
  send(ContinueButton, active, @off),
  send(This, display, ContinueButton, point(
    x := StageButton?right_side + GapX,
    y := ObservationHistory?bottom_side + GapY
  )),

  send(This, updateSpacers),
  send(This, assign_accelerators),
  send(This, minimalSize, size(
    width := RelationMenu?right_side + GapX,
    height := ContinueButton?bottom_side + GapY
  )).

onResize(This, Difference : difference = size):->
  get(This, gapX, GapX),
  get(This, gapY, GapY),

  send(This?observationHistory_member, right_side,
    This?observationHistory_member?right_side + (Difference?width / 2)
  ),
  send(This?culpritHistory_member, x,
    This?observationHistory_member?right_side + GapX
  ),
  send(This?culpritHistory_member, right_side, This?right_side - GapX),

  send(This?space_member, bottom_side, This?to_quantity_member?bottom_side),
  send(This?relation_member, bottom_side,
    This?to_quantity_member?bottom_side
  ),
  send(This?observationHistory_member, y,
    This?expression_label_member?bottom_side + GapY
  ),
  send(This?culpritHistory_member, y,
    This?expression_label_member?bottom_side + GapY
  ),
  send(This?cancel_member, position, point(
    x := This?cancel_member?x,
    y := This?cancel_member?y + Difference?height
  )),
  send(This?stage_member, position, point(
    x := This?stage_member?x,
    y := This?stage_member?y + Difference?height
  )),
  send(This?continue_member, position, point(
    x := This?continue_member?x,
    y := This?continue_member?y + Difference?height
  )),
  send(This?observationHistory_member, bottom_side,
    This?continue_member?top_side - GapY
  ),
  send(This?culpritHistory_member, bottom_side,
    This?continue_member?top_side - GapY
  ),
  send(This?probe_label_member, right_side, This?right_side - GapX),
  send(This?expression_label_member, right_side, This?right_side - GapX).

open(This):->
  % Open the dialog.
  send(This, open_centered),

  % Populate the dialog's contents.
  set_spaces(This),
  send(This, select_space),
  set_relations(This).

destroy(This):->
  % Goodnight!
  send_super(This, destroy).



% SELECT %

select_from_derivative(This, Selection : selection = [dict_item]):->
  default(
    Selection,
    ?(This?from_derivative_member?members, head),
    SetSelection
  ),
  send(This?from_derivative_member, selection, SetSelection),
  enable_to_quantity(This),
  send(This?relation_member, selection, @nil),
  get_space(This, Space),
  space_label(Space, SpaceLabel),
  get(This?from_quantity_member?selection, label, EntityQuantity),
  get(SetSelection, label, Derivative),
  format(
    atom(Message),
    'In ~w I expect that the derivative of ~w is ~w.',
    [SpaceLabel, EntityQuantity, Derivative]
  ),
  send(This?expression_label_member, selection, Message),
  enable_stage(This).

select_from_magnitude(This, Selection : selection = [dict_item]):->
  default(
    Selection,
    ?(This?from_magnitude_member?members, head),
    SetSelection
  ),
  send(This?from_magnitude_member, selection, SetSelection),
  enable_to_quantity(This),
  set_relations(This),
  get_space(This, Space),
  space_label(Space, SpaceLabel),
  get(This?from_quantity_member?selection, label, EntityQuantity),
  get(SetSelection, label, Magnitude),
  format(
    atom(Message),
    'In ~w I expect that ~w is ~w.',
    [SpaceLabel, EntityQuantity, Magnitude]
  ),
  send(This?expression_label_member, selection, Message),
  enable_stage(This).

% There are no from quantities to select.
select_from_quantity(This, _Selection):->
  send(This?from_quantity_member?members, empty).
% There are from quantities to select.
select_from_quantity(This, Selection):->
  get(This?from_quantity_member?members, head, DefaultSelection),
  default(Selection, DefaultSelection, SetSelection),
  send(This?from_quantity_member, selection, SetSelection),
  get(SetSelection, object, SelectedQuantity),
  set_from_magnitudes(This, SelectedQuantity),
  set_from_derivatives(This, SelectedQuantity),
  send(This?relation_member, selection, @nil),
  set_relations(This),

  % Expression label.
  get_space(This, Space),
  space_label(Space, SpaceLabel),!,
  get(SetSelection, label, EntityQuantity),
  format(
    atom(Message),
    'In ~w I expect that ~w is...',
    [SpaceLabel, EntityQuantity]
  ),
  send(This?expression_label_member, selection, Message),
  disable_stage(This).

select_relation(This, Selection : selection = [dict_item]):->
  default(
    Selection,
    ?(This?relation_member?members, head),
    SetSelection
  ),
  send(This?relation_member, selection, SetSelection),

  communicate_relation_expectation_label,
  (
    mode(expectations)
  ->
    disable_stage(This)
  ;
    true
  ).

select_space(This, Selection : selection = [dict_item]):->
  get(This?space_member?members, head, DefaultState),
  default(Selection, DefaultState, SetSelection),
  send(This?space_member, selection, SetSelection),

  get_space(This, Space),
  set_from_quantities(This, Space),
  set_to_quantities(This, Space),
  send(This, select_from_quantity),
  send(This?relation_member, selection, @nil),

  % Expression label.
  space_label(Space, SpaceLabel),
  format(atom(Message), 'In ~w I expect that ...', [SpaceLabel]),
  send(This?expression_label_member, selection, Message),

  disable_stage(This).

select_to_derivative(This, Selection : selection = [dict_item]):->
  default(
    Selection,
    ?(This?to_derivative_member?members, head),
    SetSelection
  ),
  send(This?to_derivative_member, selection, SetSelection),
  send(This?to_magnitude_member, selection, @nil),
  set_relations(This),
  enable_relations(This),
  communicate_relation_expectation_label,
  disable_stage(This).

select_to_magnitude(This, Selection : selection = [dict_item]):->
  default(
    Selection,
    ?(This?to_magnitude_member?members, head),
    SetSelection
  ),
  send(This?to_magnitude_member, selection, SetSelection),
  send(This?to_derivative_member, selection, @nil),
  set_relations(This),
  enable_relations(This),
  communicate_relation_expectation_label,
  disable_stage(This).

% There are no to quanities to select.
select_to_quantity(This, _Selection):->
  send(This?to_quantity_member?members, empty).
% There are to quantities to select.
select_to_quantity(This, Selection):->
  get(This?to_quantity_member?members, head, DefaultSelection),
  default(Selection, DefaultSelection, SetSelection),
  send(This?to_quantity_member, selection, SetSelection),
  get(SetSelection, object, SelectedQuantity),
  enable_to_magnitude(This),
  set_to_magnitudes(This, SelectedQuantity),
  enable_to_derivative(This),
  set_to_derivatives(This, SelectedQuantity),
  send(This?relation_member, selection, @nil),
  activate_quantity_relations(This),
  set_relations(This),
  enable_relations(This),
  communicate_relation_expectation_label,
  disable_stage(This).



% BUTTONS %

onClose(This):->
  send(This, destroy).

onContinue(This):->
  get(This, slot, diagnosis, Diagnosis),
  continue(This, Diagnosis).

onDestroy(This):->
  send(This, destroy).

onStage(This):->
  stage(This).



% LIST BROWSER DICTIONARY ITEMS

showCulprit(_This, DictItem : selection = dict_item):->
  get(DictItem, object, Culprit),
  locate_culprit(Culprit).

:- pce_end_class(diagnoseDialog).



% LIST BROWSERS & DICTIONARY ITEMS

find_item_by_object(ListBrowser, Object, DictItem):-
  get(ListBrowser?dict, find, message(@arg1?object, equal, Object), DictItem).

from_derivative_list_browser(Dialog, ListBrowser):-
  get(Dialog, member, from_derivative, ListBrowser).

from_magnitude_list_browser(Dialog, ListBrowser):-
  get(Dialog, member, from_magnitude, ListBrowser).

from_quantity_list_browser(Dialog, ListBrowser):-
  get(Dialog, member, from_quantity, ListBrowser).

%% locate_culprit(+Culprit:environment) is det.
% Shows the given culprit.
%
% @param Culprit An environment.

% The given culprit is the Q.E.D. indicating that diagnosis is done.
% Go into repair mode.
locate_culprit(EmptyEnvironment):-
  empty_environment(EmptyEnvironment),
  !,
  stage_to_store(Diagnosis),
  start_repair(Diagnosis).
% The given culprit is a real culprit, i.e. an environment.
locate_culprit(Culprit):-
  findall(
    Expression,
    (
      member_environment(ComponentCloud, Culprit),
      culprit_expression(ComponentCloud, Expression)
    ),
    Expressions
  ),
  maplist(locate_expression, Expressions).

culprit_expression(ComponentCloud, Expression):-
  component_cloud_subsumes_component_cloud(
    ComponentCloud,
    LowerComponentCloud
  ),
  !,
  culprit_expression(LowerComponentCloud, Expression).
culprit_expression(ComponentCloud, Expression):-
  component_cloud_support_point_cloud(ComponentCloud, SupportPointCloud),
  !,
  teacher(Teacher),
  % Only points that are believed by the teacher are included.
  % These are the points that correspond to something in the build model.
  % The rest is either added by the learner (as an expectation) or by
  % the GDE algorithm (derived from observations).
  point_cloud(SupportPoint, SupportPointCloud),
  believe_point(Teacher, SupportPoint),
  once(point(Expression, SupportPoint)).
% A scenario value component cloud has exactly one component, exactly one
% input point cloud, exactly one input point, and exactly one input
% expression.
culprit_expression(ComponentCloud, Expression):-
  scenario_value_component_cloud(ComponentCloud),
  !,
  component_cloud_input_point_cloud(ComponentCloud, InputPointCloud),
  expression_point_cloud(Expression, InputPointCloud).

space_list_browser(Dialog, ListBrowser):-
  get(Dialog, member, space, ListBrowser).

to_derivative_list_browser(Dialog, ListBrowser):-
  get(Dialog, member, to_derivative, ListBrowser).

to_magnitude_list_browser(Dialog, ListBrowser):-
  get(Dialog, member, to_magnitude, ListBrowser).

to_quantity_list_browser(Dialog, ListBrowser):-
  get(Dialog, member, to_quantity, ListBrowser).



% SET %

set_derivatives(ListBrowser, SelectedQuantity):-
  send(ListBrowser, clear),
  quantity_derivative_quantity_space(
    SelectedQuantity,
    DerivativeQuantitySpace
  ),
  quantity_space_to_quantity_values(DerivativeQuantitySpace, Values),
  reverse(Values, ReverseValues),
  forall(
    member(Value, ReverseValues),
    (
      quantity_value_label(Value, ValueLabel),
      new(DictionaryItem, dict_item(
        key := ValueLabel,
        label := ValueLabel,
        object := Value
      )),
      send(ListBrowser, insert, DictionaryItem)
    )
  ).

set_from_derivatives(Dialog, SelectedQuantity):-
  from_derivative_list_browser(Dialog, FromDerivativeListBrowser),
  set_derivatives(FromDerivativeListBrowser, SelectedQuantity).

set_from_magnitudes(Dialog, Quantity):-
  from_magnitude_list_browser(Dialog, FromMagnitudeListBrowser),
  set_magnitudes(FromMagnitudeListBrowser, Quantity).

set_from_quantities(Dialog, State):-
  from_quantity_list_browser(Dialog, FromQuantityListBrowser),
  set_quantities(FromQuantityListBrowser, State).

set_magnitudes(ListBrowser, Quantity):-
  send(ListBrowser, clear),
  quantity_magnitude_quantity_space(Quantity, QuantitySpace),
  quantity_space_to_quantity_values(QuantitySpace, Values),
  reverse(Values, ReverseValues),
  forall(
    member(Value, ReverseValues),
    (
      quantity_value_label(Value, ValueLabel),
      new(DictionaryItem, dict_item(
        key := ValueLabel,
        label := ValueLabel,
        object := Value
      )),
      send(ListBrowser, insert, DictionaryItem)
    )
  ).

set_probe_relations(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(ExistingExpression, Probe),
  (
    inequality_expression(ExistingExpression)
  ->
    % The first result is the top inequality expression definition.
    inequality_expression_definition(ExpressionDefinition)
  ;
    support_expression(ExistingExpression)
  ->
    % The first result is the top support expression definition.
    once(support_expression_definition(ExpressionDefinition))
  ;
    % The top expression definition.
    rdf_global_id(expression:expression, ExpressionDefinition)
  ),
  set_relations(Dialog, ExpressionDefinition).

set_quantities(ListBrowser, Space):-
  send(ListBrowser, clear),

  % 1. Display quantities that occur in the currently selected space
  %    first.
  space_to_quantities(Space, SpaceQuantities),
  forall(
    member(Quantity, SpaceQuantities),
    (
      quantity_label(Quantity, QuantityLabel),
      new(DictionaryItem, dict_item(
        key := QuantityLabel,
        label := QuantityLabel,
        object := Quantity
      )),
      send(ListBrowser?dict, insert, DictionaryItem)
    )
  ),
  % 2. Display quantities that do occur in the simulation resuls,
  %    but not in the currently selected space.
  quantities(AllQuantities),
  ord_intersection(
    SpaceQuantities,
    AllQuantities,
    SpaceQuantities,
    NonspaceQuantities
  ),
  forall(
    member(Quantity, NonspaceQuantities),
    (
      quantity_label(Quantity, QuantityLabel),
      new(DictionaryItem, dict_item(
        key := QuantityLabel,
        label := QuantityLabel,
        object := Quantity
      )),
      send(ListBrowser?dict, insert, DictionaryItem)
    )
  ).
/*
  % 3. Display quantities that do not belong to the simulation results,
  %    but that do occur in the build model.
  quantity_definitions(QuantityDefinitions),
  forall(
    member(QuantityDefinition, QuantityDefinitions),
    (
      quantity_definition_label(QuantityDefinition, QuantityDefinitionLabel),
      new(DictionaryItem, dict_item(
        key := QuantityDefinitionLabel,
        label := QuantityDefinitionLabel,
        object := QuantityDefinition
      )),
      send(ListBrowser?dict, insert, DictionaryItem)
    )
  ).
*/

set_relations(Dialog):-
  mode(expectations),
  !,
  inequality_expression_definition(InequalityExpressionDefinition),
  set_relations(Dialog, InequalityExpressionDefinition).
set_relations(Dialog):-
  mode(probes),
  !,
  set_probe_relations(Dialog).
set_relations(Dialog):-
  rdf_global_id(expression:expression, ExpressionDefinitionFilter),
  set_relations(Dialog, ExpressionDefinitionFilter).

set_relations(Dialog, ExpressionDefinitionFilter):-
  (
    mode(expectations)
  ;
    is_active_from_quantity(Dialog),
    is_active_to_quantity(Dialog)
  ),
  !,
  findall(
    ExpressionDefinition,
    (
      rdfs_subclass_of(ExpressionDefinition, ExpressionDefinitionFilter),
      rdfs_subclass_of(ExpressionDefinition, expression:qq),
      \+((
        rdfs_subclass_of(X, ExpressionDefinition),
        X \== ExpressionDefinition
      ))
    ),
    ExpressionDefinitions
  ),
  set_relations1(Dialog, ExpressionDefinitions).
set_relations(Dialog, ExpressionDefinitionFilter):-
  is_active_from_quantity(Dialog),
  is_active_to_magnitude(Dialog),
  !,
  findall(
    ExpressionDefinition,
    (
      rdfs_subclass_of(ExpressionDefinition, ExpressionDefinitionFilter),
      rdfs_subclass_of(ExpressionDefinition, expression:qp),
      rdfs_subclass_of(ExpressionDefinition, expression:magnitude),
      \+((
        rdfs_subclass_of(X, ExpressionDefinition),
        X \== ExpressionDefinition
      ))
    ),
    ExpressionDefinitions
  ),
  set_relations1(Dialog, ExpressionDefinitions).
set_relations(Dialog, ExpressionDefinitionFilter):-
  is_active_from_quantity(Dialog),
  is_active_to_derivative(Dialog),
  !,
  findall(
    ExpressionDefinition,
    (
      rdfs_subclass_of(ExpressionDefinition, ExpressionDefinitionFilter),
      rdfs_subclass_of(ExpressionDefinition, expression:qp),
      rdfs_subclass_of(ExpressionDefinition, expression:derivative),
      \+((
        rdfs_subclass_of(X, ExpressionDefinition),
        X \== ExpressionDefinition
      ))
    ),
    ExpressionDefinitions
  ),
  set_relations1(Dialog, ExpressionDefinitions).
set_relations(Dialog, ExpressionDefinitionFilter):-
  is_active_from_magnitude(Dialog),
  is_active_to_magnitude(Dialog),
  !,
  findall(
    ExpressionDefinition,
    (
      rdfs_subclass_of(ExpressionDefinition, ExpressionDefinitionFilter),
      rdfs_subclass_of(ExpressionDefinition, expression:vv),
      \+(rdfs_subclass_of(ExpressionDefinition, expression:derivative)),
      \+((
        rdfs_subclass_of(X, ExpressionDefinition),
        X \== ExpressionDefinition
      ))
    ),
    ExpressionDefinitions
  ),
  set_relations1(Dialog, ExpressionDefinitions).
set_relations(Dialog, ExpressionDefinitionFilter):-
  is_active_from_derivative(Dialog),
  is_active_to_derivative(Dialog),
  !,
  findall(
    ExpressionDefinition,
    (
      rdfs_subclass_of(ExpressionDefinition, ExpressionDefinitionFilter),
      rdfs_subclass_of(ExpressionDefinition, expression:vv),
      \+(rdfs_subclass_of(ExpressionDefinition, expression:magnitude)),
      \+((
        rdfs_subclass_of(X, ExpressionDefinition),
        X \== ExpressionDefinition
      ))
    ),
    ExpressionDefinitions
  ),
  set_relations1(Dialog, ExpressionDefinitions).
set_relations(Dialog, _ExpressionDefinitionFilter):-
  set_relations1(Dialog, []).

set_relations1(Dialog, ExpressionDefinitions):-
  send(Dialog?relation_member, clear),
  forall(
    member(ExpressionDefinition, ExpressionDefinitions),
    (
      can_instantiate_expression_definition(ExpressionDefinition),
      expression_definition_abbreviation(ExpressionDefinition, Label),
      new(DictionaryItem, dict_item(
        dict_item,
        key := Label,
        label := Label,
        object := ExpressionDefinition
      )),
      send(Dialog?relation_member?dict, insert, DictionaryItem)
    ;
      true
    )
  ),
  send(Dialog?relation_member, selection, @nil).

set_spaces(Dialog):-
  mode(expectations),
  !,
  states(States),
  set_spaces1(Dialog, States).

set_spaces1(Dialog, Spaces):-
  forall(
    member(Space, Spaces),
    (
      space_label(Space, SpaceLabel),
      new(DictionaryItem, dict_item(
        key := SpaceLabel,
        label := SpaceLabel,
        object := Space
      )),
      send(Dialog?space_member?dict, insert, DictionaryItem)
    )
  ).

set_to_derivatives(Dialog, SelectedQuantity):-
  to_derivative_list_browser(Dialog, ToDerivativeListBrowser),
  set_derivatives(ToDerivativeListBrowser, SelectedQuantity).

set_to_magnitudes(Dialog, Quantity):-
  to_magnitude_list_browser(Dialog, ToMagnitudeListBrowser),
  set_magnitudes(ToMagnitudeListBrowser, Quantity).

set_to_quantities(Dialog, State):-
  to_quantity_list_browser(Dialog, ToQuantityListBrowser),
  set_quantities(ToQuantityListBrowser, State).



% STAGE %

%% stage(Dialog:dialog) is det.
% Stages all expectations that are currently selected in the given dialog.
% Multiple expectations may be stages at once (e.g. magnitude and derivative
% values for the same quantity in the same space).
%
% @param Dialog A diagnosis dialog.

stage(This):-
  % With at least one staged user input it is possible to
  % continue to diagnosis.
  send(This?continue_member, active, @on),
  % Stage the user input.
  get_user_input_point(This, Point),
  get(This, slot, diagnosis, Diagnosis),
  stage(Diagnosis, Point),
  stage(This).
stage(This):-
  % Reset the UI, allowing a new user input to be staged.
  send(This, select_space).



% START DIAGNOSIS %

create_diagnosis_dialog(Diagnosis):-
  new(Dialog, diagnoseDialog(Diagnosis)),
  maplist(
    register_method,
    [
      communicate_candidates_method/communicate_candidates_dui,
      communicate_culprits_method/communicate_culprits_dui,
      communicate_early_culprit_method/communicate_early_culprit_dui,
      communicate_end_method/communicate_end_dui,
      communicate_impossible_stage_method/communicate_impossible_stage_dui,
      communicate_no_discrepancy_method/communicate_no_discrepancy_dui,
      communicate_stage_method/communicate_stage_dui
    ]
  ),
  add_dialog(Diagnosis, Dialog),
  send(Dialog, open).



% ACTIVATE / DEACTIVATE %

activate_from_derivative(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?from_derivative_member, colour, new(colour(green))),
  enable_from_derivative(Dialog).

activate_to_derivative(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?to_derivative_member, colour, new(colour(green))),
  enable_to_derivative(Dialog).

activate_from_magnitude(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?from_magnitude_member, colour, new(colour(green))),
  enable_from_magnitude(Dialog).

activate_to_magnitude(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?to_magnitude_member, colour, new(colour(green))),
  enable_to_magnitude(Dialog).

activate_inequalities(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?relation_member, label, 'Inequality relation:'),
  send(Dialog?relation_member, colour, new(colour(green))),
  enable_relations(Dialog).

activate_relations(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?relation_member, colour, new(colour(green))),
  enable_relations(Dialog).

activate_quantity_relations(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?relation_member, label, 'Quantity relation:'),
  send(Dialog?relation_member, colour, new(colour(green))),
  enable_relations(Dialog).

activate_value_relations(Dialog):-
  deactivate_all(Dialog),
  send(Dialog?relation_member, label, 'Value relation:'),
  send(Dialog?relation_member, colour, new(colour(green))),
  enable_relations(Dialog).

deactivate_all(Dialog):-
  send(Dialog?from_quantity_member, colour, new(colour(black))),
  send(Dialog?from_magnitude_member, colour, new(colour(black))),
  send(Dialog?from_derivative_member, colour, new(colour(black))),
  send(Dialog?to_quantity_member, colour, new(colour(black))),
  send(Dialog?to_magnitude_member, colour, new(colour(black))),
  send(Dialog?to_derivative_member, colour, new(colour(black))),
  send(Dialog?relation_member, colour, new(colour(black))).

% The from derivative is active for expectations.
is_active_from_derivative(_Dialog):-
  mode(expectations),
  !.
is_active_from_derivative(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Probe),
  expression_to_argument(Expression, ToArgument),
  derivative_quantity_value(ToArgument).

% The from magnitude is active for expectations.
is_active_from_magnitude(_Dialog):-
  mode(expectations),
  !.
is_active_from_magnitude(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Probe),
  expression_to_argument(Expression, ToArgument),
  magnitude_quantity_value(ToArgument).

is_active_from_value(Dialog):-
  is_active_from_magnitude(Dialog).
is_active_from_value(Dialog):-
  is_active_from_derivative(Dialog).

is_active_to_derivative(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Probe),
  expression_to_argument(Expression, ToArgument),
  derivative_quantity_value(ToArgument).

is_active_to_magnitude(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Probe),
  expression_to_argument(Expression, ToArgument),
  magnitude_quantity_value(ToArgument).

is_active_to_value(Dialog):-
  is_active_to_magnitude(Dialog).
is_active_to_value(Dialog):-
  is_active_to_derivative(Dialog).

is_active_from_quantity(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Probe),
  expression_from_argument(Expression, FromArgument),
  quantity(FromArgument).

is_active_to_quantity(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Probe),
  expression_to_argument(Expression, ToArgument),
  quantity(ToArgument).

is_active_relations(Dialog):-
  get(Dialog, slot, diagnosis, Diagnosis),
  diagnosis_current_probe(Diagnosis, Probe),
  point(Expression, Probe),
  \+((
    derivative_quantity_value_expression(Expression)
  ;
    magnitude_quantity_value_expression(Expression)
  )).



% BASIC HELP %

%% howto_label(-HowToLabel:atom) is det.
% Returns the "How To?" label.
%
% @param HowToLabel An atomic "How To?" label.

howto_label('How to express an expectation'):-
  mode(expectations).
howto_label('How to give a probe'):-
  mode(probes).



% COMMUNICATE %

%% communicate_relation_expectation_label(+Diagnosis:diagnosis) is det.
% Communicates a relational expectation to the current
% diagnosis dialog's expectation label.
%
% @param Diagnosis A diagnosis.

communicate_relation_expectation_label(Diagnosis):-
  diagnosis_dialog(Diagnosis, Dialog),

  % Space.
  get_space(Dialog, Space),
  space_label(Space, SpaceLabel),

  % From argument.
  get_from_argument(Dialog, FromArgument),
  rdfs_label_or_dots(FromArgument, FromArgumentLabel),

  % Relation.
  get_relation(Dialog, Relation),
  rdfs_label_or_dots(Relation, RelationLabel),

  % To argument.
  get_to_argument(Dialog, ToArgument),
  rdfs_label_or_dots(ToArgument, ToArgumentLabel),

  % Message.
  format(
    atom(Message),
    'In ~w I expect that ~w ~w ~w.',
    [SpaceLabel, FromArgumentLabel, RelationLabel, ToArgumentLabel]
  ),

  send(Dialog?expression_label_member, selection, Message).

%% communicate_candidates_dui(+Diagnosis:diagnosis) is det.
% Communictaes the given candidates in the current diagnosis dialog.
%
% @param Diagnosis A diagnosis.

communicate_candidates_dui(Diagnosis):-
  diagnosis_to_candidates(Diagnosis, Candidates),
  maplist(environment_to_dui_label, Candidates, CandidateDUILabels),
  atomic_list_concat(CandidateDUILabels, ',  ', CandidatesDUILabel),
  format(atom(Message), 'Candidates: ~w', [CandidatesDUILabel]),
  post_to_culprit_history(Diagnosis, Message, Candidates).

%% communicate_culprits_dui(+Diagnosis:diagnosis) is det.
% Communicates the final candidates or culprits of the given diagnosis
% in the diagnosis' dialog.
%
% @param Diangnosis A diagnosis.

communicate_culprits_dui(Diagnosis):-
  diagnosis_to_candidates(Diagnosis, Candidates),
  forall(
    member(Candidate, Candidates),
    (
      nth1(Index, Candidates, Candidate),
      communicate_culprit_dui(Diagnosis, Index, Candidate)
    )
  ).

communicate_early_culprit_dui(Diagnosis, EarlyCulprit):-
  communicate_culprit_dui(Diagnosis, 0, EarlyCulprit).

%% communicate_end_dui(+Diagnosis:diagnosis) is det.
% Communicates that the current use case ended.
%
% @param Diagnosis A diagnosis.

communicate_end_dui(Diagnosis):-
  diagnosis_atms(Diagnosis, ATMS),
  empty_environment(ATMS, Object),
  Message = 'Q.E.D.',
  post_to_culprit_history(Diagnosis, Message, Object),
  diagnosis_dialog(Diagnosis, Dialog),
  disable_dialog(Dialog).

%% communicate_culprit_dui(
%%   +Diagnosis:diagnosis,
%%   +Index:number,
%%   +Candidate:environment
%% ) is det.
% Communicate the given candidate in the diagnosis dialog.
%
% @param Diagnosis A diagnosis.
% @param Index A numeric index.
% @param Candidate An environment.

communicate_culprit_dui(Diagnosis, Index, Candidate):-
  forall(
    member_environment(ComponentCloud, Candidate),
    (
      % Generate the culprit message for the given component cloud.
      (
        component_cloud_support_point(ComponentCloud, SupportPoint)
      ->
        point_to_sentence(SupportPoint, Label)
      ;
        component_definition_component_cloud(ComponentDefinition, ComponentCloud),
        component_definition_label(ComponentDefinition, Label)
      ),
      component_cloud_id(ComponentCloud, ComponentCloudID),
      titlecase(Label, CapitalizedLabel),
      % Possible fault:
      format(
        atom(Message),
        'Culprit ~w: ~w (~w)',
        [Index, CapitalizedLabel, ComponentCloudID]
      ),
      environment(ATMS, Candidate),
      environment(ATMS, [ComponentCloud], Environment),
      post_to_culprit_history(Diagnosis, Message, Environment)
    )
  ).

%% communicate_expectation_point_dui(
%%   +Diagnosis:diagnosis,
%%   +ExpectationPoint:point
%% ) is det.
% Communictaes the given expectation point in the current diagnosis' dialog.
%
% @param Diagnosis A diagnosis.
% @param ExpectationPoint A point.

communicate_expectation_point_dui(Diagnosis, ExpectationPoint):-
  point(ExpectationExpression, Space, ExpectationPoint),
  space_label(Space, SpaceLabel),
  expression_to_sentence(ExpectationExpression, Sentence),
  format(atom(Message), ' ~w: ~w', [SpaceLabel, Sentence]),
  post_to_observation_history(Diagnosis, Message).

%% communicate_impossible_stage_dui(
%%   +Diagnosis:diagnosis,
%%   +Point:point,
%%   +AlternativePoint:point
%% ) is det.
% Communicates that it is impossible to stage the given point,
% since the given alternative point is already staged.
%
% @param Diagnosis A diagnosis.
% @param Point A point.
% @param AlternativePoint A point.

communicate_impossible_stage_dui(Diagnosis, Point, AlternativePoint):-
  point_to_sentence(Point, Sentence),
  point_to_sentence(AlternativePoint, AlternativeSentence),
  format(
    atom(Message),
    'Impossible to stage "~w" because "~w" is already staged.',
    [Sentence, AlternativeSentence]
  ),
  post_to_observation_history(Diagnosis, Message).

%% communicate_no_discrepancy_dui(+Diagnosis:diagnosis) is det.
% Communicates that there are no discrepancies in the
% current diagnosis dialog.
%
% @param Diagnosis A diagnosis.

communicate_no_discrepancy_dui(Diagnosis):-
  post_to_culprit_history(Diagnosis, 'No discrepancy, so no diagnosis.'),
  diagnosis_dialog(Diagnosis, Dialog),
  disable_dialog(Dialog).

communicate_stage_dui(Diagnosis, Point):-
  point_to_sentence_list(Point, SentenceList),
  once(space_point(Space, Point)),
  once(space_label(Space, SpaceLabel)),
  append(SentenceList, [in, SpaceLabel], SentenceList_),
  sentence_list_to_sentence(SentenceList_, Sentence),
  format(atom(Message), 'Staged: "~w"', [Sentence]),
  post_to_observation_history(Diagnosis, Message).

% This covers both diagnosis and repair results.
locate_culprit_dui(Expression):-
  locate_culprit(Expression, _FragmentIndividual, Sentence),
  send(@app, msgBox, text := Sentence, type := notification).

%% post_to_culprit_history(+Diagnosis:diagnosis, +Message:atom) is det.
% Posts the given message to the culprit history.
%
% @param Diagnosis A diagnosis.
% @param Message An atomic message.

post_to_culprit_history(Diagnosis, Message):-
  post_to_culprit_history(Diagnosis, Message, Message).

%% post_to_culprit_history(
%%   +Diagnosis:diagnosis,
%%   +Message:atom,
%%   +Object:object
%% ) is det.
% Posts the given message and object to the culprit history.
%
% @param Diagnosis A diagnosis.
% @param Message An atomic message.
% @param Object A PCE object.

post_to_culprit_history(Diagnosis, Message, Object):-
  diagnosis_dialog(Diagnosis, Dialog),
  get(Dialog, member, culpritHistory, CulpritHistory),
  post_to_list_browser(CulpritHistory, Message, Object).

%% post_to_list_browser(
%%   +ListBrowser:list_browser,
%%   +Message:atom,
%%   +Object:object
%% ) is det.
% Posts the given message and object to the given list browser.
%
% @param ListBrowser A PCE list browser.
% @param Message An atomic message.
% @param Object A PCE object.

post_to_list_browser(ListBrowser, Message, Object):-
  % Create the dictionary item for the list browser's dictionary.
  new(DictItem, dict_item(
    key := Message,
    label := Message,
    object := Object
  )),

  % Add the message to the culprit history, only if it was not added before.
  (
    get(ListBrowser?dict, find,
      message(@arg1?label, equal, Message),
      _DictItem
    )
  ->
    true
  ;
    send(ListBrowser?dict, insert, DictItem)
  ).

%% post_to_observation_history(+Diagnosis:diagnosis, +Message:atom) is det.
% Posts the given message to the observation history.
%
% @param Diagnosis A diagnosis.
% @param Message An atomic message.

post_to_observation_history(Diagnosis, Message):-
  post_to_observation_history(Diagnosis, Message, Message).

%% post_to_observation_history(
%%   +Diagnosis:diagnosis,
%%   +Message:atom,
%%   +Object:object
%% ) is det.
% Posts the given message to the observation history.
%
% @param Diagnosis A diagnosis.
% @param Message An atomic message.
% @param Object A PCE object.

post_to_observation_history(Diagnosis, Message, Object):-
  diagnosis_dialog(Diagnosis, Dialog),
  get(Dialog, member, observationHistory, ObservationHistory),
  post_to_list_browser(ObservationHistory, Message, Object).

%% rdfs_label_or_dots(?Resource:uri, ?Label:atom) is nondet.
% Pairs of resources and labels, taking into consideration that a resource
% may be absent; indicated by a PCE @nil object.
%
% @param Resource A URI resource or the PCE object @nil.
% @param Label An atomic natural language label.

rdfs_label_or_dots(@nil, '...'):-
  !.
rdfs_label_or_dots(Resource, Label):-
  rdfs_label(Resource, Label).



% ENABLE / DISABLE %

disable_dialog(Dialog):-
  disable_space(Dialog),
  disable_from_quantity(Dialog),
  disable_from_magnitude(Dialog),
  disable_from_derivative(Dialog),
  disable_to_quantity(Dialog),
  disable_to_magnitude(Dialog),
  disable_to_derivative(Dialog),
  disable_inequalities_or_support(Dialog),
  disable_continue(Dialog),
  disable_stage(Dialog).

disable_cancel(Dialog):-
  send(Dialog?cancel_member, active, @off).

disable_continue(Dialog):-
  send(Dialog?continue_member, active, @off).

disable_from_derivative(Dialog):-
  send(Dialog?from_derivative_member, active, @off).

disable_to_derivative(Dialog):-
  send(Dialog?to_derivative_member, active, @off).

disable_from_quantity(Dialog):-
  send(Dialog?from_quantity_member, active, @off).

disable_stage(Dialog):-
  send(Dialog?stage_member, active, @off).

disable_to_quantity(Dialog):-
  send(Dialog?to_quantity_member, active, @off).

disable_from_magnitude(Dialog):-
  send(Dialog?from_magnitude_member, active, @off).

disable_to_magnitude(Dialog):-
  send(Dialog?to_magnitude_member, active, @off).

disable_space(Dialog):-
  send(Dialog?space_member, active, @off).

disable_inequalities_or_support(Dialog):-
  send(Dialog?relation_member, active, @off).

enable_cancel(Dialog):-
  send(Dialog?cancel_member, active, @on).

enable_continue(Dialog):-
  send(Dialog?continue_member, active, @on).

enable_from_derivative(Dialog):-
  send(Dialog?from_derivative_member, active, @on).

enable_stage(Dialog):-
  mode(expectations),
  !,
  send(Dialog?stage_member, active, @on).
% Always succeed.
enable_stage(_Dialog).

enable_to_derivative(Dialog):-
  send(Dialog?to_derivative_member, active, @on).

enable_from_quantity(Dialog):-
  send(Dialog?from_quantity_member, active, @on).

enable_to_quantity(Dialog):-
  send(Dialog?to_quantity_member, active, @on).

enable_from_magnitude(Dialog):-
  send(Dialog?from_magnitude_member, active, @on).

enable_to_magnitude(Dialog):-
  send(Dialog?to_magnitude_member, active, @on).

enable_space(Dialog):-
  send(Dialog?space_member, active, @on).

enable_relations(Dialog):-
  send(Dialog?relation_member, active, @on).



% GET %

get_from_argument(This, FromArgument):-
  get_from_value(This, FromArgument),
  !.
get_from_argument(This, FromArgument):-
   get_from_quantity(This, FromArgument).

get_to_argument(This, ToArgument):-
  get_to_value(This, ToArgument),
  !.
get_to_argument(This, ToArgument):-
   get_to_quantity(This, ToArgument).

get_from_derivative(This, FromDerivativeValue):-
  get(This?from_derivative_member, selection, @nil),
  !,
  FromDerivativeValue = @nil.
get_from_derivative(This, FromDerivativeValue):-
  get(This?from_derivative_member?selection, object, FromDerivativeValue).

get_from_magnitude(This, FromMagnitudeValue):-
  get(This?from_magnitude_member, selection, @nil),
  !,
  FromMagnitudeValue = @nil.
get_from_magnitude(This, FromMagnitudeValue):-
  get(This?from_magnitude_member?selection, object, FromMagnitudeValue).

get_from_quantity(This, FromQuantity):-
  get(This?from_quantity_member, selection, @nil),
  !,
  FromQuantity = @nil.
get_from_quantity(This, FromQuantity):-
  get(This?from_quantity_member?selection, object, FromQuantity).

get_from_value(This, FromValue):-
  is_active_from_magnitude(This),
  !,
  get_from_magnitude(This, FromValue).
get_from_value(This, FromValue):-
  is_active_from_derivative(This),
  get_from_derivative(This, FromValue).

get_relation(This, Relation):-
  get(This?relation_member, selection, @nil),
  !,
  Relation = @nil.
get_relation(This, Relation):-
  get(This?relation_member?selection, object, Relation).

get_space(This, Space):-
  get(This?space_member, selection, @nil),
  !,
  Space = @nil.
get_space(This, Space):-
  get(This?space_member?selection, object, Space).

get_to_derivative(This, ToDerivativeValue):-
  get(This?to_derivative_member, selection, @nil),
  !,
  ToDerivativeValue = @nil.
get_to_derivative(This, ToDerivativeValue):-
  get(This?to_derivative_member?selection, object, ToDerivativeValue).

get_to_magnitude(This, ToMagnitudeValue):-
  get(This?to_magnitude_member, selection, @nil),
  !,
  ToMagnitudeValue = @nil.
get_to_magnitude(This, ToMagnitudeValue):-
  get(This?to_magnitude_member?selection, object, ToMagnitudeValue).

get_to_quantity(This, ToQuantity):-
  get(This?to_quantity_member, selection, @nil),
  !,
  ToQuantity = @nil.
get_to_quantity(This, ToQuantity):-
  get(This?to_quantity_member?selection, object, ToQuantity).

get_to_value(This, ToValue):-
  is_active_to_magnitude(This),
  !,
  get_to_magnitude(This, ToValue).
get_to_value(This, ToValue):-
  is_active_to_derivative(This),
  get_to_derivative(This, ToValue).

%% get_user_input_point(+Diagnosis:diagnosis, -Expectation:point) is det.
% Retrieves a single expectation point from the user.
%
% @param Diagnosis A diagnosis.
% @param Expectation A point.

get_user_input_point(Dialog, Expectation):-
  get_user_input1(Dialog, FromArgument/Relation/ToArgument),
  find_or_add_expression(FromArgument, Relation, ToArgument, Expression),
  get_space(Dialog, Space),
  (
    % The expectation already exists as a point in the given space. Done.
    point(Expression, Space, Expectation)
  ->
    true
  ;
    % If there is an alternative expression that has a point
    % in the same space, then there is a conflict and we go
    % into diagnosis mode.
    find_alternative_expression(
      Expression,
      _SubsententialExpression,
      AlternativeExpression
    ),
    point(AlternativeExpression, Space, _AlternativePoint)
  ->
    find_or_add_point(Space, Expression, Expectation),
    add_mode(diagnosis)
  ;
    % There is no alternative expression that has a point in the
    % given space, so a new and unrelated point is added. This also
    % means that we go into repair mode.
    add_point(Expression, Space, Expectation),
    add_mode(repair)
  ).

%% get_user_input_resource(
%%   +Dialog:diagnosis_dialog,
%%   -Resource:uri
%% ) is det.
% Returns a resource that was given as input by the user.
%
% @param Dialog A diagnosis dialog.
% @param Resource One of the following:
%        1. A magnitude quantity value.
%        2. A magnitude quantity value.
%        3. An inequality relation.
%        4. A support relation.

get_user_input_resource(Dialog, MagnitudeQuantityValue):-
  is_active_from_magnitude(Dialog),
  !,
  get_from_magnitude(Dialog, MagnitudeQuantityValue).
get_user_input_resource(Dialog, DerivativeQuantityValue):-
  is_active_from_derivative(Dialog),
  !,
  get_from_derivative(Dialog, DerivativeQuantityValue).
get_user_input_resource(Dialog, Relation):-
  is_active_relations(Dialog),
  !,
  get_relation(Dialog, Relation).

get_user_input1(Dialog, FromArgument/ExpressionDefinition/ToArgument):-
  is_active_from_magnitude(Dialog),
  \+(get(Dialog?from_magnitude_member, selection, @nil)),
  once(magnitude_quantity_value_expression_definition(ExpressionDefinition)),
  get_from_quantity(Dialog, FromArgument),
  get_from_magnitude(Dialog, ToArgument),
  send(Dialog?from_magnitude_member, selection, @nil).
get_user_input1(Dialog, FromArgument/ExpressionDefinition/ToArgument):-
  is_active_from_derivative(Dialog),
  \+(get(Dialog?from_derivative_member, selection, @nil)),
  once(derivative_quantity_value_expression_definition(ExpressionDefinition)),
  get_from_quantity(Dialog, FromArgument),
  get_from_derivative(Dialog, ToArgument),
  send(Dialog?from_derivative_member, selection, @nil).
get_user_input1(Dialog, FromArgument/ExpressionDefinition/ToArgument):-
  is_active_relations(Dialog),
  get_relation(Dialog, ExpressionDefinition),
  get_from_argument(Dialog, FromArgument),
  get_to_argument(Dialog, ToArgument).



% MODES %

%% dialog_label(-DiagnosisLabel:atom) is det.
% Returns the label of the diagnosis dialog.
%
% @param DiagnosisLabel An atomic diagnosis label.

dialog_label('Express expectations'):-
  mode(expectations).
dialog_label('Enter probes'):-
  mode(probes).



% ACTIONS %

%% continue(+Dialog:diagnosis_dialog, +Diagnosis:diagnosis) is det.
% This is run upon pressing the 'continue'-button.
%
% @param Dialog A diagnosis dialog.
% @param Diagnosis A diagnosis.

% Stay in diagnosis and probes mode.
continue(Dialog, Diagnosis):-
  mode(probes),
  !,
  get_user_input_resource(Dialog, Answer),
  continue_diagnosis(Diagnosis, Answer).
% Stay in repair mode.
continue(_Dialog, Diagnosis):-
  mode(repair),
  !,
  stage_to_store(Diagnosis),
  start_repair(Diagnosis).
% Start the repair.
continue(_Dialog, Diagnosis):-
  mode(expectations),
  mode(repair),
  !,
  remove_mode(expectations),
  start_repair(Diagnosis).
% Go from expectations into probes mode and start diagnosis.
continue(Dialog, Diagnosis):-
  mode(expectations),
  mode(diagnosis),
  !,
  remove_mode(expectations),
  add_mode(probes),
  stage_to_store(Diagnosis),
  disable_stage(Dialog),
  start_diagnosis(Diagnosis).

%% ask_question_dui(+Diagnosis:diagnosis) is det.
% Ask a question inside the current diagnosis dialog.
%
% @param Diagnosis A diagnosis.

ask_question_dui(Diagnosis):-
  % Retrieve the request.
  diagnosis_current_probe(Diagnosis, CurrentProbe),
  rdf(CurrentProbe, ca:has_request, Request, ccm),
  rdf_literal(Request, request:has_question, QuestionSentence, ccm),

  diagnosis_dialog(Diagnosis, Dialog),
  space_list_browser(Dialog, ListBrowser),
  % Preselect the question's space.
  space_point(Space, CurrentProbe),
  find_item_by_object(ListBrowser, Space, DictItem),
  send(Dialog, select_space, DictItem),

  % Communicate the question inside the probe label.
  send(Dialog?probe_label_member, selection, QuestionSentence),
  send(Dialog?probe_label_member, active, @on),

  % Highlight the list from which a selection should be made.
  highlight_list_browser(Dialog, CurrentProbe).

%% highlight_list_browser(
%%   +Dialog:diagnosis_dialog,
%%   +Probe:point
%% ) is det.
% Highlights the list browser from which a selection is to be made
% in order to answer the given probe.
%
% @param Dialog A diagnosis dialog.
% @param Probe A point.

highlight_list_browser(Dialog, Point):-
  point(Expression, Space, Point),
  select_space(Dialog, Space),
  highlight_list_browser_(Dialog, Expression).

highlight_list_browser_(Dialog, Expression):-
  magnitude_quantity_value_expression(Expression),
  !,
  activate_from_magnitude(Dialog),
  from_quantity_expression(FromQuantity, Expression),
  select_from_quantity(Dialog, FromQuantity).
highlight_list_browser_(Dialog, Expression):-
  derivative_quantity_value_expression(Expression),
  !,
  activate_from_derivative(Dialog),
  from_quantity_expression(FromQuantity, Expression),
  select_from_quantity(Dialog, FromQuantity).
highlight_list_browser_(Dialog, Expression):-
  inequality_expression(Expression),
  !,
  (
    from_quantity_expression(FromQuantity, Expression)
  ->
    select_from_quantity(Dialog, FromQuantity)
  ;
    true
  ),
  (
    to_quantity_expression(ToQuantity, Expression)
  ->
    select_to_quantity(Dialog, ToQuantity)
  ;
    true
  ),
  activate_inequalities(Dialog).
highlight_list_browser_(Dialog, _Expression):-
  (
    from_quantity_expression(FromQuantity, Expression)
  ->
    select_from_quantity(Dialog, FromQuantity)
  ;
    true
  ),
  (
    to_quantity_expression(ToQuantity, Expression)
  ->
    select_to_quantity(Dialog, ToQuantity)
  ;
    true
  ),
  activate_relations(Dialog).

select_from_quantity(Dialog, Quantity):-
  quantity_label(Quantity, QuantityLabel),
  get(Dialog?from_quantity_member, find, QuantityLabel, DictItem),
  send(Dialog, select_from_quantity, DictItem).

select_space(Dialog, Space):-
  space_label(Space, SpaceLabel),
  get(Dialog?space_member, find, SpaceLabel, DictItem),
  send(Dialog, select_space, DictItem).

select_to_quantity(Dialog, Quantity):-
  quantity_label(Quantity, QuantityLabel),
  get(Dialog?to_quantity_member, find, QuantityLabel, DictItem),
  send(Dialog, select_to_quantity, DictItem).
