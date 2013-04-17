:- module(
  ilp_input,
  [
    export_background_knowledge/1, % -BackgroundKnowledgeFile:atom
    export_landmarks/1, % -Landmarks:list(compound)
    export_ilp_input/1, % -BaseName:atom
    export_negative_examples/2, % -NegativeExamplesFile:atom
                                % -IllegalStates:list(compound)
    export_negative_examples/3, % +PositiveExamples:list(compound)
                                % -NegativeExamplesFile:atom
                                % -IllegalStates:list(compound)
    export_negative_examples/4, % +PositiveExamples:list(compound)
                                % +Landmarks:list(compound)
                                % -NegativeExamplesFile:atom
                                % -IllegalStates:list(compound)
    export_positive_examples/2, % -PositiveExamplesFile:atom
                                % -PositiveExamples:list(compound)
    generate_magnitude_value_types/1 % -MagnitudeValueTypes:list(compound)
  ]
).

/** <module> ILP Input

This module provides the input for ILP techniques.

@author Wouter Beek
@version May 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(generic(atom_ext)).
:- use_module(generic(file_ext)).
:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(ilp(qsim_bratko)).
:- use_module(qr(qr_api)).



alternative_state([], _Landmarks, []).
alternative_state([Q:_M/_D | Qs], Landmarks, [Q:AltM/AltD | AltQs]):-
  member(landmarks(Q, Ms), Landmarks),
  member(AltM, Ms),
  member(AltD, [decreasing, steady, increasing]),
  alternative_state(Qs, Landmarks, AltQs).

create_base_name_(BaseName):-
  get(@app?currentModel, name, ModelName0),
  re(
    [out(atom), replace(space,underscore)],
    any,
    ModelName,
    ModelName0,
    []
  ),
  get(@app?activeSimulation?currentScenario, name, ScenarioName0),
  re(
    [out(atom), replace(space,underscore)],
    any,
    ScenarioName,
    ScenarioName0,
    []
  ),
  get_time(TimeStampFloat),
  TimeStamp is round(TimeStampFloat),
  format(atom(BaseName), '~w_~w_~w', [ModelName, ScenarioName, TimeStamp]).

%% create_export_file(
%%   +FileType:atom,
%%   -AbsoluteFile:atom,
%%   -Stream:stream
%% ) is det.
% Returns both the file and stream for exporting to.
%
% @param AbsoluteFile The atomic name of an absolute file.
% @param Stream A stream for writing.

create_export_file(FileType, AbsoluteFile, Stream):-
  create_base_name_(BaseName),
  create_export_file_(BaseName, FileType, AbsoluteFile, Stream).

create_export_file_(BaseName, FileType, AbsoluteFile, Stream):-
  absolute_file_name(
    data_ilp(BaseName),
    AbsoluteFile,
    [access(write), file_type(FileType)]
  ),
  open(AbsoluteFile, write, Stream).

export_background_knowledge(BackgroundKnowledgeFile):-
  create_export_file(background_knowledge, BackgroundKnowledgeFile, Stream),
  generate_background_knowledge(BackgroundKnowledge),
  forall(
    member(SubList, BackgroundKnowledge),
    (
      maplist(format(Stream, '~w.\n'), SubList),
      format(Stream, '\n', [])
    )
  ),
  flush_output(Stream),
  close(Stream).

export_ilp_input(BaseName):-
  generate,
  % The three files must share the same base name, since otherwise the
  % timestamp chould make them diverse.
  create_base_name_(BaseName),
  
  % Positive examples.
  create_export_file_(
    BaseName,
    pos,
    _PositiveExamplesFile,
    PositiveExamplesStream
  ),
  generate_positive_examples(PositiveExamples),
  maplist(format(PositiveExamplesStream, '~w.\n'), PositiveExamples),
  flush_output(PositiveExamplesStream),
  close(PositiveExamplesStream),
  
  % Negative examples.
  create_export_file_(
    BaseName,
    neg,
    _NegativeExamplesFile,
    NegativeExamplesStream
  ),
  generate_negative_examples(PositiveExamples, NegativeExamples),
  maplist(format(NegativeExamplesStream, '~w.\n'), NegativeExamples),
  flush_output(NegativeExamplesStream),
  close(NegativeExamplesStream),
  
  % Background knowledge.
  create_export_file_(
    BaseName,
    background_knowledge,
    _BackgroundKnowledgeFile,
    BackgroundKnowledgeStream
  ),
  generate_background_knowledge(BackgroundKnowledge),
  forall(
    member(SubList, BackgroundKnowledge),
    (
      maplist(format(BackgroundKnowledgeStream, '~w.\n'), SubList),
      format(BackgroundKnowledgeStream, '\n', [])
    )
  ),
  flush_output(BackgroundKnowledgeStream),
  close(BackgroundKnowledgeStream).

%% export_landmarks(-Landmarks:list(compound)) is det.
% Returns the landmarks in the current simulation results.
%
% @param Landmarks A list of landmarks(Quantity, QuantityValues).

export_landmarks(Landmarks):-
  setoff(
    landmarks(QuantityDefinitionName, MagnitudeQuantityValueDefinitionNames),
    generate_landmark(
      QuantityDefinitionName,
      MagnitudeQuantityValueDefinitionNames
    ),
    Landmarks
  ).

export_negative_examples(
  NegativeExamplesFile,
  NegativeExamples
):-
  export_positive_examples(_PositiveExamplesFile, PositiveExamples),
  export_negative_examples(
    PositiveExamples,
    NegativeExamplesFile,
    NegativeExamples
  ).

export_negative_examples(
  PositiveExamples,
  NegativeExamplesFile,
  NegativeExamples
):-
  export_landmarks(Landmarks),
  export_negative_examples(
    PositiveExamples,
    Landmarks,
    NegativeExamplesFile,
    NegativeExamples
  ).

%% export_negative_examples(
%%   +PositiveExamples:list(compound),
%%   +Landmarks:list(compound),
%%   -NegativeExamplesFile:atom,
%%   -NegativeExamples:list(compound)
%% ) is det.

export_negative_examples(
  PositiveExamples,
  Landmarks,
  NegativeExamplesFile,
  NegativeExamples
):-
  create_export_file(neg, NegativeExamplesFile, Stream),
  generate_negative_examples(PositiveExamples, Landmarks, NegativeExamples),
  maplist(format(Stream, '~w.\n'), NegativeExamples),
  flush_output(Stream),
  close(Stream).

%% export_positive_examples(
%%   -PositiveExamplesFile:atom,
%%   -PositiveExamples:list(compound)
%% ) is det.
% Exports the current simulation results to the returned file.
% This is a positive examples (.f) file for aleph.
%
% @param PositiveExamplesFile The file to which the current simulation
%        has been saved.
% @param PositiveExamples A list of legalstate compound terms.

export_positive_examples(PositiveExamplesFile, PositiveExamples):-
  generate,
  create_export_file(pos, PositiveExamplesFile, Stream),
  generate_positive_examples(PositiveExamples),
  maplist(format(Stream, '~w.\n'), PositiveExamples),
  flush_output(Stream),
  close(Stream).

%% generate_mode_declarations(
%%   +Predicates:list(atom),
%%   +NumberOfQuantities:number,
%%   -ModeDeclarations:list(atom)
%% ) is det.
% Returns the mode declarations for the given predicates, all with the given
% number of quantity arguments.
%
% @param Predicates
% @param NumberOfQuantities
% @param ModeDeclarations

generate_mode_declarations(Predicates, NumberOfQuantities, ModeDeclarations):-
  repeating_list('+quantity', NumberOfQuantities, Quantities),
  findall(
    ModeDeclaration,
    (
      member(Predicate, Predicates),
      generate_mode_declaration_(Predicate, Quantities, ModeDeclaration)
    ),
    ModeDeclarations
  ).

%% generate_mode_declaration_(
%%   +Predicate:atom,
%%   +Quantities:list(atom),
%%   -ModeDeclaration:atom
%% ) is det.
% Returns the mode declaration for the given predicate and the given
% quantity arguments.
%
% @param Predicate
% @param ModeDeclaration

generate_mode_declaration_(Predicate, Quantities, ModeDeclaration):-
  (
    Predicate == legal_state,
    PredicateSpecification =.. [Predicate | [Quantities]],
    ModeDeclaration_ =.. [modeh, '*', PredicateSpecification],
    format(atom(ModeDeclaration), ':- ~w', [ModeDeclaration_]),
    !
  ;
    PredicateSpecification =.. [Predicate | Quantities],
    ModeDeclaration_ =.. [modeb, '*', PredicateSpecification],
    format(atom(ModeDeclaration), ':- ~w', [ModeDeclaration_])
  ).

generate_background_knowledge(
  [
    ModeDeclarations,
    TypeDeclarations,
    AlephSettings,
    Quantities,
    Relations,
    QuantityDerivative,
    QuantitySpaces,
    MagnitudeValueTypes,
    MagnitudeRelationTable,   
    InequalityStatements,
    BehaviorMethods
  ]
):-
  generate_quantities(Quantities),
  length(Quantities, NumberOfQuantities),
  generate_mode_declarations(
    [
      legal_state %,
      % propplus,
      % propmin,
      % influenceplus,
      % influencemin,
      % mult_influence,
      % qcorrespondenceplus,
      % qcorrespondencemin,
      % vcorrespondence
    ],
    NumberOfQuantities,
    ModeDeclarations_
  ),
  append(
    ModeDeclarations_, 
    [
      ':- modeb(*, propplus(+quantity, +quantity))',
      ':- modeb(*, propmin(+quantity, +quantity))',
      ':- modeb(*, influenceplus(+quantity, +quantity))',
      ':- modeb(*, influencemin(+quantity, +quantity))',
      ':- modeb(*, mult_influence(+quantity, +quantity, +quantity, #rel))',
      ':- modeb(*, mult_proportionality(+quantity, +quantity, +quantity, #rel))',
      ':- modeb(*, qcorrespondencemin(+quantity, +quantity))',
      ':- modeb(*, qcorrespondenceplus(+quantity, +quantity))',
      ':- modeb(*, vcorrespondence(+quantity, +quantity, #magrel))'
    ],
    ModeDeclarations
  ),
  TypeDeclarations =
    [
      ':- determination(legal_state/1,propplus/2)',
      ':- determination(legal_state/1,propmin/2)',
      ':- determination(legal_state/1,influenceplus/2)',
      ':- determination(legal_state/1,influencemin/2)',
      ':- determination(legal_state/1,mult_influence/4)',
      ':- determination(legal_state/1,mult_proportionality/4)',
      ':- determination(legal_state/1,qcorrespondenceplus/2)',
      ':- determination(legal_state/1,qcorrespondencemin/2)',
      ':- determination(legal_state/1,vcorrespondence/3)'
    ],
  AlephSettings =
    [
      ':- set(samplesize,4)',
      ':- set(resample,4)',
      ':- set(permute_bottom,true)',
      ':- set(nreduce_bottom,true)',
      ':- set(search,false)'
    ],
  Relations =
    [
      'rel(pospos)',
      'rel(posneg)',
      'rel(negpos)',
      'rel(negneg)'
    ],
  QuantityDerivative =
    [
      'qder(increasing, 1)',
      'qder(steady, 0)',
      'qder(decreasing, -1)'
    ],
  setoff(
    quantity_space(
      QuantityDefinitionName:mag/_,
      MagnitudeQuantityValueDefinitionNames
    ),
    generate_landmark(
      QuantityDefinitionName,
      MagnitudeQuantityValueDefinitionNames
    ),
    QuantitySpaces_
  ),
  QuantitySpaces =
    [
      'quantity_space(_:_/der, [decreasing, steady, increasing])'
      | QuantitySpaces_
    ],
  generate_magnitude_value_types(MagnitudeValueTypes),
  generate_magnitude_relation_table(
    MagnitudeValueTypes,
    MagnitudeRelationTable
  ),
  generate_inequality_statements(InequalityStatements),
  absolute_file_name(
    ilp(background_knowledge),
    AbsoluteFileName,
    [file_type(prolog)]
  ),
  open(AbsoluteFileName, read, InStream, [close_on_abort(true), type(text)]),
  read_terms(InStream, BehaviorMethods, []).

generate_derivative_inequality_statement(
  derivative_inequality(StateName, FromQuantityName, SignName, ToQuantityName)
):-
  inequality(
    State,
    FromQuantity,
    DerivativeInequalityExpressionDefinition,
    ToQuantity,
    DerivativeInequalityExpression
  ),
  derivative_inequality_expression_definition(
    DerivativeInequalityExpressionDefinition
  ),
  rdfs_subclass_of(DerivativeInequalityExpressionDefinition, expression:qq),
  inequality_expression_sign(DerivativeInequalityExpression, Sign),
  state_name(State, StateName),
  quantity_name(FromQuantity, FromQuantityName),
  rdf_global_id(expression:SignName, Sign),
  quantity_name(ToQuantity, ToQuantityName).

generate_inequality_statements(InequalityStatements):-
  findall(
    InequalityStatement,
    generate_inequality_statement(InequalityStatement),
    InequalityStatements
  ).

generate_inequality_statement(DerivativeInequalityStatement):-
  generate_derivative_inequality_statement(DerivativeInequalityStatement).
generate_inequality_statement(MagnitudeInequalityStatement):-
  generate_magnitude_inequality_statement(MagnitudeInequalityStatement).

generate_landmark(
  QuantityDefinitionName,
  MagnitudeQuantityValueDefinitionNames
):-
  quantity_definition(QuantityDefinition),
  quantity_definition_label(QuantityDefinition, QuantityDefinitionName),
  quantity_definition_magnitude_quantity_space_definition(
    QuantityDefinition,
    MagnitudeQuantitySpaceDefinition
  ),
  findall(
    MagnitudeQuantityValueDefinitionName,
    (
      quantity_space_definition_quantity_value_definition(
        MagnitudeQuantitySpaceDefinition,
        MagnitudeQuantityValueDefinition
      ),
      quantity_value_definition_label(
        MagnitudeQuantityValueDefinition,
        MagnitudeQuantityValueDefinitionName
      )
    ),
    MagnitudeQuantityValueDefinitionNames
  ).

generate_magnitude_inequality_statement(
  magnitude_inequality(StateName, FromQuantityName, SignName, ToQuantityName)
):-
  inequality(
    State,
    FromQuantity,
    MagnitudeInequalityExpressionDefinition,
    ToQuantity,
    MagnitudeInequalityExpression
  ),
  magnitude_inequality_expression_definition(
    MagnitudeInequalityExpressionDefinition
  ),
  rdfs_subclass_of(MagnitudeInequalityExpressionDefinition, expression:qq),
  inequality_expression_sign(MagnitudeInequalityExpression, Sign),
  state_name(State, StateName),
  quantity_name(FromQuantity, FromQuantityName),
  rdf_global_id(expression:SignName, Sign),
  quantity_name(ToQuantity, ToQuantityName).

generate_magnitude_value_types(MagnitudeValueTypes):-
  findall(
    MagnitudeValueType,
    generate_magnitude_value_type(MagnitudeValueType),
    MagnitudeValueTypes
  ).

generate_magnitude_value_type(
  magnitude_value_type(
    QuantityDefinitionName,
    MagnitudeQuantityValueDefinitionName,
    Type
  )
):-
  quantity_definition_magnitude_quantity_space_definition(
    QuantityDefinition,
    MagnitudeQuantitySpaceDefinition
  ),
  quantity_definition_label(QuantityDefinition, QuantityDefinitionName),
  quantity_space_definition_quantity_value_definition(
    MagnitudeQuantitySpaceDefinition,
    MagnitudeQuantityValueDefinition
  ),
  quantity_value_definition_label(
    MagnitudeQuantityValueDefinition,
    MagnitudeQuantityValueDefinitionName
  ),
  (
    point_quantity_value_definition(MagnitudeQuantityValueDefinition)
  ->
    Type = point
  ;
    interval_quantity_value_definition(MagnitudeQuantityValueDefinition)
  ->
    Type = interval
  ).

generate_magnitude_relation_table(List, MagrelTable):-
  findall(Interval, (member(X, List), X = magnitude_value_type(_,Interval,interval)), Intervals),
  remove_duplicates(Intervals, NewIntervals),
  findall(Point, (member(X, List), X = magnitude_value_type(_,Point,point)), Points),
  remove_duplicates(Points, NewPoints),
  get_table(NewPoints, PointTable),
  get_table(NewIntervals, IntervalTable),
  append(PointTable, IntervalTable, MagrelTable).

get_table([], []).  
get_table([H| List], [HR|Result]):-
  concat(H, H, CH),
  HR = magrel_table(H,H,CH),
  get_table(H, List, HalfResult),
  get_table(List, OtherHalf),
  append(HalfResult, OtherHalf, Result).

get_table(_, [], []):-!.

get_table(H, [HL | List], [HR | Result]):-
  concat(H, HL, CH),
  HR = magrel_table(H,HL,CH),
  get_table(H, List, Result).  

remove_duplicates(L,NL):-
  remove_duplicates(L, [], NL).

remove_duplicates([Item | Rest], Current, NewRest):-
  member(Item, Current),!,
  remove_duplicates(Rest, Current, NewRest).
remove_duplicates([Item | Rest], Current, NewRest):-
  append(Current, [Item], NewCurrent),
  remove_duplicates(Rest, NewCurrent, NewRest).
remove_duplicates([], Result, Result).  

generate_negative_examples(PositiveExamples, NegativeExamples):-
  export_landmarks(Landmarks),
  generate_negative_examples(PositiveExamples, Landmarks, NegativeExamples).

generate_negative_examples(PositiveExamples, Landmarks, NegativeExamples):-
  first(PositiveExamples, PositiveExample),
  PositiveExample =.. [legal_state | [Qs]],
  findall(
    NegativeExample,
    (
      alternative_state(Qs, Landmarks, AltQs),
      NegativeExample = legal_state(AltQs),
      \+(member(NegativeExample, PositiveExamples))
    ),
    NegativeExamples
  ).

%% generate_positive_examples(-PositiveExamples:list(compound)) is det.
% Returns the positive examples for the aleph algorithm.
% These are the legal states that are in the simulation results.
%
% @param PositiveExamples A list of legalstate compound terms.

generate_positive_examples(PositiveExamples):-
  findall(
    legal_state(Quantities),
    (
      state(State),
      setoff(
        QuantityName:MagnitudeQuantityValueName/DerivativeQuantityValueName,
        (
          quantity_value(
            State,
            Quantity,
            MagnitudeQuantityValue,
            _MagnitudeQuantityValueExpression,
            DerivativeQuantityValue,
            _DerivativeQuantityValueExpression
          ),
          quantity_name(Quantity, QuantityName),
          quantity_value_label(
            MagnitudeQuantityValue,
            MagnitudeQuantityValueName
          ),
          quantity_value_label(
            DerivativeQuantityValue,
            DerivativeQuantityValueName
          )
        ),
        Quantities
      )
    ),
    PositiveExamples
  ).

generate_quantities(Quantities):-
  setoff(
    quantity(QuantityName:_/_),
    quantity_name(_Quantity, QuantityName),
    Quantities
  ).
