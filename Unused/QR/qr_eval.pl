:- module(
  qr_eval,
  [
    evaluate_current_model/0,
    evaluate_current_model/1 % +Stream:stream
  ]
).

/** <module> QR Evaluate

Evaluate a QR model.

@author Wouter Beek
@version Jul 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(generic(deb_ext)).
:- use_module(generic(math_ext)).
:- use_module(generic(meta_ext)).
:- use_module(qr(qr_api)).



evaluate_current_model:-
  evaluate_current_model(user_output).

evaluate_current_model(Stream):-
  generate,
  ansi_format(Stream, [underline], 'Within-state complexity', []),
  count(
    quantity_influence(_FromQuantity1, _Relation, _ToQuantity1, _Expression1, _PointCloud1),
    NumberOfQuantityInfluences
  ),
  format(
    Stream,
    'The number of quantity influences is ~w.\n',
    [NumberOfQuantityInfluences]
  ),
  count(
    quantity_proportionality(_FromQuantity2, _Relation2, _ToQuantity2, _Expression2, _PointCloud2),
    NumberOfQuantityProportionalities
  ),
  format(
    Stream,
    'The number of quantity proportionalities is ~w.\n',
    [NumberOfQuantityProportionalities]
  ),
  count(
    direct_quantity_feedback(Quantity, _Expression3, _SubsumedExpressions3),
    NumberOfDirectQuantityFeedbacks
  ),
  format(
    Stream,
    'The number of direct quantity feedbacks is ~w.\n',
    [NumberOfDirectQuantityFeedbacks]
  ),
  count(
    indirect_quantity_feedback(Quantity, _Expression4, _SubsumedExpressions4),
    NumberOfIndirectQuantityFeedbacks
  ),
  format(
    Stream,
    'The number of indirect quantity feedbacks is ~w.\n',
    [NumberOfIndirectQuantityFeedbacks]
  ),
  count(
    quantity_causality(_FromQuantity5, _Relation5, _ToQuantity5, _Expression5, _PointCloud5),
    NumberOfQuantityCausalLinks
  ),
  format(
    Stream,
    'The number of direct quantity causal links is ~w.\n',
    [NumberOfQuantityCausalLinks]
  ),
  count(
    quantity_causality_path(_FromQuantity6, _Relation6, _ToQuantity6,  _Expression6, _PointCloud6),
    NumberOfQuantityCausalPaths
  ),
  format(
    Stream,
    'The number of quantity causal paths is ~w.\n',
    [NumberOfQuantityCausalPaths]
  ),
  count(
    singular_quantity_feedback(_Quantity7, _Expression7, _SubmissiveExpressions7),
    NumberOfSingularQuantityFeedbacks
  ),
  format(
    Stream,
    'The number of singular quantity feedbacks is ~w.\n',
    [NumberOfSingularQuantityFeedbacks]
  ),
  count(
    double_quantity_feedback(_Quantity8, _Expression8, _SubmissiveExpressions8),
    NumberOfDoubleQuantityFeedbacks
  ),
  format(
    Stream,
    'The number of double quantity feedbacks is ~w.\n',
    [NumberOfDoubleQuantityFeedbacks]
  ),
  count(
    branching_quantity_causality(_FromQuantity9, _Triples9),
    NumberOfBranchingQuantityCausalities_
  ),
  NumberOfBranchingQuantityCausalities is NumberOfBranchingQuantityCausalities_ / 2,
  format(
    Stream,
    'The number of branching quantity proportionalities is ~w.\n',
    [NumberOfBranchingQuantityCausalities]
  ),
  count(
    magnitude_quantity_space_correspondence(_FromQuantity10, _ToQuantity10),
    NumberOfMagnitudeQuantitySpaceCorrespondences
  ),
  count(
    unidirectional_magnitude_quantity_space_correspondence(_FromQuantity11, _ToQuantity11),
    NumberOfDirectedMagnitudeQuantitySpaceCorrespondences
  ),
  count(
    bidirectional_magnitude_quantity_space_correspondence(_FromQuantity12, _ToQuantity12),
    NumberOfUndirectedMagnitudeQuantitySpaceCorrespondences
  ),
  count(
    inverted_magnitude_quantity_space_correspondence(_FromQuantity13, _ToQuantity13),
    NumberOfInvertedMagnitudeQuantitySpaceCorrespondences
  ),
  count(
    uninverted_magnitude_quantity_space_correspondence(_FromQuantity14, _ToQuantity14),
    NumberOfUninvertedMagnitudeQuantitySpaceCorrespondences
  ),
  format(
    Stream,
    'The number of magnitude quantity space correspondences is ~w (~w directed and ~w undirected) (~w inverted and ~w uninverted).\n',
    [
      NumberOfMagnitudeQuantitySpaceCorrespondences,
      NumberOfDirectedMagnitudeQuantitySpaceCorrespondences,
      NumberOfUndirectedMagnitudeQuantitySpaceCorrespondences,
      NumberOfInvertedMagnitudeQuantitySpaceCorrespondences,
      NumberOfUninvertedMagnitudeQuantitySpaceCorrespondences
    ]
  ),
  count(
    magnitude_quantity_value_correspondence(_FromQuantity15, _ToQuantity15),
    NumberOfMagnitudeQuantityValueCorrespondences
  ),
  count(
    unidirectional_magnitude_quantity_value_correspondence(_FromQuantity16, _ToQuantity16),
    NumberOfDirectedMagnitudeQuantityValueCorrespondences
  ),
  count(
    bidirectional_magnitude_quantity_value_correspondence(_FromQuantity17, _ToQuantity17),
    NumberOfUndirectedMagnitudeQuantityValueCorrespondences
  ),
  format(
    Stream,
    'The number of magnitude quantity value correspondences is ~w (~w directed and ~w undirected).\n',
    [
      NumberOfMagnitudeQuantityValueCorrespondences,
      NumberOfDirectedMagnitudeQuantityValueCorrespondences,
      NumberOfUndirectedMagnitudeQuantityValueCorrespondences
    ]
  ),
  
  ansi_format(Stream, [underline], 'Trans-state complexity', []),
  count(state(_State), NumberOfStates),
  count(state_transition(_StateTransition), NumberOfStateTransitions),
  count(initial_state_transtition(_InitialStateTransition), NumberOfInitialStateTransitions),
  format(
    Stream,
    'There are ~w states and ~w state transitions and ~w initial state transtitions.\n',
    [NumberOfStates, NumberOfStateTransitions, NumberOfInitialStateTransitions]
  ),
  ansi_format(Stream, [underline], 'Complexity metric', []),
  Z is NumberOfStateTransitions + NumberOfInitialStateTransitions,
  (
    Z = 0
  ->
    OverallComplexity = 0
  ;
    Connectedness is Z / NumberOfStates,
    maplist(
      multiply,
      [0.2, 0.4, 0.05, 0.15, 0.2, 0.1, 0.1],
      [
        NumberOfDirectQuantityFeedbacks,
        NumberOfIndirectQuantityFeedbacks,
        NumberOfQuantityCausalLinks,
        NumberOfQuantityCausalPaths,
        NumberOfBranchingQuantityCausalities,
        NumberOfMagnitudeQuantitySpaceCorrespondences,
        NumberOfMagnitudeQuantityValueCorrespondences
      ],
      WithinstateComplexities
    ),
    sum_list(WithinstateComplexities, WithinstateComplexity),
    log(10, NumberOfStates, X),
    Y is Connectedness * X,
    OverallComplexity is Y + WithinstateComplexity
  ),
  format(Stream, 'The overall complexity is ~w.\n', [OverallComplexity]).

