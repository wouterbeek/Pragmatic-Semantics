:- module(
  qsim_vocabulary,
  [
    qsim_assert_vocabulary/1 % +Graph:atom
  ]
).

/** <module> QSIM vocabulary

Asserts the vocabulary for QSIM.

@author Wouter Beek
@version 2013/02-2013/03, 2013/07, 2013/09, 2014/01-2014/02
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(owl(owl_build)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf(rdf_reification)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_label_ext)).
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(qsim, 'http://www.wouterbeek.com/qsim#').
:- xml_register_namespace(webqr, 'http://www.wouterbeek.com/webqr#').



%! qsim_assert_vocabulary(+Graph:atom) is det.
% Loads the QSIM ontology in the given graph.

% The QSIM vocabulary has already been loaded in the given graph,
% so do nothing.
qsim_assert_vocabulary(G):-
  % Look for the presence of an arbitrary triple.
  % This will suffice in practice.
  rdfs_class(m(t,f,f), qsim:'Quantity', G), !.
% Assert the QSIM vocabulary in the given graph.
qsim_assert_vocabulary(G):-
  % Entity
  rdfs_assert_class(           qsim:'Entity',                   G),
  rdfs_assert_label(           qsim:'Entity', 'entity',         G),
  rdfs_assert_isDefinedBy(     qsim:'Entity',                   G),
  owl_assert_resource_identity(qsim:'Entity', dbpedia:'Entity', G),
  rdf_register_class_color(_AnyGraph0, qsim:'Entity', green),

  % Quantity
  rdfs_assert_class(           qsim:'Quantity',                     G),
  rdfs_assert_label(           qsim:'Quantity', 'quantity',         G),
  rdfs_assert_isDefinedBy(     qsim:'Quantity',                     G),
  owl_assert_resource_identity(qsim:'Quantity', dbpedia:'Quantity', G),
  rdf_register_class_color(_AnyGraph1, qsim:'Quantity', green),

  % Entity -> Quantity
  rdf_assert_property(    qsim:quantity,                  G),
  rdfs_assert_label(      qsim:quantity, 'has quantity',  G),
  rdfs_assert_domain(     qsim:quantity, qsim:'Entity',   G),
  rdfs_assert_range(      qsim:quantity, qsim:'Quantity', G),
  rdfs_assert_isDefinedBy(qsim:quantity,                  G),

  % Quantity Space
  rdfs_assert_class(      qsim:'QuantitySpace',                   G),
  rdfs_assert_label(      qsim:'QuantitySpace', 'quantity space', G),
  rdfs_assert_isDefinedBy(qsim:'QuantitySpace',                   G),
  rdf_register_class_color(_AnyGraph2, qsim:'QuantitySpace', purple),

  % Quantity -> Quantity Space
  rdf_assert_property(    qsim:quantity_space,                       G),
  rdfs_assert_label(      qsim:quantity_space, 'has quantity space', G),
  rdfs_assert_domain(     qsim:quantity_space, qsim:'Quantity',      G),
  rdfs_assert_range(      qsim:quantity_space, qsim:'QuantitySpace', G),
  rdfs_assert_isDefinedBy(qsim:quantity_space,                       G),

  % Quantity Value
  rdfs_assert_class(      qsim:'QualitativeValue',                          G),
  rdfs_assert_label(      qsim:'QualitativeValue', 'qualitative value',     G),
  rdfs_assert_isDefinedBy(qsim:'QualitativeValue',                          G),
  rdfs_assert_subclass(   qsim:'MagnitudeValue',   qsim:'QualitativeValue', G),
  rdfs_assert_label(      qsim:'MagnitudeValue',   'magnitude value',       G),
  rdfs_assert_isDefinedBy(qsim:'MagnitudeValue',                            G),
  rdfs_assert_subclass(   qsim:'Landmark',         qsim:'MagnitudeValue',   G),
  rdfs_assert_label(      qsim:'Landmark',         'landmark',              G),
  rdfs_assert_isDefinedBy(qsim:'Landmark',                                  G),
  rdfs_assert_subclass(   qsim:'NegativeLandmark', qsim:'Landmark',         G),
  rdfs_assert_label(      qsim:'NegativeLandmark', 'negative landmark',     G),
  rdfs_assert_isDefinedBy(qsim:'NegativeLandmark',                          G),
  rdfs_assert_subclass(   qsim:'ZeroLandmark',     qsim:'Landmark',         G),
  rdfs_assert_label(      qsim:'ZeroLandmark',     'zero landmark',         G),
  rdfs_assert_isDefinedBy(qsim:'ZeroLandmark',                              G),
  rdfs_assert_subclass(   qsim:'PositiveLandmark', qsim:'Landmark',         G),
  rdfs_assert_label(      qsim:'PositiveLandmark', 'positive landmark',     G),
  rdfs_assert_isDefinedBy(qsim:'PositiveLandmark',                          G),
  rdfs_assert_subclass(   qsim:'Interval',         qsim:'MagnitudeValue',   G),
  rdfs_assert_label(      qsim:'Interval',         'interval',              G),
  rdfs_assert_isDefinedBy(qsim:'Interval',                                  G),
  rdfs_assert_subclass(   qsim:'NegativeInterval', qsim:'Interval',         G),
  rdfs_assert_label(      qsim:'NegativeInterval', 'negative interval',     G),
  rdfs_assert_isDefinedBy(qsim:'NegativeInterval',                          G),
  rdfs_assert_subclass(   qsim:'PositiveInterval', qsim:'Interval',         G),
  rdfs_assert_label(      qsim:'PositiveInterval', 'positive interval',     G),
  rdfs_assert_isDefinedBy(qsim:'PositiveInterval',                          G),
  rdf_register_class_color(_AnyGraph3, qsim:'QualitativeValue', indigo),

  % Quantity Space -> Quantity Value
  rdf_assert_property(    qsim:landmarks,                       G),
  rdfs_assert_label(      qsim:landmarks, 'has landmarks',      G),
  rdfs_assert_domain(     qsim:landmarks, qsim:'QuantitySpace', G),
  rdfs_assert_range(      qsim:landmarks, qsim:'Landmark',      G),
  rdfs_assert_isDefinedBy(qsim:landmarks,                       G),

  % Q -> QV
  rdf_assert_property(    qsim:value,                               G),
  rdfs_assert_label(      qsim:value,      'has value',             G),
  rdfs_assert_domain(     qsim:value,      qsim:'Quantity',         G),
  rdfs_assert_range(      qsim:value,      qsim:'QualitativeValue', G),
  rdfs_assert_isDefinedBy(qsim:value,                               G),
  rdfs_assert_subproperty(qsim:derivative, qsim:value,              G),
  rdfs_assert_label(      qsim:derivative, 'has derivative',        G),
  rdfs_assert_isDefinedBy(qsim:derivative,                          G),
  rdfs_assert_subproperty(qsim:magnitude,  qsim:value,              G),
  rdfs_assert_label(      qsim:magnitude,  'has magnitude',         G),
  rdfs_assert_isDefinedBy(qsim:magnitude,                           G),

  % Derivative values.
  rdfs_assert_subclass(   qsim:'DerivativeValue', qsim:'QualitativeValue', G),
  rdfs_assert_label(      qsim:'DerivativeValue', 'derivative value',      G),
  rdfs_assert_isDefinedBy(qsim:'DerivativeValue',                          G),
  rdf_assert_individual(  qsim:inc,               qsim:'DerivativeValue',  G),
  rdfs_assert_label(      qsim:inc,               'increasing',            G),
  rdfs_assert_isDefinedBy(qsim:inc,                                        G),
  rdf_assert_individual(  qsim:std,               qsim:'DerivativeValue',  G),
  rdfs_assert_label(      qsim:std,               'steady',                G),
  rdfs_assert_isDefinedBy(qsim:std,                                        G),
  rdf_assert_individual(  qsim:dec,               qsim:'DerivativeValue',  G),
  rdfs_assert_label(      qsim:dec,               'decreasing',            G),
  rdfs_assert_isDefinedBy(qsim:dec,                                        G),
  rdf_register_class_color(_AnyClass4, qsim:'DerivativeValue', indigo),

  % L -> L
  rdf_assert_property(     qsim:next_landmark,                      G),
  rdfs_assert_label(       qsim:next_landmark, 'has next landmark', G),
  rdfs_assert_domain_range(qsim:next_landmark, qsim:'Landmark',     G),
  rdfs_assert_isDefinedBy( qsim:next_landmark,                      G),

  % QS -> QVs
  rdf_assert_property(    qsim:values,                       G),
  rdfs_assert_label(      qsim:values, 'has values',         G),
  rdfs_assert_domain(     qsim:values, qsim:'QuantitySpace', G),
  rdfs_assert_range(      qsim:values, rdf:'List',           G),
  rdfs_assert_isDefinedBy(qsim:values,                       G),

  % QV -> QV
  rdf_assert_property(     qsim:next_value,                          G),
  rdfs_assert_label(       qsim:next_value, 'has next value',        G),
  rdfs_assert_domain_range(qsim:next_value, qsim:'QualitativeValue', G),
  rdfs_assert_isDefinedBy( qsim:next_value,                          G),

  % S
  rdfs_assert_class(      qsim:'State',          G),
  rdfs_assert_label(      qsim:'State', 'state', G),
  rdfs_assert_isDefinedBy(qsim:'State',          G),
  rdf_register_class_color(_, qsim:'State', darkgreen),
  % Static state
  rdf_assert_individual(  qsim:static, qsim:'State',   G),
  rdfs_assert_label(      qsim:static, 'static state', G),
  rdfs_assert_isDefinedBy(qsim:static,                 G),
  % Cache state
  rdf_assert_individual(  qsim:cache, qsim:'State',   G),
  rdfs_assert_label(      qsim:cache, 'cache state',  G),
  rdfs_assert_isDefinedBy(qsim:cache,                 G),

  % Dynamic relations
  % Q <-- not_m -->
  rdf_assert_property(    qsim:relation,                                   G),
  rdfs_assert_subproperty(qsim:relation,            webqr:relation,        G),
  rdfs_assert_isDefinedBy(qsim:relation,                                   G),
  rdfs_assert_subproperty(qsim:dynamic_relation,    qsim:relation,         G),
  rdfs_assert_isDefinedBy(qsim:dynamic_relation,                           G),
  rdfs_assert_subproperty(qsim:magnitude_exclusion, qsim:dynamic_relation, G),
  rdfs_assert_label(      qsim:magnitude_exclusion, 'magnitude exclusion', G),
  rdfs_assert_domain(     qsim:magnitude_exclusion, qsim:'Quantity',       G),
  rdfs_assert_range(      qsim:magnitude_exclusion, qsim:'MagnitudeValue', G),
  rdfs_assert_isDefinedBy(qsim:magnitude_exclusion,                        G),

  % Q <-- m --->
  rdfs_assert_subproperty(qsim:magnitude, qsim:dynamic_relation, G),
  rdfs_assert_label(      qsim:magnitude, 'magnitude          ', G),
  rdfs_assert_domain(     qsim:magnitude, qsim:'Quantity',       G),
  rdfs_assert_range(      qsim:magnitude, qsim:'MagnitudeValue', G),
  rdfs_assert_isDefinedBy(qsim:magnitude,                        G),
  
  % Q <-- d --->
  rdfs_assert_subproperty(qsim:derivative, qsim:dynamic_relation,  G),
  rdfs_assert_label(      qsim:derivative, 'derivative          ', G),
  rdfs_assert_domain(     qsim:derivative, qsim:'Quantity',        G),
  rdfs_assert_range(      qsim:derivative, qsim:'DerivativeValue', G),
  rdfs_assert_isDefinedBy(qsim:derivative,                         G),
  
  % Static relations
  % QMV <-- corr --> QMV
  rdfs_assert_subproperty( qsim:static_relation, qsim:relation,        G),
  rdfs_assert_isDefinedBy( qsim:static_relation,                       G),
  
  % E --has-quantity--> Q
  rdfs_assert_subproperty(qsim:has_quantity, qsim:relation,   G),
  rdfs_assert_label(      qsim:has_quantity, 'has quantity',  G),
  rdfs_assert_domain(     qsim:has_quantity, qsim:'Entity',   G),
  rdfs_assert_range(      qsim:has_quantity, qsim:'Quantity', G),
  
  % QMV <-- correspondence --> QMV
  rdfs_assert_subproperty( qsim:correspondence, qsim:static_relation,  G),
  rdfs_assert_label(       qsim:correspondence, 'correspondence',      G),
  rdfs_assert_domain_range(qsim:correspondence, qsim:'MagnitudeValue', G),
  rdfs_assert_isDefinedBy( qsim:correspondence,                        G),
  
  % QDV <-- d_rel --> QDV
  rdfs_assert_subproperty( qsim:d_rel, qsim:static_relation,   G),
  rdfs_assert_label(       qsim:d_rel, 'derivative relation',  G),
  rdfs_assert_domain_range(qsim:d_rel, qsim:'DerivativeValue', G),
  rdfs_assert_isDefinedBy( qsim:d_rel,                         G),

  % Q <-- mono ---> Q
  rdfs_assert_subproperty( qsim:m, qsim:static_relation, G),
  rdfs_assert_label(       qsim:m, 'monotonic function', G),
  rdfs_assert_domain_range(qsim:m, qsim:'Quantity',      G),
  rdfs_assert_isDefinedBy( qsim:m,                       G),

  % Q <-- mono_pos ---> Q
  rdfs_assert_subproperty(qsim:m_pos, qsim:m,                        G),
  rdfs_assert_label(      qsim:m_pos, 'positive monotonic function', G),
  rdfs_assert_isDefinedBy(qsim:m_pos,                                G),

  % Q <-- mono_neg ---> Q
  rdfs_assert_subproperty(qsim:m_neg, qsim:m,                        G),
  rdfs_assert_label(      qsim:m_neg, 'negative monotonic function', G),
  rdfs_assert_isDefinedBy(qsim:m_neg,                                G),
  
  % Within `Class`.
  owl_assert_disjointWith(qsim:'Entity',           qsim:'Quantity',         G),
  owl_assert_disjointWith(qsim:'Entity',           qsim:'QuantitySpace',    G),
  owl_assert_disjointWith(qsim:'Entity',           qsim:'QualitativeValue', G),
  owl_assert_disjointWith(qsim:'Entity',           qsim:'State',            G),
  owl_assert_disjointWith(qsim:'Quantity',         qsim:'QuantitySpace',    G),
  owl_assert_disjointWith(qsim:'Quantity',         qsim:'QualitativeValue', G),
  owl_assert_disjointWith(qsim:'Quantity',         qsim:'State',            G),
  owl_assert_disjointWith(qsim:'QuantitySpace',    qsim:'QualitativeValue', G),
  owl_assert_disjointWith(qsim:'QuantitySpace',    qsim:'State',            G),
  
  % Within `QualitativeValue`.
  owl_assert_disjointWith(qsim:'DerivativeValue',  qsim:'MagnitudeValue',   G),
  
  % Within `MagnitudeValue`.
  owl_assert_disjointWith(qsim:'Interval',         qsim:'Landmark',         G),
  
  % Within `Landmark`.
  owl_assert_disjointWith(qsim:'NegativeLandmark', qsim:'ZeroLandmark',     G),
  owl_assert_disjointWith(qsim:'NegativeLandmark', qsim:'PositiveLandmark', G),
  owl_assert_disjointWith(qsim:'ZeroLandmark',     qsim:'PositiveLandmark', G),
  
  % Hooks into module =|rdf_export|=.
  rdf_global_id(qsim:value, QSIM_Value),
  rdf_register_edge_style(QSIM_Value, dotted).

