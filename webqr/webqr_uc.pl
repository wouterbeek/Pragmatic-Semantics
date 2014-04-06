:- module(
  webqr_uc,
  [
    webqr_uc/2 % +UseCaseNumber:between(1,3)
               % -Graph:atom
  ]
).

/** <module> WebQR use cases

~~~
Ik ben nu een automatisch tekenalgoritme voor QR aan het schrijven.
Mijn basis idee is als volgt: kwantiteiten krijgen wanneer ze gedefinieerd
worden een SVG term met vrije variabelen. Behavioral states zijn collecties
van RDF statements, waarbij de vrije variabelen in de SVG term zijn
geinstantieerd. Nu wil ik van de RDF state en de SVG term naar een SVG zin
komen. Ik heb al gekeken naar XSLT, maar daar zitten nog wel wat haken en ogen
aan voordat dat gaat werken.
~~~

@author Wouter Beek
@tbd Rewrite the use cases in terms of the ontology-agnostic WebQR API.
@version 2013/02-2013/03, 2013/07, 2013/09-2013/10, 2013/12
*/

:- use_module(library(semweb/rdf_db)).
:- use_remote_module(qsim(qsim_build)).
:- use_remote_module(qsim(qsim_read)).
:- use_remote_module(qsim(qsim_engine)).
:- use_remote_module(rdf_file(rdf_serial)).
:- use_remote_module(webqr(webqr_build)).



initial_state1(G, S):-
  webqr_create_model(G),
  webqr_create_circle(
    G,
    'http://dbpedia.org/resource/Category:Amount_of_substance',
    _Amount_Q
  ),
  add_state(G, S).

/*
initial_state1(G, S):-
  webqr_create_model(G),

  add_quantity_definition(G, amount, [zero,full], Amount_QD),
  add_quantity(G, Amount_QD, amount, Amount_Q),
  add_concept(
    G,
    'http://dbpedia.org/resource/Category:Amount_of_substance',
    Amount_Q
  ),

  add_entity_definition(G, beaker, Beaker_ED),
  add_entity(G, Beaker_ED, this_beaker, [Amount_Q], Beaker_E),
  add_concept(G, 'http://dbpedia.org/resource/Beaker_(drinkware)', Beaker_E),

  once(landmark(G, Amount_Q, amount, Amount_Zero_QV, zero)),
  derivative_increasing(Inc),
  add_state(G, S),
  add_dynamic(G, S, add_magnitude( Amount_Q,  Amount_Zero_QV)),
  add_dynamic(G, S, add_derivative(Amount_Q,  Inc           )).

initial_state2(G, S):-
  webqr_create_model(G),

  % Static model
  add_quantity_definition(G, amount, [zero,full,inf], Amount_QD),
  add_quantity(G, Amount_QD, amount, Amount_Q),
  add_concept(
    G,
    'http://dbpedia.org/resource/Category:Amount_of_substance',
    Amount_Q
  ),

  add_quantity_definition(G, length, [zero,top,inf], Length_QD),
  add_quantity(G, Length_QD, length, Length_Q),
  add_concept(G, 'http://dbpedia.org/resource/Length', Length_Q),
  add_static(G, add_monotonic_increasing(Amount_Q, Length_Q)),

  add_entity_definition(G, beaker, Beaker_ED),
  add_entity(G, Beaker_ED, this_beaker, [Amount_Q,Length_Q], Beaker_E),
  add_concept(G, 'http://dbpedia.org/resource/Beaker_(drinkware)', Beaker_E),

  % Values
  once(landmark(G, Amount_Q, amount, Amount_Zero_QV, zero)),
  once(landmark(G, Amount_Q, amount, Amount_Full_QV, full)),
  once(landmark(G, Amount_Q, amount, Amount_Inf_QV,  inf )),
  once(landmark(G, Length_Q, length, Length_Zero_QV, zero)),
  once(landmark(G, Length_Q, length, Length_Top_QV,  top )),
  once(landmark(G, Length_Q, length, Length_Inf_QV,  inf )),
  derivative_increasing(Inc),

  % Static model: with values.
  add_static(G, add_correspondence(Amount_Zero_QV, Length_Zero_QV)),
  add_static(G, add_correspondence(Amount_Full_QV, Length_Top_QV )),
  add_static(G, add_correspondence(Amount_Inf_QV,  Length_Inf_QV )),

  % Dynamic model: intial state
  add_state(G, S),
  add_dynamic(G, S, add_magnitude( Amount_Q,  Amount_Zero_QV)),
  add_dynamic(G, S, add_derivative(Amount_Q,  Inc           )),
  add_dynamic(G, S, add_magnitude( Length_Q,  Length_Zero_QV)),
  add_dynamic(G, S, add_derivative(Length_Q,  Inc           )).

initial_state3(G, S):-
  webqr_create_model(G),

  % Add quantity definitions and quantity individuals.
  % Amount
  add_quantity_definition(G, amount, [zero,full,inf], Amount_QD),
  add_quantity(G, Amount_QD, amount, Amount_Q),
  add_concept(
    G,
    'http://dbpedia.org/resource/Category:Amount_of_substance',
    Amount_Q
  ),

  % Length
  add_quantity_definition(G, length, [zero,top,inf], Length_QD),
  add_quantity(G, Length_QD, length, Length_Q),
  add_concept(G, 'http://dbpedia.org/resource/Length', Length_Q),

  % Flows
  add_quantity_definition(G, flow, [minf,zero,inflow,inf], Flow_QD),
  add_quantity(G, Flow_QD, inflow,  Inflow_Q ),
  add_quantity(G, Flow_QD, netflow, Netflow_Q),
  add_quantity(G, Flow_QD, outflow, Outflow_Q),
  add_concept(G, 'http://dbpedia.org/resource/Inflow', Inflow_Q),
  add_concept(G, 'http://dbpedia.org/resource/Netflow', Netflow_Q),
  add_concept(G, 'http://dbpedia.org/resource/Outflow', Outflow_Q),

  % Sink
  add_entity_definition(G, sink, Sink_ED),
  add_entity(
    G,
    Sink_ED,
    this_sink,
    [Amount_Q,Inflow_Q,Length_Q,Netflow_Q,Outflow_Q],
    Sink_E
  ),
  add_concept(G, 'http://dbpedia.org/resource/Sink', Sink_E),

  % Add static statements: monotonicity, summation, and derivative relations.
  add_static(G, add_monotonic_increasing(Amount_Q, Length_Q)),
  add_static(G, add_monotonic_increasing(Length_Q, Outflow_Q)),
  add_static(G, add_sum(Outflow_Q, Netflow_Q, Inflow_Q)),
  add_static(G, add_derivative_relation(Amount_Q, Netflow_Q)),

  % Retrieve some magnitude quantity values.
  once(landmark(G, Amount_Q,  amount,  Amount_Zero_QV, zero  )),
  once(landmark(G, Amount_Q,  amount,  Amount_Full_QV, full  )),
  once(landmark(G, Amount_Q,  amount,  Amount_Inf_QV,  inf   )),
  once(landmark(G, Length_Q,  length,  Length_Zero_QV, zero  )),
  once(landmark(G, Length_Q,  length,  Length_Top_QV,  top   )),
  once(interval(G, Length_Q,  length,  Length_Top_QV,  top,
                                       Length_Inf_QV,  inf,
                                       Length_Top_Inf_QV     )),
  once(landmark(G, Netflow_Q, netflow, Flow_Inflow_QV, inflow)),
  once(landmark(G, Outflow_Q, outflow, Flow_Zero_QV,   zero  )),

  % Retrieve the derivative quantity values.
  derivative_increasing(Inc),
  derivative_steady(Std),
  derivative_decreasing(Dec),

  % Add static correspondences between magnitude quantity values.
  add_static(G, add_correspondence(Amount_Zero_QV, Length_Zero_QV)),
  add_static(G, add_correspondence(Amount_Full_QV, Length_Top_QV )),
  add_static(G, add_correspondence(Amount_Inf_QV,  Length_Inf_QV )),

  % Add static quantity values: for quantity Inflow.
  add_static(G, add_magnitude(Inflow_Q, Flow_Inflow_QV)),
  add_static(G, add_derivative(Inflow_Q, Std)),
  add_static(G, add_magnitude_exclusion(Length_Q, Length_Top_Inf_QV)),

  % Define the initial state.
  add_state(G, S),

  % Add dynamic quantity values: Amount, Outflow, Netflow, and Length.
  add_dynamic(G, S, add_magnitude( Amount_Q,  Amount_Zero_QV)),
  add_dynamic(G, S, add_derivative(Amount_Q,  Inc           )),
  add_dynamic(G, S, add_magnitude( Inflow_Q,  Flow_Inflow_QV)),
  add_dynamic(G, S, add_derivative(Inflow_Q,  Std           )),
  add_dynamic(G, S, add_magnitude( Outflow_Q, Flow_Zero_QV  )),
  add_dynamic(G, S, add_derivative(Outflow_Q, Inc           )),
  add_dynamic(G, S, add_magnitude( Netflow_Q, Flow_Inflow_QV)),
  add_dynamic(G, S, add_derivative(Netflow_Q, Dec           )),
  add_dynamic(G, S, add_magnitude( Length_Q,  Length_Zero_QV)),
  add_dynamic(G, S, add_derivative(Length_Q,  Inc           )).
*/

webqr_uc(_N, G):-
  rdf_graph(G), !.
webqr_uc(N, G):-
  format(atom(Pred), 'initial_state~w', [N]),
  call(Pred, G, InitialState),
  simulate(G, InitialState).
