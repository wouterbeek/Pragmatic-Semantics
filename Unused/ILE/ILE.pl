:- module(
  ile,
  [
    add_ile/0,
    ile/1 % ?ILE:ile
  ]
).

/** <module> ILE, Integrated Learning Environment

The ILE, Integrated Learning Environment main architecture.

A learning environment instance is created to which the following
two agents:
    1. A teacher agent *Socrates* and a learner agent *Plato*.

@author Wouter Beek
@version 2012/08-2012/10.
*/

:- use_module(ile(agent)).
:- use_module(rdf(rdf_build)).



%% add_ile is det.
% Adds a learning environment object.

add_ile:-
  flag(ile_id, 0, 0),
  !,
  init_ile,
  
  % Create a ILE instance.
  flag(ile_id, ID, ID + 1),
  format(atom(Name), 'ile_~w', [ID]),
  rdf_global_id(ile:Name, Instance),
  rdf_assert(Instance, rdf:type, ile:ile, ile),
  format(atom(Label), 'Learning environment instance ~w.', [ID]),
  rdfs_assert_label(Instance, Label, ile),
  
  % Create the learner and teacher agents.
  add_teacher('Socrates', Teacher),
  rdf_assert(ILE, ile:has_teacher, Teacher, ile),
  add_learner('Plato', Learner),
  rdf_assert(ILE, ile:has_learner, Learner, ile).
add_ile:-
  debug(
    generic,
    'Multiple and related (= running under the same SWI-Prolog instance) instances of learning environments are currently not supported.',
    []
  ).

%% ile(ILE) is semidet.
% A ILE ILE instance.
% @param ILE A resource.

ile(ILE):-
  rdf_global_id(ile:'0', ILE).

current_simulation(CurrentSimulation):-
  rdf(ile:ile, ile:has_current_simulation, CurrentSimulation, ile).

%% init_ile is det.
% Initialises the triples that are needed for asserting a
% learning environment.

init_ile:-
  rdf_register_prefix(agent, 'http://www.wouterbeek.com/prasem/agent.owl#'),
  rdf_register_prefix(use_case, 'http://www.wouterbeek.com/prasem/use_case.owl#'),

  rdfs_assert_label(rdfs:'Class', 'class', ile),
  rdfs_assert_label(rdf:'Property', 'property', ile),

  rdf_assert(ile:ile, rdf:type, rdf:'Class', ile),
  rdfs_assert_label(ile:ile, 'Learning environments', ile),
  
  rdf_assert(ile:relation, rdfs:subPropertyOf, rdfs:'Property', ile),
  rdfs_assert_label(ile:relation, 'A relation with a learning environment main object.', ile),
  
  rdf_assert(ile:has_current_simulation, rdfs:subPropertyOf, ile:relation, ile),
  rdfs_assert_label(ile:has_current_simulation, 'A learning environment\'s current simulation use case.', ile),

  rdf_assert(ile:has_learner, rdfs:subPropertyOf, ile:relation, ile),
  rdfs_assert_label(ile:has_learner, 'A learning environment has a learner agent.', ile),

  rdf_assert(ile:has_teacher, rdfs:subPropertyOf, ile:relation, ile),
  rdfs_assert_label(ile:has_teacher, 'A learning environment has a teacher agent.', ile),
  
  flag(ile_id, _OldID, 0).
