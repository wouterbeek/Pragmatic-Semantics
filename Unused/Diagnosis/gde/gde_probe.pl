:- module(
  gde_probe,
  [
    find_probe_point_weights/3 % +Diagnosis:diagnosis
                               % +Candidates:ord_set(environment)
                               % -SortedProbePoints:list(point_cloud)
  ]
).

/** <module> GDE PROBE

Calculate which probes to ask based on a number of candidates from GDE.

@author Wouter Beek
@version May 2012, Aug 2012
*/

:- use_module(atms(atms_api)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_prob)).
:- use_module(diagnosis(diagnosis)).
:- use_module(generic(list_ext)).
:- use_module(generic(math_ext)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).



% SETTINGS %

% Choose an algorithm to calculate the weight for candidates.
calculate_candidate_weight(default_candidate_weight).

% Choose an algorithm to calculate the weight for components.
calculate_component_cloud_weight(candidate_component_cloud_weight).
%calculate_component_cloud_weight(default_component_cloud_weight).

% Choose an algorithm to calculate the weight for point clouds.
%calculate_point_cloud_weight(default_point_cloud_weight).
calculate_point_cloud_weight(neighbor_point_cloud_weight).

% Choose an algorithm to calculate the weight for probe point clouds.
calculate_probe_point_cloud_weight(neighbor_probe_point_cloud_weight).
calculate_probe_point_cloud_weight(random_probe_point_cloud_weight).
%calculate_probe_point_cloud_weight(splits_probe_point_cloud_weight).



% PROBE POINT GENERATION AND ORDERING %

%% find_probe_point_weights(
%%   +Diagnosis:diagnosis,
%%   +Candidates:ord_set(environment),
%%   -SortedPointWeights:list(point/integer)
%% ) is det.
% Returns a sorted list of probe point clouds. The head is the best
% probe to pose, i.e. the one that would provide the most information
% for the GDE algorithm.
%
% @param Diagnosis A diagnosis.
% @param Candidates An ordered set of environments.
% @param SortedPointWeights A list of point/integer pairs.
%        The order is relevant.

find_probe_point_weights(Diagnosis, Candidates, SortedPointWeights):-
  % 1. Calculate candidate weights.
  calculate_candidate_weight(CalculateCandidateWeightPredicate),
  maplist(CalculateCandidateWeightPredicate, Candidates, CandidateWeights),

  % 2. Calculate component weights.
  maplist(environment_assumptions, Candidates, ComponentCloudss),
  ord_union(ComponentCloudss, ComponentClouds),
  calculate_component_cloud_weight(CalculateComponentCloudWeightPredicate),
  CalculateComponentCloudWeightCall =..
    [CalculateComponentCloudWeightPredicate, CandidateWeights],
  maplist(
    CalculateComponentCloudWeightCall,
    ComponentClouds,
    ComponentCloudWeights
  ),

  % 3. Calculate point cloud weights.
  learner(Diagnosis, Learner),
  does_consider_point_clouds(Learner, ConsideredPointClouds),
  calculate_point_cloud_weight(CalculatePointCloudWeightPredicate),
  CalculatePointCloudWeightCall =..
    [CalculatePointCloudWeightPredicate, ComponentCloudWeights],
  maplist(
    CalculatePointCloudWeightCall,
    ConsideredPointClouds,
    ConsideredPointCloudWeights
  ),

  % 4. Calculate the point weights based on the point cloud weights.
  findall(
    Point/PointCloudWeight,
    (
      diagnosis_probable_point(Diagnosis, Point),
      point_cloud(Point, PointCloud),
      member(PointCloud/PointCloudWeight, ConsideredPointCloudWeights),
      PointCloudWeight \== 0
    ),
    PointWeights
  ),

  % Sort the probe points based on their weights.
  predsort_with_duplicates(greater_weight, PointWeights, SortedPointWeights).

%% combined_weights(
%%   +ElementWeightss:list(list(compound)),
%%   -CombinedWeights:list(compound)
%% ) is det.
% Extracts the weights as they have been assigned by the various methods.
% These weights are combined into a single weight.
%
% @param ElementWeightss A list of lists of compound terms, i.e. pairs of an
%        element and a weight.
% @param CombinedWeights A list of compound terms, i.e. pairs of element and
%        weight.

combined_weights(ElementWeightss, CombinedWeights):-
  findall(
    Element/CombinedWeight,
    (
      first(ElementWeightss, FirstElementWeights),
      nth0(Index, FirstElementWeights, Element/_Weight),
      findall(
        Weight,
        (
          member(ElementWeights, ElementWeightss),
          nth0(Index, ElementWeights, Element/Weight)
        ),
        Weights
      ),
      sum_list(Weights, CombinedWeight)
    ),
    CombinedWeights
  ).

greater_weight(Order, _Element1/Weight1, _Element2/Weight2):-
  compare(Order, Weight2, Weight1).

smaller_weight(Order, _Element1/Weight1, _Element2/Weight2):-
  compare(Order, Weight1, Weight2).



% [1] CALCULATE CANDIDATE WEIGHTS %

%% default_candidate_weight(
%%   +Candidate:ord_set(environment),
%%   +CandidateWeight:ord_set(environment)/float
%% ) is det.
% Returns the weight of the given candidate.
%
% @param Candidate An ordered set of components.
% @param CandidateWeight A pair consisting of the given candidate and a
%        float value indicating its weight.

default_candidate_weight(Candidate, Candidate/CandidateWeight):-
  environment_assumptions(Candidate, ComponentClouds),
  maplist(
    component_cloud_to_probability,
    ComponentClouds,
    ComponentCloudProbabilities
  ),
  sum_list(ComponentCloudProbabilities, CandidateWeight).



% [2] CALCULATE COMPONENT CLOUD WEIGHTS %

%% candidate_component_cloud_weight(
%%   +CandidateWeights:list(environment/float)
%%   +ComponentCloud:component_cloud,
%%   -ComponentCloudWeight:component_cloud/float
%% ) is det.
% Returns the weigth of the given component clouds based on the given
% candidate weights.
%
% @param CadidateWeights A list of pairs consisting of an environment and a
%        float.
% @param ComponentCloud A component cloud.
% @param ComponentCloudWeight A pair of a component cloud and a float.

candidate_component_cloud_weight(
  CandidateWeights,
  ComponentCloud,
  ComponentCloud/ComponentCloudWeight
):-
  findall(
    CandidateWeight,
    (
      member(Candidate/CandidateWeight, CandidateWeights),
      environment_assumptions(Candidate, ComponentClouds),
      member(ComponentCloud, ComponentClouds)
    ),
    ComponentCloudWeights
  ),
  sum_list(ComponentCloudWeights, ComponentCloudWeight).

default_component_cloud_weight(
  _CandidateWeights,
  ComponentCloud,
  ComponentCloud/ComponentCloudWeight
):-
  component_cloud_to_probability(ComponentCloud, ComponentCloudWeight).



% [3] CALCULATE POINT CLOUD WEIGHTS %

default_point_cloud_weight(
  _ComponentCloudWeights,
  PointCloud,
  PointCloud/PointCloudWeight
):-
  point_cloud_to_probability(PointCloud, PointCloudWeight).

neighbor_point_cloud_weight(
  ComponentCloudWeights,
  PointCloud,
  PointCloud/PointCloudWeight
):-
  findall(
    NeighborComponentCloudWeight,
    (
      component_cloud_point_cloud(NeighborComponentCloud, PointCloud),
      member(
        NeighborComponentCloud/NeighborComponentCloudWeight,
        ComponentCloudWeights
      )
    ),
    NeighborComponentCloudWeights
  ),
  sum_list(NeighborComponentCloudWeights, PointCloudWeight).



% [4] CALCULATE PROBE POINT CLOUD WEIGHTS %

neighbor_probe_point_cloud_weight(
  _Agent,
  _CandidateWeights,
  ComponentCloudWeights,
  _PointCloudWeights,
  PointCloud,
  PointCloud/PointCloudWeight
):-
  neighbor_point_cloud_weight(
    ComponentCloudWeights,
    PointCloud,
    PointCloud/PointCloudWeight
  ).

random_probe_point_cloud_weight(
  _Agent,
  _CandidateWeights,
  _ComponentCloudWeights,
  _PointCloudWeights,
  ProbePointCloud,
  ProbePointCloud/ProbePointCloudWeight
):-
  ProbePointCloudWeight is random_float.

/*
splits_probe_point_cloud_weight(
  Agent,
  _CandidateWeights,
  ComponentCloudWeights,
  _PointCloudWeights,
  ProbePointCloud,
  ProbePointCloud/ProbePointCloudWeight
):-
  findall(
    ComponentCloud,
    member(ComponentCloud/_ComponentCloudWeight, ComponentCloudWeights),
    ComponentClouds
  ),
  point_cloud_splits_component_clouds(
    Agent,
    ComponentClouds,
    ProbePointCloud,
    Paths
  ),
  maplist(length, Paths, PathLengths),

  % We are interested in point clouds with the following two properties:
  %   1. As many paths as possible.
  %   2. As few distinctions between the lengths of the various paths
  %      as possible (preferably zero).
  % A quick guess of a good approximation for the optimal function is:
  % number-of-paths^2 / (max(path-lengths) - min(path-lengths) + 1)
  length(PathLengths, NumberOfPaths),
  max_list(PathLengths, MaximumPathLength),
  min_list(PathLengths, MinimumPathLength),
  Temp1 is MaximumPathLength - MinimumPathLength,
  Temp2 is Temp1 + 1,
  Temp3 is NumberOfPaths * NumberOfPaths,
  ProbePointCloudWeight is Temp3 / Temp2.
*/
