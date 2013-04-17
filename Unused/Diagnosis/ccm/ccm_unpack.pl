:- module(
  ccm_unpack,
  [
    component_hierarchical_unpack/4 % +Diagnosis:diagnosis
                                    % +Unpackable:component_cloud
                                    % -Unpacked:ord_set(component_cloud)
                                    % -Removed:ord_set(component_cloud)
  ]
).

/** <module> CCM_UNPACK

Methods for unpacking aggregate components.

@author Wouter Beek
@version 2012/02
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_db)).
:- use_module(atms(atms_build)).
:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_db)).
:- use_module(diagnosis(diagnosis)).
:- use_module(generic(meta_ext)).
:- use_module(ile(agent)).



%% component_hierarchical_unpack(
%%   +Diagnosis:diagnosis,
%%   +Unpackable:component_cloud,
%%   -Unpacked:ord_set(component_cloud),
%%   -Removed:ord_set(component_cloud)
%% ) is semidet.
% Unpacks the given component, if it is an aggregate component that can be
% unpacked.
% For components that cannot unpack this method fails.
%
% @param Diagnosis A diagnosis.
% @param Unpackable A component cloud.
% @param Unpacked An ordered set of component clouds.
% @param Removed An ordered set of component clouds that have been removed.

component_hierarchical_unpack(Diagnosis, Unpackable, Unpacked, Removed):-
  % The component cloud can be unpacked.
  % This excludes competitive aggregate component clouds.
  unpackable_component_cloud(Unpackable),

  % The component cloud has not been unpacked and disabled before.
  \+(disabled_component_cloud(Unpackable)),

  recursive_unpack(Diagnosis, [Unpackable], Unpacked, Removed).

%% recursive_unpack(
%%   +Diagnosis:diagnosis,
%%   +Tops:ord_set(component_cloud),
%%   -Bots:ord_set(component_cloud),
%%   -RemovedNodes:ord_set(component_cloud)
%% ) is det.
% Recursively unpack aggregate component clouds.
% Suppose there are three base component clouds A, B, C.
% Suppose there are two aggregate component clouds [A,B], [B,C].
% If we only unpack [B,C] then the input port for C is problematic
% (since B is in [A,B]). So we recursively check for subsuming component
% clouds that subsume component clouds that belong to the initial set
% of unpacked component clouds.
%
% @param Diagnosis A diagnosis.
% @param Tops An ordered set of component clouds.
% @param Bots An ordered set of component clouds.
% @param RemovedNodes An ordered set of component clouds that
%        have been removed in the process.

recursive_unpack(_Diagnosis, [], [], []):-
  !.
% An unpackable component cloud.
recursive_unpack(Diagnosis, [Top | Tops], Bots, RemovedNodes):-
  unpackable_component_cloud(Top),
  !,

  % Retrieve the subsumed component clouds.
  component_cloud_to_subsumed_component_clouds(Top, TopBots1),
  % Also include the support component clouds for the subsumed
  % component clouds.
  maplist(
    component_cloud_to_support_component_clouds,
    TopBots1,
    TopBotsSupportss
  ),
  ord_union([TopBots1 | TopBotsSupportss], TopBots2),
  % Consider the bottom component clouds.
  learner(Diagnosis, Learner),
  maplist(consider_component_cloud(Learner), TopBots2),

  % Diable the top component cloud and its components.
  component_cloud_to_components(Top, Components),
  maplist(disable_component, Components),
  disable_component_cloud(Top),

  % Assume the bottom component clouds.
  diagnosis_atms(Diagnosis, ATMS),
  maplist(find_or_add_node(ATMS), TopBots2, TopBotNodes),
  maplist(assume_node, TopBotNodes),
  % Also consider their directly connected point clouds.
  component_clouds_to_point_clouds(TopBots2, TopBots2PCs),
  maplist(consider_point_cloud(Learner), TopBots2PCs),

  % Deconsider the top component cloud.
  deconsider_component_cloud(Learner, Top),
  % Also deconsider the retrieval component clouds for the top component
  % cloud.
  component_cloud_to_support_component_clouds(Top, TopSupportCCs),
  maplist(deconsider_component_cloud(Learner), TopSupportCCs),
  % Also deconsider the support point clouds for the top component cloud.
  component_cloud_to_support_point_clouds(Top, TopSupportPCs),
  maplist(deconsider_point_cloud(Learner), TopSupportPCs),

  % Remove the top component cloud node from the ATMS.
  maplist(node(ATMS), [Top | TopSupportCCs], TopNodes_),
  list_to_ord_set(TopNodes_, TopNodes),
  maplist(atms_remove_node(ATMS), TopNodes),

  % Other component clouds may (among other things) subsume the component
  % cloud that has just been unpacked. These should be added to the recursion.
  setoff(
    AlsoTop,
    (
      component_cloud_subsumes_component_cloud(AlsoTop, TopBot),
      member(TopBot, TopBots2),
      AlsoTop \== Top
    ),
    AlsoTops
  ),
  ord_union(Tops, AlsoTops, TotalTops),

  % DEBUG
  debug(ccm_aggr, '[UNPACK] From: ~w To: ~w', [Top, TopBots2]),

  % Recursion
  recursive_unpack(Diagnosis, TotalTops, TopsBots, RemovedNodes_),
  ord_union(TopBots2, TopsBots, Bots),
  ord_union(TopNodes, RemovedNodes_, RemovedNodes).
% There is nothing to unpack for this component cloud.
recursive_unpack(Diagnosis, [_Top | Tops], TopsBots, RemovedNodes):-
  recursive_unpack(Diagnosis, Tops, TopsBots, RemovedNodes).
