:- module(
  ccm_label,
  [
% AGENT
    agent_id/2, % ?Agent:agent
                % ?AgentID:number
    agent_label/2, % ?Agent:agent
                   % ?AgentLabel:atom
    agent_to_dot_name/2, % +Agent:agent
                         % -AgentDOTName:atom

% EXPLANATION
    explanation_id/2, % +Explanation:explanation
                      % -ExplanationBareID:number
    explanation_label/2, % ?Explanation:explanation
                         % ?ExplanationLabel:atom
    explanation_to_dot_name/2, % +Explanation:explanation
                               % -ExplanationDOTName:atom

% EXPLANATION OFFSET
    explanation_offset_id/2, % +ExplanationOffset:explanation
                             % -ExplanationOffsetBareID:number
    explanation_offset_label/2, % ?ExplanationOffset:explanation
                                % ?ExplanationOffsetLabel:atom
    explanation_offset_to_dot_name/2, % +ExplanationOffset:explanation
                                      % -ExplanationOffsetDOTName:atom

% PROPOSITION
    proposition_id/2, % ?Proposition:proposition
                      % ?PropositionID:number
    proposition_label/2, % ?Proposition:proposition
                         % ?PropositionLabel:atom
    proposition_to_dot_name/2, % +Proposition:proposition
                               % -PropositionDOTName:atom

% REQUEST
    request_id/2, % ?Request:request
                  % ?RequestID:number
    request_label/2, % ?Request:explanation
                     % ?RequestLabel:atom
    request_to_dot_name/2 % +Request:explanation
                          % -RequestDOTName:atom
  ]
).

/** <module> CCM labels

This module specifically focusses on the labels of the respective elements
that constitute the CCM, i.e. components, points, and spaces.

These labels are important because of the following reasons:
  1. *Identification*:
    1. An =|ID|= + type uniquely identifies an element.
    2. A =|URI|= uniquely identifies an element.
  2. *Debugging*:
    1. =|DEBUG-DESCRIPTION|= that gives a descriptive name for debugging purposes.
    2. =|ID|= is used in DOT to internally name nodes.
    3. =|ID|= is used in DOT to externally name / display nodes.
  3. *|Natural language explanations|*: =|NATURAL-LANGUAGE-DESCRIPTION|=
     gives a full natural language explanation of the element.

The methods are named as follows:
  1. =|TYPE_id/2|=
  2. =|TYPE_to_dot_name/2|= serves reasons 1.1, 2.2, and 2.3.
  3. =|TYPE_label/2|= serves reason 3,
     =|NATURAL-LANGUAGE-DESCRIPTION|=.

@author Wouter Beek
@version 2011/12-2012/02, 2012/05
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_db)).
:- use_module(ccm(ccm_verb)).
:- use_module(ccm(ccm_api)).
:- use_module(generic(atom_ext)).
:- use_module(ile(agent)).
:- use_module(library(semweb/rdf_db)).
:- use_module(qr(qr_api)).
:- use_module(rdf(rdf_read)).



% AGENT %

agent_id(Agent, AgentID):-
  atms_id(Agent, AgentID).

% agent_label(?Agent:agent, -AgentLabel:atom) is nondet.
% Returns the =Label= describing the given agent.
%
% @param Agent An agent.
% @param Label The atomic natural language label of an agent.

agent_label(Agent, AgentLabel):-
  atms_label(Agent, AgentLabel).

%% agent_to_dot_name(+Agent:agent, -AgentDOTName:integer) is det.
% Returns the identifier of the =Agent=.
%
% @param Agent An agent.
% @param AgentDOTName The atomic identifier of the given =Agent=.

agent_to_dot_name(Agent, AgentDOTName):-
  atms_id(Agent, AgentID),
  atomic_concat('ag_', AgentID, AgentDOTName).


% EXPLANATION %

explanation_id(Explanation, ExplanationBareID):-
  nonvar(Explanation),
  !,
  explanation_id1(Explanation, ExplanationBareID),
  !.
explanation_id(Explanation, ExplanationBareID):-
  explanation_id1(Explanation, ExplanationBareID).

explanation_id1(Explanation, ExplanationBareID):-
  rdf_datatype(Explanation, explanation:has_id, integer, ExplanationBareID, ccm).

explanation_to_dot_name(Explanation, ExplanationDOTName):-
  explanation_id(Explanation, ExplanationID),
  format(atom(ExplanationDOTName), 'ex_~w', [ExplanationID]).

explanation_label(Explanation, ExplanationLabel):-
  rdfs_label(Explanation, ExplanationLabel).


% EXPLANATION OFFSET %

explanation_offset_id(ExplanationOffset, ExplanationOffsetID):-
  nonvar(ExplanationOffset),
  !,
  explanation_offset_id1(ExplanationOffset, ExplanationOffsetID),
  !.
explanation_offset_id(ExplanationOffset, ExplanationOffsetID):-
  explanation_offset_id1(ExplanationOffset, ExplanationOffsetID).

explanation_offset_id1(ExplanationOffset, ExplanationOffsetID):-
  rdf_datatype(ExplanationOffset, explanation:has_id, integer, ExplanationOffsetID, ccm).

explanation_offset_label(ExplanationOffset, ExplanationOffsetLabel):-
  rdfs_label(ExplanationOffset, ExplanationOffsetLabel).

explanation_offset_to_dot_name(ExplanationOffset, ExplanationOffsetDOTName):-
  explanation_offset_id(ExplanationOffset, ExplanationOffsetID),
  format(atom(ExplanationOffsetDOTName), 'exoff_~w', [ExplanationOffsetID]).


% PROPOSITION %

proposition_id(Proposition, PropositionBareID):-
  rdf_datatype(Proposition, explanation:has_id, integer, PropositionBareID, ccm).

proposition_to_dot_name(Proposition, PropositionDOTName):-
  proposition_id(Proposition, PropositionID),
  format(atom(PropositionDOTName), 'prop_~w', [PropositionID]).

proposition_label(Proposition, PropositionLanguageLabel):-
  rdfs_label(Proposition, PropositionLanguageLabel).


% REQUEST %

request_id(Request, RequestID):-
  nonvar(Request),
  !,
  request_id1(Request, RequestID),
  !.
request_id(Request, RequestID):-
  request_id1(Request, RequestID).

request_id1(Request, RequestID):-
  rdf_datatype(Request, explanation:has_id, integer, RequestID, ccm).

request_to_id(Request, RequestID):-
  request_id(Request, RequestID1),
  format(atom(RequestID), 'req_~w', [RequestID1]).

request_label(Request, RequestLabel):-
  rdfs_label(Request, RequestLabel).

request_to_dot_name(Request, RequestDOTName):-
  request_id(Request, RequestID),
  format(atom(RequestDOTName), 'req_~w', [RequestID]).
