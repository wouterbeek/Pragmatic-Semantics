:- module(
  causal_explanation_export,
  [
    print_request/1 % +Request:request
  ]
).

/** <module> Why export

@author Wouter Beek
@version Mar2012
*/

:- use_module(ccm(ccm_label)).
:- use_module(generic(file_ext)).
:- use_module(generic(os_ext)).
:- use_module(qr(causal_explanation)).



print_request(Request):-
  request_id(Request, RequestID),
  format(atom(PrintsFlag), 'request_prints_~w', [RequestID]),
  flag(PrintsFlag, PrintID, PrintID + 1),
  format(atom(Name), 'request_~w_~w', [RequestID, PrintID]),
  create_file(debug(ce), Name, graphviz, AbsoluteFile),
  open(
    AbsoluteFile,
    write,
    _Stream
    [alias(request), close_on_abort(true), type(text)]
  ),
  format(request, 'digraph circuit {\n', []),
  
  % Export all requests.
  requests(Requests),
  forall(
    member(Request, Requests),
    (
      request_label(Request, RequestLabel),!,
      request_to_dot_name(Request, RequestDOTName),
      format(
        request,
        '  ~w [fontsize="11", label="~w"];\n',
        [RequestDOTName, RequestLabel]
      )
    )
  ),

  % Export all explanation offsets.
  explanation_offsets(ExplanationOffsets),
  forall(
    member(ExplanationOffset, ExplanationOffsets),
    (
      explanation_offset_to_dot_name(
        ExplanationOffset,
        ExplanationOffsetDOTName
      ),
      format(
        request,
        '  ~w [fontsize="11", label="~w"];\n',
        [ExplanationOffsetDOTName, ExplanationOffsetDOTName]
      )
    )
  ),

  % Export all explanations.
  explanations(Explanations),
  forall(
    member(Explanation, Explanations),
    (
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      format(
        request,
        '  ~w [fontsize="11", label="~w"];\n',
        [ExplanationDOTName, ExplanationDOTName]
      )
    )
  ),

  % Export all propositions.
  propositions(Propositions),
  forall(
    member(Proposition, Propositions),
    (
      proposition_to_dot_name(Proposition, PropositionDOTName),
      proposition_label(Proposition, PropositionLabel),!,
      format(
        request,
        '  ~w [fontsize="11", label="~w"];\n',
        [PropositionDOTName, PropositionLabel]
      )
    )
  ),

  format(request, '\n', []),

  % Relations from requests to explanation offsets (1..M).
  findall(
    Request/ExlanationOffset,
    request_explanation_offset(Request, ExlanationOffset),
    RequestExpressionOffsetPairs
  ),
  forall(
    member(Request/ExlanationOffset, RequestExpressionOffsetPairs),
    (
      request_to_dot_name(Request, RequestDOTName),
      explanation_offset_to_dot_name(
        ExlanationOffset,
        ExpressionOffsetDOTName
      ),
      format(
        request,
        '  ~w -> ~w;\n',
        [RequestDOTName, ExpressionOffsetDOTName]
      )
    )
  ),

  % Relations from explanation offsets to explanations (1..M).
  findall(
    ExlanationOffset/Explanation,
    explanation_offset_explanation(ExlanationOffset, Explanation),
    ExpressionOffsetExplanationPairs
  ),
  forall(
    member(ExlanationOffset/Explanation, ExpressionOffsetExplanationPairs),
    (
      explanation_offset_to_dot_name(
        ExlanationOffset,
        ExlanationOffsetDOTName
      ),
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      format(
        request,
        '  ~w -> ~w;\n',
        [ExlanationOffsetDOTName, ExplanationDOTName]
      )
    )
  ),

  % Relations from explanations to premises (1..M).
  forall(
    explanation_premise(Explanation, Premise),
    (
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      proposition_to_dot_name(Premise, PremiseDOTName),
      format(
        request,
        '  ~w -> ~w [label="premise"];\n',
        [ExplanationDOTName, PremiseDOTName]
      )
    )
  ),

  % Relations from explanations to derivation rules (1..M).
  forall(
    explanation_derivation_rule(Explanation, DerivationRule),
    (
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      proposition_to_dot_name(DerivationRule, DerivationRuleDOTName),
      format(
        request,
        '  ~w -> ~w [label="derivation rule"];\n',
        [ExplanationDOTName, DerivationRuleDOTName]
      )
    )
  ),

  % Relations from explanations to conclusions (1..M).
  forall(
    explanation_conclusion(Explanation, Conclusion),
    (
      explanation_to_dot_name(Explanation, ExplanationDOTName),
      proposition_to_dot_name(Conclusion, ConclusionDOTName),
      format(
        request,
        '  ~w -> ~w [label="conclusion"];\n',
        [ExplanationDOTName, ConclusionDOTName]
      )
    )
  ),

  format(request, '\n', []),
  format(request, '  overlap=false\n', []),
  request_label(Request, RequestLabel),!,
  request_to_dot_name(Request, RequestDOTName),
  format(request, '  charset="UTF-8"\n', []),
  format(request, '  fontsize="11"\n', []),
  format(
    request,
    '  label="Request: ~w  Argument: ~w"\n',
    [RequestDOTName, RequestLabel]
  ),
  format(request, '}\n', []),
  close(request).

%%%%write_explanation_nodes(causal_graph),
%%%%write_explanation_offset_nodes(causal_graph),
%%%%write_request_nodes(causal_graph),
%%%%write_explanation_offset_to_explanation_relations(causal_graph),
%%%%write_explanation_to_component_relations(causal_graph),
%%%%write_explanation_to_point_relations(causal_graph),
%%%%write_request_to_explanation_offset_relations(causal_graph),
