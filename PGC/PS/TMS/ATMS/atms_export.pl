:- module(
  atms_export,
  [
    print_atms/2 % +ATMS:atms
                 % +Explanation:atom
  ]
).

/** <module> Exports ATMSs to DOT files.

This module is known to work with GraphViz version 2.28.0.

@author Wouter Beek
@version 2011/11-2012/02
*/

:- use_module(atms(atms_api)).
:- use_module(atms(atms_db)).
:- use_module(os(file_ext)).



%% node_color(+Node:node, -Color:atom) is det.
% Returns the color of the given node.
%
% @arg Node Either a point or a component.
% @arg Color The atomic name of a color.

node_color(Node, green):-
  is_in_node(Node),
  !.
node_color(Node, red):-
  is_contradiction(Node),
  !.
node_color(_Node, black).

print_atms(ATMS, Explanation):-
  atms_id(ATMS, ATMSID),
  format(atom(PrintsFlag), 'atms_prints_~w', [ATMSID]),
  flag(PrintsFlag, PrintID, PrintID + 1),
  format(atom(Name), 'atms_~w_~w', [ATMSID, PrintID]),
  create_file(debug(ccm), Name, graphviz, File),
  open(
    File,
    write,
    _Stream,
    [alias(atms), close_on_abort(true), type(text)]
  ),
  format(atms, 'digraph circuit {\n', []),

  atms_to_nodes(ATMS, Nodes),
  forall(
    member(Node, Nodes),
    (
      node_to_dot_name(Node, NodeDOTName),
      node_label(Node, NodeLabel),
      node_color(Node, NodeColor),
      format(
        atms,
        '  ~w [color="~w", fontsize="11", label="~w", shape="ellipse", style="solid"];\n',
        [NodeDOTName, NodeColor, NodeLabel]
      )
    )
  ),
  atms_to_justifications(ATMS, Justifications),
  forall(
    member(Justification, Justifications),
    (
      justification_label(Justification, JustificationLabel),
      justification_to_dot_name(Justification, JustificationDOTName),
      format(
        atms,
        '  ~w [color="orange", fontsize="11", label="~w", shape="box", style="solid"];\n',
        [JustificationDOTName, JustificationLabel]
      )
    )
  ),
  atms_to_environments(ATMS, Environments),
  forall(
    member(Environment, Environments),
    (
      zero_environment(Environment)
    ;
      (
        nogood(ATMS, Environment)
      ->
        Color = red
      ;
        Color = blue
      ),
      environment_to_dot_name(Environment, EnvironmentDOTName),
      environment_id(Environment, EnvironmentID),
      format(
        atms,
        '  ~w [color="~w", fontsize="11", label="~w", shape="pentagon", style="solid"];\n',
        [EnvironmentDOTName, Color, EnvironmentID]
      )
    )
  ),
  format(atms, '\n', []),
  forall(
    justification_consequence(Justification, Consequence),
    (
      justification_to_dot_name(Justification, JustificationDOTName),
      node_to_dot_name(Consequence, ConsequenceDOTName),
      format(
        atms,
        '  ~w -> ~w;\n',
        [JustificationDOTName, ConsequenceDOTName]
      )
    )
  ),
  forall(
    justification_antecedent(Justification, Antecedent),
    (
      node_to_dot_name(Antecedent, AntecedentDOTName),
      justification_to_dot_name(Justification, JustificationDOTName),
      format(atms, '  ~w -> ~w;\n', [AntecedentDOTName, JustificationDOTName])
    )
  ),
  forall(
    node_environment(Node, Environment),
    (
      node_to_dot_name(Node, NodeDOTName),
      environment_to_dot_name(Environment, EnvironmentDOTName),
      format(atms, '  ~w -> ~w;\n', [NodeDOTName, EnvironmentDOTName])
    )
  ),

  format(atms, '\n', []),
  format(atms, '  charset="UTF-8"\n', []),
  format(atms, '  fontsize="11"\n', []),
  format(
    atms,
    '  label="ATMS: ~w  Explanation: ~w"\n',
    [ATMSID, Explanation]
  ),
  format(atms, '  overlap=false\n', []),
  format(atms, '}\n', []),
  close(atms).
