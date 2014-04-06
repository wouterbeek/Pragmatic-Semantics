:- module(doyle_web, []).

/** <module> Doyle web

Web-interface to Doyle's TMS.

@author Wouter Beek
@version 2013/05, 2014/01, 2014/03
*/

:- use_module(doyle(doyle)).
:- use_module(generics(db_ext)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(tms(tms)).

:- multifile(http:location/3).
http:location(tms, root(tms), []).
:- http_handler(tms(doyle), doyle_web, []).



doyle_web(_Request):-
  findall(
    [
      Node,
      SupportStatus,
      %SupportingJustifications,
      SupportingNodes,
      Antecedents,
      Foundations,
      %Ancestors,
      Consequences,
      AffectedConsequences,
      BelievedConsequences,
      Repercussions,
      BelievedRepercussions
    ],
    (
      node(TMS, Node),
      doyle:support_status(Node, SupportStatus),
      %doyle:supporting_justifications(Node, SupportingJustifications),
      doyle:supporting_nodes(Node, SupportingNodes),
      doyle:antecedents(Node, Antecedents),
      doyle:foundations(Node, Foundations),
      %doyle:ancestors(Node, Ancestors),
      doyle:consequences(Node, Consequences),
      doyle:affected_consequences(Node, AffectedConsequences),
      doyle:believed_consequences(Node, BelievedConsequences),
      doyle:repercussions(Node, Repercussions),
      doyle:believed_repercussions(Node, BelievedRepercussions)
    ),
    Rows
  ),
  reply_html_page(
    app_style,
    title('Doyle'),
    html(
      \html_table(
        [header_row(true)],
        html('Doyle\'s TMS overview'),
        [
          ['Node',
           'Support status',
           %'Supporting justification',
           'Supporting nodes',
           'Antecedents',
           'Foundations',
           %'Ancestors',
           'Consequences',
           'Affected consequences',
           'Believed consequences',
           'Repercussions',
           'Believed repercussions'
          ]|Rows]
      )
    )
  ).

