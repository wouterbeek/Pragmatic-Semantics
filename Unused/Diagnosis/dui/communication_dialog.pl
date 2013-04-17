:- module(communication_dialog, []).

/** <module> Communication Dialog

Communication Dialog

@author Wouter Beek
@version Mrt, Apr 2012
*/

:- use_module(dui(diagnosis_character)).
:- use_module(dui(diagnosis_dialog)).


:- pce_begin_class(communicationDialog, assistanceDialog).

initialise(This, Label, SupportExpressionMessagePairs):->
  get(@app, currentModel, Model),
  send_super(This, initialise,
    label := Label,
    model := Model,
    how_to := @on,
    what_is := @off,
    why := @off
  ),

  get(This, gapX, GapX),
  get(This, gapY, GapY),
  get(This, topY, TopY),

  new(MessageWindow, extended_list_browser(
    name := communicationWindow,
    label := 'Repair suggestions:',
    width := 80,
    height := 12
  )),
  send(This, display, MessageWindow, point(x := GapX, y:= TopY)),
  send(MessageWindow, select_message, message(This, showCulprit, @arg1)),
  load_messages(MessageWindow, SupportExpressionMessagePairs),

  send(This, updateSpacers),
  send(This, assign_accelerators),
  send(This, minimalSize, size(
    width := MessageWindow?right_side + GapX,
    height := MessageWindow?bottom_side + GapY
  )).

showCulprit(_This, DictItem : selection = dict_item):->
  get(DictItem, object, SupportExpression),
  locate_culprits([SupportExpression]).

onClose(This):->
  send(This, destroy).

onDestroy(This):->
  send(This, destroy).

onResize(This, Difference : difference = size):->
  send(This?communicationWindow_member, bottom_side,
    This?communicationWindow_member?bottom_side + Difference?height
  ),
  send(This?communicationWindow_member, right_side,
    This?communicationWindow_member?right_side + Difference?width
  ).

:- pce_end_class(communicationDialog).

%% load_messages(
%%   +MessageWindow:extended_list_browser,
%%   +SupportExpressionMessagePairsChain:list(expression/atom)
%% ) is det.
% Loads the given messages into the given list browser.
%
% @param MessageWindow An extended list browser.
% @param SupportExpressionMessagePairs A list of pairs of support
%        expressions and atoms.

load_messages(MessageWindow, SupportExpressionMessagePairs):-
  forall(
    member(SupportExpression/Message, SupportExpressionMessagePairs),
    (
      nth0(Index, SupportExpressionMessagePairs, SupportExpression/Message),
      new(DictItem, dict_item(
        key := Index,
        label := Message,
        object := SupportExpression
      )),
      send(MessageWindow?dict, insert, DictItem)
    )
  ).
