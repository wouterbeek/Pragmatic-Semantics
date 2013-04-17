:- module(
  diagnosis_character,
  [
    ask_question_vc/1, % +Question:point_cloud
    locate_culprit_vc/1 % +SupportExpression:support_expression
  ]
).

/** <module> Diagnosis Virtual Characters

Virtual Characters for diagnosis and repair.

@author Wouter Beek
@version Apr 2012
*/

:- use_module(ccm(ccm_api)).
:- use_module(ccm(ccm_verb)).
:- use_module(diagnosis(diagnosis)).
:- use_module(library(semweb/rdf_db)).
:- use_module(generic(file_ext)).
:- use_module(generic(os_ext)).



:- pce_begin_class(diagnosisCharacter, virtualCharacter).

initialise(This,
  Message : message = any,
  VirtualCharacters : virtualCharacters = [virtualCharacters]
):->
  send_super(This, initialise,
    message := Message,
    virtualCharacters := VirtualCharacters
  ).

:- pce_end_class(diagnosisCharacter).



% [TODO] Eventually the question is to be extracted from the
% diagnosis object. Multiple questions may be registered and
% the best question can be chosen intelligently etc. Right
% now this works with the hamsters, using the dedicted RDF
% graph =|probe_vc|=.
ask_question_vc(_Diagnosis):-
  % Write.
  open('probe_vc.tmp', write, Stream),
  % Make sure the aliases are included for the value URIs as well.
  rdf_current_namespaces(probe_vc, Aliases),
  rdf_save(stream(Stream), [graph(probe_vc), namespaces(Aliases)]),
  close(Stream),
  rdf_retractall(_Subject, _Predicate, _Object, probe_vc),

  % Read.
  open('probe_vc.tmp', read, Stream2),
  read_stream_to_codes(Stream2, Codes),
  atom_codes(Output, Codes),
  close(Stream2),
  delete_file('probe_vc.tmp'),

  % Complement the message.
  atom_concat(Output, '\0', EndOfMessage),
  get(@app?currentModel, learningSpaceNumber, LearningSpace),
  get(@app?currentModel, name, ModelName),
  get(@app?applicationWindow?activeContent?simulation?scenario, name,
    ScenarioName
  ),
  format(
    atom(Message),
    '<request type="diagnosis_probe" model="~w" scenario="~w" ls="~w"/>\n~w',
    [ModelName, ScenarioName, LearningSpace, EndOfMessage]
  ),
  communicate_message_vc(Message).

locate_culprit_vc(SupportExpression):-
  locate_culprit(SupportExpression, FragmentIndividuals, Sentence),
  locate_culprit_vc1(SupportExpression, FragmentIndividuals, Sentence).

% This is for diagnosis results.
locate_culprit_vc1(_SupportExpression, [FragmentIndividual], Sentence):-
  fragment_individual_to_highlight_atom(FragmentIndividual, HighlightAtom),
  get(@app?currentModel, learningSpaceNumber, LearningSpace),
  get(@app?currentModel, name, ModelName),
  format(
    atom(Message),
    '<request type="diagnosis_culprit" model="~w" ls="~w">\n<task>\n  <text>~w</text>\n~w\n</task>\n</request>\0',
    [ModelName, LearningSpace, Sentence, HighlightAtom]
  ),
  communicate_message_vc(Message).
% This is for repair results.
locate_culprit_vc1(SupportExpression, FragmentIndividuals, _Sentence):-
  expression_definition_expression(
    SupportExpressionDefinition,
    SupportExpression
  ),
  expression_definition_abbreviation(
    SupportExpressionDefinition,
    SupportExpressionDefinitionLabel
  ),
  maplist(
    fragment_individual_to_highlight_atom,
    FragmentIndividuals,
    [FromQuantityHighlightAtom, ToQuantityHighlightAtom]
  ),
  FragmentIndividuals = [A, B],
  get(A, name, FromQuantityName),
  get(B, name, ToQuantityName),
  % Create the message.
  get(@app?currentModel, learningSpaceNumber, LearningSpace),
  get(@app?currentModel, name, ModelName),
  format(
    atom(Message),
    '<request type="diagnosis_repair" model="~w" ls="~w">\n<task>\n  <text>Add ~w between ~w</text>\n~w\n</task>\n<task>\n  <text>and ~w.</text>\n~w\n</task>\n</request>\0',
    [
      ModelName,
      LearningSpace,
      SupportExpressionDefinitionLabel,
      FromQuantityName,
      FromQuantityHighlightAtom,
      ToQuantityName,
      ToQuantityHighlightAtom
    ]
  ),
  communicate_message_vc(Message).

communicate_message_vc(Message):-
  debug(dui, '~w', [Message]),
  communicate_message_vc1(Message).

communicate_message_vc1(Message):-
  send(@app, slot, loading, diagnosis),
  send(@app, slot, loadingMessage, Message),
  get(@app, virtualCharacters, _VirtualCharacters).

fragment_individual_to_highlight_atom(FragmentIndividual, HighlightAtom):-
  fragment_indiviual_to_coordinates(FragmentIndividual, X1, Y1, X2, Y2),
  format(
    atom(HighlightAtom),
    '  <highlight x1="~w" y1="~w" x2="~w" y2="~w"/>',
    [X1, Y1, X2, Y2]
  ).

fragment_indiviual_to_coordinates(FragmentIndividual, X1, Y1, X2, Y2):-
  get(FragmentIndividual, fragmentElement, FragmentElement),
  get(FragmentElement, hypered, fragmentElement, VisualElement),

  get(VisualElement?display_position, x, X1),
  get(VisualElement?display_position, y, TempY1),

  get(VisualElement, left_side, RelativeX1),
  get(VisualElement, right_side, RelativeX2),
  DeltaX is RelativeX2 - RelativeX1,
  get(VisualElement, top_side, RelativeY1),
  get(VisualElement, bottom_side, RelativeY2),
  DeltaY is RelativeY2 - RelativeY1,

  X2 is X1 + DeltaX,
  TempY2 is TempY1 + DeltaY,

  (
    is_mac
  ->
    mac_y_pixel(TempY1, Y1),
    mac_y_pixel(TempY2, Y2)
  ;
    Y1 = TempY1,
    Y2 = TempY2
  ).
