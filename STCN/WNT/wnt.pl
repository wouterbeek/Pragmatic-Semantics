:- module(wnt, []).

/** <module> WNT

Woordenboek der Nederlandse Taal

## First query

~~~
http://gtb.inl.nl/iWDB/search?
  %5F%5Flzbc%5F%5F=1361390809819%2E61
  &actie=results
  &lemmodern=cabaalmaker
  &domein=0
  &wdb=wnt
  &conc=true
  &sensitive=false
  &xmlerror=true
~~~

## Second query

~~~
http://gtb.inl.nl/iWDB/search?
  actie=article
  &wdb=WNT
  &id=M012481.re.6
  &lemmodern=cabaalmaker

@author Wouter Beek
@version 2013/02, 2014/01, 2014/03
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(atom_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(picarta(picarta)).



entry(Term):-
  query(Term, DOM),
  dom_lemma(DOM, Lemma),
  dom_modern_lemma(DOM, ModernLemma),
  dom_word_type(DOM, WordType),
  dom_position(Previous, DOM, Next),
  findall(
    Sense,
    dom_sense(DOM, Sense),
    Senses0
  ),
  dcg_with_output_to(atom(Senses), list(pl_term, Senses0)),
  format(
    user,
    'Lemma: ~w\nModern lemma: ~w\nWord type: ~w\nPrevious: ~w\nNext: ~w\nSenses:\n~w\n',
    [Lemma, ModernLemma, WordType, Previous, Next, Senses]
  ).

dom_lemma(DOM, Lemma):-
  xpath(DOM, //span(@class=orth), SPAN_Lemma),
  xpath(SPAN_Lemma, /self(text), Lemma0),
  !,
  re([case(lower),in(atom),out(atom),q('+')], [letter], Lemma, Lemma0).

dom_modern_lemma(DOM, ModernLemma):-
  xpath(DOM, //p, P),
  xpath(P, span(@class=field), SPAN),
  xpath(SPAN, /self(text), 'Modern lemma: '),
  xpath(P, /self(text), ModernLemma0),
  !,
  atom_concat('Modern lemma: ', ModernLemma, ModernLemma0).

%% dom_position(-PreviousLemma:atom, +Term:list, -NextLemma:atom) is det.
% Returns the previous and the next entry in the WNT.

dom_position(PreviousLemma, DOM, NextLemma):-
  findall(
    HTML,
    (
      xpath(DOM, //div(@id=koppelingen), DIV),
      xpath(DIV, a(@href), HREF),
      wnt_html(HREF, HTML_Frames),
      xpath(HTML_Frames, //frameset/frame/frame, FRAME),
      xpath(FRAME, /self(@src), Src),
      wnt_html(Src, HTML)
    ),
    [Previous, Next]
  ),
  dom_lemma(Previous, PreviousLemma),
  dom_lemma(Next, NextLemma).

dom_sense(DOM, sense(SenseNumber, Definition, Quotes)):-
  xpath(DOM, //div(@class=sense), DIV_Sense),

  % Sense number
  xpath(DIV_Sense, //span(@class=sensenumber), SPAN_SenseNumber),
  xpath(SPAN_SenseNumber, /self(text), SenseNumber0),
  
  first_split(SenseNumber0, '.', SenseNumber),

  % Definition
  xpath(DIV_Sense, //span(@class=def), SPAN_Definition),
  xpath(SPAN_Definition, /self(text), Definition),

  % Citation
  xpath(DIV_Sense, //div(@class=citatenblock), DIV_Citations),
  findall(
    Quote,
    (
      xpath(DIV_Citations, //span(@class=q2), SPAN_Q2),
      xpath(SPAN_Q2, /self(text), Quote)
    ),
    Quotes
  ).

dom_word_type(DOM, WordType):-
  xpath(DOM, //div(@class=entry), DIV),
  xpath(DIV, p(@class=hangindent), P),
  xpath(P, /self(text), WordType0),
  !,
  atom_concat('Woordsoort: ', WordType, WordType0).

query(Term, HTML):-
  % First query.
  wnt_xml(
    [
      actie(results),
      conc(true),
      domein(0),
      lemmodern(Term),
      sensitive(false),
      xmlerror(true),
      wdb(wnt)
    ],
    XML
  ),
  XML = [element(results, _ResultsAttributes, [_Statistics | Results])],
  maplist(query2(Term), Results, [HTML | _]).

query2(Term, Result, HTML):-
  Result = element(result, ResultAttributes, _ResultContents),
  memberchk(id=ID, ResultAttributes),
  % Second query.
  wnt_html(
    [
      actie(article_content),
      id(ID),
      lemmodern(Term),
      wdb('WNT')
    ],
    HTML
  ).

wnt_host('gtb.inl.nl').

wnt_html(Search, HTML):-
  wnt_stream(Search, Stream),
  load_html_file(Stream, HTML).

wnt_path('/iWDB/search').

wnt_port(80).

wnt_protocol(http).

wnt_query(1, actie, results).
wnt_query(2, actie, article).
wnt_query(1, conc, true).
wnt_query(1, domein, 0).
wnt_query(2, id, _ID).
wnt_query(1, lemmodern, _Term).
wnt_query(1, sensitive, false).
wnt_query(1, xmlerror, true).
wnt_query(1, wdb, wnt).
wnt_query(2, wdb, 'WNT').

wnt_stream(Search, Stream):-
  is_list(Search), !,
  wnt_protocol(Protocol),
  wnt_host(Host),
  wnt_port(Port),
  wnt_path(Path),
  http_open(
    [
      scheme(Protocol),
      host(Host),
      port(Port),
      path(Path),
      search(Search)
    ],
    Stream,
    []
  ).
wnt_stream(HREF, Stream):-
  atom_concat('search?', SearchString, HREF),
  uri_query_components(SearchString, Search),
  wnt_stream(Search, Stream).

wnt_xml(Search, XML):-
  wnt_stream(Search, Stream),
  load_structure(Stream, XML, [dialect(xml), space(default)]).


