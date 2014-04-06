:- module(el_app, []).

/** <module> Energy labels application

A simple application to showcase what can be done with
the LOD version of the dataset of energy labels.

@author Wouter Beek
@see Uses Tipsy for labels
     http://onehackoranother.com/projects/jquery/tipsy/
@see Used Ultimate CSS Gradient Generator for background gradient
     http://www.colorzilla.com/gradient-editor/
@version 2013/10-2013/12, 2014/03
*/

:- use_module(el(el_script)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(row_ext)).
:- use_module(html(html_form)).
:- use_module(html(html_table)).
:- use_module(library(apply)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/js_write)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(settings)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(server(app_ui)).
:- use_module(server(web_modules)).
:- use_module(sparql(sparql_build)).
:- use_module(sparql(sparql_db)).
:- use_module(sparql(sparql_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(el,
    'https://data.overheid.nl/data/dataset/energielabels-agentschap-nl/').

:- http_handler(root(el_app), el_app, []).

user:web_module('EnergyLabels', el_app).

:- sparql_register_remote(el, 'lod.cedar-project.nl', 8080, '/sparql/pilod').

% /css
user:file_search_path(css, el(css)).
:- html_resource(css('el.css'), []).
:- html_resource(css('tipsy.css'), []).

% /js
user:file_search_path(js, el(js)).
:- html_resource(js('jsquery.min.js'), []).
:- html_resource(js('jquery.tipsy.js'), [requires(js('jsquery.min.js'))]).



el_app(Request):-
  http_parameters(
    Request,
    [
      house_number(HouseNumber, [default(no_house_number)]),
      house_number_addition(
        HouseNumberAddition,
        [default(no_house_number_addition)]
      ),
      postcode(Postcode, [default(no_postcode)])
    ]
  ),
  reply_html_page(
    app_style,
    \el_head,
    \el_body(Postcode, HouseNumber, HouseNumberAddition)
  ).

el_body(Postcode, HouseNumber, HouseNumberAddition) -->
  {http_absolute_location(root(el), URL, [])},
  html([
    \submission_form(
      URL,
      [
        fieldset(class='pure-group', [
          input([
            class='pure-input-1-2',
            name=postcode,
            placeholder='Postcode',
            type=text
          ]),
          input([
            class='pure-input-1-2',
            name=house_number,
            placeholder='House number',
            type=text
          ]),
          input([
            class='pure-input-1-2',
            name=house_number_addition,
            placeholder='House number addition',
            type=text
          ])
        ]),
        \submit_button
      ]
    ),
    \el_content(Postcode, HouseNumber, HouseNumberAddition)
  ]).

el_content(Postcode, HouseNumber, _HouseNumberAddition) -->
  {(
    Postcode == no_postcode
  ;
    HouseNumber == no_house_number
  )}, !.
el_content(Postcode, HouseNumber, HouseNumberAddition) -->
  {
    el_index(Postcode, HouseNumber, HouseNumberAddition, Building),
    sub_atom(Postcode, 0, 5, _, PostcodePrefix),
    el_indexes(PostcodePrefix, Ls1),
    nth0(I, Ls1, Building-_),
    length(Ls1, NumberOfDataItems),
    pairs_values(Ls1, Ls2),
    WItem = 10,
    WBar = 600,
    format(atom(WidthStyle), 'width: ~wpx;', [WBar]),
    WBarMinus is WBar - WItem
  },
  html([
    \html_table(
      [header_row(true),highlighted_rows([I]),indexed(true)],
      html('SPARQL results'),
      [['Postcode','Number','Addition','Prestation index']|Ls2]
    ),
    \html_requires(css('el.css')),
    \html_requires(css('tipsy.css')),
    \html_requires(js('jquery.tipsy.js')),
    div(class=rainbow, [
      \js_script({|javascript(_)||
        "use strict";
        window.onload=function(){$(".data_item").tipsy({gravity: 'n'});};
      |}),
      div(
        [class=rb,style=WidthStyle],
        \data_items(0-NumberOfDataItems, WBarMinus, WItem, Ls2)
      )
    ])
  ]).

data_items(_I-_N, _WBar, _WItem, []) --> !.
data_items(I1-N, WBar, WItem, [H|T]) -->
  {Left is (I1 * WBar) / (N - 1)},
  data_item(Left, WItem, H),
  {I2 is I1 + 1},
  data_items(I2-N, WBar, WItem, T).

data_item(Left, Width, H) -->
  {
    format(atom(Style), 'left: ~wpx; width: ~wpx;', [Left,Width]),
    data_item_label(H, Label)
  },
  html(div([class=data_item,'original-title'=Label,style=Style], [])).

data_item_label(
  [Postcode1,HouseNumber1,HouseNumberAddition1,PrestationIndex1],
  Label
):-
  rdf_literal(Postcode1,        Postcode2,        _, _),
  rdf_literal(HouseNumber1,     HouseNumber2,     _, _),
  rdf_literal(PrestationIndex1, PrestationIndex2, _, _),
  (
    HouseNumberAddition1 == '$null$'
  ->
    format(atom(Label), '~w-~w ~w', [Postcode2,HouseNumber2,PrestationIndex2])
  ;
    rdf_literal(HouseNumberAddition1, HouseNumberAddition2, _, _),
    format(
      atom(Label),
      '~w-~w~w ~w',
      [Postcode2,HouseNumber2,HouseNumberAddition2,PrestationIndex2]
    )
  ).

el_head -->
  html(title('Energy Labels Demo App')).

el_index(Postcode, HouseNumber, HouseNumberAddition, Building):-
  phrase(
    sparql_formulate(
      _,
      'http://example.com/el',
      [el],
      select,
      true,
      [building],
      [
        rdf(var(building), a, el:'Building'),
        rdf(var(building), el:postcode, var(postcode)),
        filter(regex(var(postcode), at_start(Postcode))),
        rdf(var(building), el:huisnummer, integer(HouseNumber)),
        rdf(var(building), el:huisnummer_toevoeging, var(house_number_addition)),
        filter(strends(var(house_number_addition), string(HouseNumberAddition))),
        rdf(var(building), el:certificaat, var(certificate)),
        rdf(var(certificate), el:engergie_prestatieindex, var(index))
      ],
      inf,
      _,
      _
    ),
    Query
  ),
  sparql_query(el, Query, _VarNames, [row(Building)|_]).

el_indexes(PostcodePrefix, Ls2):-
  format(atom(Where3), 'filter regex(?postcode, "^~w") .', [PostcodePrefix]),
  phrase(
    sparql_formulate(
      _,
      'http://example.com/el',
      [el],
      select,
      true,
      [building,postcode,house_number,house_number_addition,index],
      [
        rdf(var(building), a, el:'Building'),
        rdf(var(building), el:postcode, var(postcode)),
        Where3,
        rdf(var(building), el:huisnummer, var(house_number)),
        optional([
          rdf(var(building), el:huisnummer_toevoeging, var(house_number_addition))
        ]),
        rdf(var(building), el:certificaat, var(certificaat)),
        rdf(var(certificaat), el:energie_prestatieindex, var(index))
      ],
      inf,
      _,
      asc-[index]
    ),
    Query
  ),
  sparql_query(el, Query, _VarNames, Rows),
  rows_to_lists(Rows, Ls1),
  maplist(to_pairs, Ls1, Ls2).

to_pairs([H|T], H-T).

% Already loaded in memory.
load_el_data:-
  rdf_graph(el), !.
% Already available on disk.
load_el_data:-
  absolute_file_name(
    output(el_1),
    File,
    [access(read),file_errors(fail),file_type(turtle)]
  ), !,
  rdf_load([format(turtle)], el, File).
% Has to be created.
load_el_data:-
  el_script,
  load_el_data.

