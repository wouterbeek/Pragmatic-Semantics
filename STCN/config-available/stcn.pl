:- module(conf_stcn, []).

/** <module> STCN

The STCN SW package.

@author Wouter Beek
@version 2013/04
*/

:- use_module(html(html)).
:- use_module(os(file_ext)).

user:file_search_path(kmc, stcn('KMC').

:-
  % Data files directory.
  absolute_file_name(stcn('Debug'), DebugDirectory),
  create_directory(DebugDirectory),
  assert(user:file_search_path(debug, stcn('Debug'))),
  
  % Standards-supporting data files.
  absolute_file_name(data('Standards'), StandardsDirectory),
  create_directory(StandardsDirectory),
  assert(user:file_search_path(data_standards, data('Standards'))),

  % STCN data files directory.
  absolute_file_name(data('STCN'), STCN_Directory),
  create_directory(STCN_Directory),
  assert(user:file_search_path(data_stcn, data('STCN'))),

  % Wordnet data files directory.
  absolute_file_name(data('Wordnet'), WordnetDirectory),
  create_directory(WordnetDirectory),
  assert(user:file_search_path(data_wordnet, data('Wordnet'))).

:- use_module(cliopatria(hooks)).
:- use_module(library(http/http_dispatch)).

:- multifile(http:location/3).
http:location(stcn, cliopatria(stcn), []).
:- http_handler(stcn(stcn_main), stcn_main, []).

cliopatria:menu_item(100=stcn/stcn_main, 'Dataset info').
cliopatria:menu_item(200=stcn/load_stcn, 'Load STCN').
cliopatria:menu_item(300=stcn/load_void, 'Load VoID').
cliopatria:menu_popup_order(stcn, 120).



stcn_main(_Request):-
  reply_html_file(cliopatria(default), stcn_main).
