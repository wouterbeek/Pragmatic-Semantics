:- module(web_settings, []).

/** <module> Web-interface to settings

Sets and retrieves settings using JSON.

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@version 2009, 2013/10, 2013/12
*/

:- use_remote_module(generics(db_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(settings)).
:- use_remote_module(server(server_ext)).

:- http_handler(root(settings), dispatch, []).

:- multifile(prolog:allow/5).
prolog:allow(_,     _, get,  '/settings', _).
prolog:allow(admin, _, post, '/settings', _).



% POST
dispatch_method(post, Request, _):-
  http_parameters(Request, [module(Module,[]),setting(Setting,[])]),
  % Do not interpret the data according to the MIME type,
  % but store it directly to an atom.
  http_read_data(Request, Data, [to(atom)]),
  (
    % Convert the given value so it fits the setting's type.
    setting_property(Module:Setting, type(Type)),
    atom_to_value(Type, Data, Value),

    % A special handling of any thrown exceptions.
    catch(
      set_setting(Module:Setting, Value),
      E,
      true
    ),
    (
      var(E)
    ->
      save_settings('settings.db'),
      settings_all(Module, Setting, Json),
      reply_json(Json, [width(0)])
    ;
      message_to_string(E, Msg),
      reply_json(json([ok= @false,error=Msg]), [width(0)])
    )
  ;
    reply_json(
      json([ok= @false,error='No such module or setting.']),
      [width(0)]
    )
  ).
% GET
dispatch_method(get, Request, _):-
  http_parameters(
    Request,
    [module(Module,[default(_)]),setting(Setting,[default(_)])]
  ),
  (
    % Why double negation?
    \+ \+ current_setting(Module:Setting)
  ->
    settings_all(Module, Setting, JSON),
    reply_json(JSON, [width(0)])
  ;
    reply_json(
      json([ok= @false, error='No such module or setting.']),
      [width(0)]
    )
  ).

atom_to_value(Type, Data, Value):-
  atom_number(Data, Value),
  must_be(Type, Value), !.
atom_to_value(Type, Data, Data):-
  must_be(Type, Data).

%! settings_all(?Module:atom, ?Setting:atom, -JSON:list) is det.
% Returns all modules that contain settings in JSON.

settings_all(Module, Setting, json(JSON_List)):-
  findall(
    Module=JSON,
    (
      current_setting(Module:_),
      settings_to_json(Module, Setting, JSON)
    ),
    JSON_List
  ).

%! settings_to_json(+Module:atom, ?Setting:atom, -JSON:list) is det.
% Returns all settings in a given module in JSON.

settings_to_json(Module, Setting, json(JSON_List)):-
  findall(
    Setting=JSON,
    setting_to_json(Module, Setting, JSON),
    JSON_List
  ).

%! setting_to_json(+Module:atom, ?Setting:atom, -JSON:compound) is nondet.
% Returns the description of a setting.

setting_to_json(Module, Setting, JSON):-
  setting_property(Module:Setting, comment(Comment)),
  setting_property(Module:Setting, type(Type)),
  type_to_json(Type, JsonType),
  setting_property(Module:Setting, default(Default)),
  setting(Module:Setting, Value),
  JSON = json([comment=Comment,type=JsonType,default=Default,value=Value]).

% This is likely not complete
type_to_json(oneof(List), List):- !.
type_to_json(Type, Type).

