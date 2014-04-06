:- module(web_users, []).

/** <module> Web users

Web-interface for administration of users.

User management for Web applications.

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@version 2009, 2013/10-2013/12
*/

:- use_remote_module(generics(db_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/js_write)).
:- use_remote_module(server(app_ui)).
:- use_remote_module(server(password_db)).
:- use_remote_module(server(server_ext)).
:- use_remote_module(server(user_db)).
:- use_remote_module(server(web_ui)).
:- use_remote_module(server(web_users)).

:- db_add_novel(user:prolog_file_type(db, database)).

:- http_handler(root(users), dispatch, []).
:- http_handler(root(users_ui), users_ui, []).

:- multifile(profile:allow/5).
prolog:allow(admin, _, delete, '/users', _).
prolog:allow(_,     _, get,    '/users', _).
prolog:allow(admin, _, post,   '/users', _).



dispatch_method(post, Request, _):-
  http_parameters(Request, [user(Name,[]),password(Password,[])]),
  http_read_data(Request, OptionsAtom, [to(atom)]),
  catch(
    atom_to_term(OptionsAtom, Options, _Bindings),
    E,
    true
  ),
  (
    var(E)
  ->
    (
      \+ user(Name)
    ->
      % Make sure that the roles are being set.
      memberchk(roles(_Roles), Options),
      add_user(Name, Options),
      add_password(Name, Password),
      reply_json(json([ok= @true]), [width(0)])
    ;
      reply_json(json([error='Existing user']), [width(0)])
    )
  ;
    reply_json(json([error='Malformed option list']), [width(0)])
  ).
dispatch_method(delete, Request, _) :-
  http_parameters(Request, [user(Name,[])]),
  catch(
    (
      remove_user(Name),
      remove_password(Name)
    ),
    E,
    true
  ),
  (
    var(E)
  ->
    reply_json(json([ok= @true]), [width(0)])
  ;
    message_to_string(E, Msg),
    reply_json(json([error=Msg]), [width(0)])
  ).
% Returns the contents of the user file in JSON.
dispatch_method(get, Request, _):-
  http_parameters(Request, [user(User,[default('_')])]),
  (
    User == '_'
  ->
    % Return the properties for all users.
    list_users(_, List)
  ;
    % Return the properties for a specific user.
    list_users(User, List)
  ),
  reply_json(json(List), [width(0)]).

%! list_users(?User:atom, -List:list(nvpair)) is det.
% Returns pairs of users and their properties.

list_users(User, List) :-
  findall(
    User=Properties,
    (
      user(User),
      findall(
        Property,
        (
          user_property(User, Prop),
          term_to_atom(Prop, Property)
        ),
        Properties
      )
    ),
    List
  ).

users_ui(_Request):-
  reply_html_page(app_style, \users_ui_head, \users_ui_body).

users_ui_body -->
  html([
    h1('User administration'),
    form([onsubmit='return false;'], [
      \users,
      \settings,
      \statistics
    ]),
    div(id=response, [])
  ]).

users -->
  html(
    fieldset(class='pure-group', [
      legend('Users'),
      div(class='pure-control-group', [
        button([class='pure-button',onclick='postUser()'], 'POST'),
        label(code('/users?user=')),
        input([
          class='',
          id='user-post-user',
          name='user-post-user',
          size='10',
          spellcheck=false,
          type=text,
          value=aladdin
        ]),
        label([&(amp),code('password=')]),
        input([
          id='user-post-password',
          name='user-post-password',
          size='10',
          type=password,
          value=''
        ]),
        br([]),
        label(for='user-post-content','Content'),
        input([
          class=string,
          id='user-post-content',
          name='user-post-content',
          type=text,
          spellcheck=false,
          value='[roles([user])]'
        ]),
        \clear_button(
          ['user-post-user','user-post-password','user-post-content']
        )
      ]),
      div(class='pure-control-group', [
        button([class='pure-button',onclick='deleteUser()'], 'DELETE'),
        label(code('/users?user=')),
        input([
          id='user-delete-user',
          name=user2,
          size='10',
          spellcheck=false,
          type=text,
          value=aladdin
        ]),
        \clear_button(['user-delete-user'])
      ]),
      div(class='pure-control-group', [
        button([class='pure-button',onclick='getUsers()'], 'GET'),
        label(code('/users?user=')),
        input([
          id='user-get-user',
          name='user-get-user',
          size='10',
          spellcheck=false,
          type=text,
          value=aladdin
        ]),
        \clear_button(['user-get-user'])
      ])
    ])
  ).

settings -->
  html(
    fieldset(class='pure-group', [
      legend('Settings'),
      % POST
      div(class='pure-control-group', [
        button([class='pure-button',onclick='postSetting()'], 'POST'),
        label(code('/settings?module=')),
        input([
          id='setting-post-module',
          name='setting-post-module',
          size='10',
          spellcheck=false,
          type=text,
          value=''
        ]),
        label([&(amp),code('setting=')]),
        input([
          id='setting-post-setting',
          name='setting-post-setting',
          size='10',
          spellcheck=false,
          type=text,
          value=''
        ]),
        \clear_button([
          'setting-post-module',
          'setting-post-setting',
          'setting-post-content'
        ]),
        input([
          class=string,
          id='setting-post-content',
          name='setting-post-content',
          spellcheck=false,
          type=text,
          value=''
        ])
      ]),
      div(class='pure-control-group', [
        button([class='pure-button',onclick='getSetting()'], 'GET'),
        label(code('/settings?module=')),
        input([
          id='setting-get-module',
          name='setting-get-module',
          size='10',
          spellcheck=false,
          type=text,
          value=''
        ])
      ]),
      label(code('&setting=')),
      input([
        id='setting-get-setting',
        name='setting-get-setting',
        size='10',
        spellcheck=false,
        type=text,
        value=''
      ]),
      \clear_button(['setting-get-module','setting-get-setting'])
    ])
  ).

statistics -->
  html(
    fieldset(class='pure-group', [
      legend('Statistics'),
      div(class='pure-control-group', [
        button([class='pure-button',onclick='getStatistics()'], 'GET'),
        label(code('/statistics'))
      ])
    ])
  ).

users_ui_head -->
  html([
    \html_requires(js('generics.js')),
    \js_script({|javascript(_)||
      "use strict";
      function postUser() {
        postJSON(
          "/users?user="
              + $("#user-post-user").val()
              + "&password="
              + $("#user-post-password").val(),
          $("#user-post-content").val()
        );
      }
      function deleteUser() {
        deleteJSON("/users?user=" + $("#user-delete-user").val());
      }
      function getUsers() {
        getJSON("/users?user=" + $("#user-get-user").val());
      }
      function postSetting() {
        postJSON(
          "/settings?module="
              + $("#setting-post-module").val()
              + "&setting="
              + $("#setting-post-setting").val(),
          $("setting-post-content").val()
        );
      }
      function getSettings() {
        getJSON(
          "/settings?module="
              + $("#setting-get-module").val()
              + "&setting="
              + $("setting-get-setting").val()
        );
      }
      function getStatistics() {
        getJSON("/statistics");
      }
    |})
  ]).

