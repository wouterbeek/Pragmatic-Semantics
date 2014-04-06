:- module(
  api_keys,
  [
    current_api_key/3 % +Organization:atom
                      % +Service:atom
                      % -Key:atom
  ]
).

/** <module> API keys

Stores API keys per user.

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(db_ext)).
:- use_module(html(html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
%:- use_module(library(http/json_convert)).
:- use_module(library(persistency)).
:- use_module(server(app_ui)).
:- use_module(server(login_db)).
:- use_module(server(password_db)).
:- use_module(server(server_ext)).

%:- json_object api_key(
%  user:atom,
%  organization:atom,
%  service:atom,
%  key:atom
%) + [type=api_key].

:- http_handler(root(keys), dispatch, []).

:- multifile(prolog:allow/5).
prolog:allow(admin, _, get, '/keys', _).

:- db_add_novel(user:prolog_file_type(db, database)).

:- persistent(api_key(user:atom,organization:atom,service:atom,key:atom)).

:- initialization(init_api_keys_db).



api_keys -->
  {
    findall(
      [U,O,S,K],
      api_key(U, O, S, K),
      Rows
    )
  },
  html(
    \html_table(
      [header_row(true),indexed(true)],
      html('API keys'),
      [['User','Organization','Service','Key']|Rows]
    )
  ).

current_api_key(Organization, Service, Key):-
  logged_in(_, User, _), !,
  api_key(User, Organization, Service, Key).

/*
  http_absolute_uri(root(keys), URL),
  JSON_In = [organization=Organization,service=Service],
  http_post(
    URL,
    json(json(JSON_In)),
    JSON_Out1,
    [request_header('Content-Type'='application/json')]
  ),
  JSON_Out1 = json(JSON_Out2),
  json_to_prolog(JSON_Out2, json([keys=Keys])),
  memberchk(json([key=Key]), Keys).
*/
:- multifile(dispatch_method/3).
dispatch_method(post, Request, User):-
  password_db_file_unix(File),
  http_authenticate(basic(File), Request, [User|_]), !,
  http_read_json(Request, json(JSON_In)),
  memberchk(organization=Organization, JSON_In),
  memberchk(service=Service, JSON_In),
  api_key(User, Organization, Service, Key),
  JSON_Out = [keys=json([key=Key])],
  reply_json(json(JSON_Out)).
%dispatch_method(get, _, _):-
%  reply_html_page(app_style, title('API keys'), \api_keys).

init_api_keys_db:-
  absolute_file_name(
    project(api_keys),
    File,
    [access(write),file_type(database)]
  ),
  db_attach(File, []).

