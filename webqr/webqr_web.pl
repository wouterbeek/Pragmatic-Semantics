:- module(webqr_web, []).

/** <module> WebQR Web-based front-end

Web-based front-end for WebQR.

The whole API consists of two URL address spaces,
used for posing REST requests:
  1. <webqr>/circle
  2. <webqr>/line

Specific circles and lines are identified by a unique integer
(unique within either of these URL address spaces), e.g.:
  1. <webqr>/circle/12
  2. <webqr>/line/8007

The kind of action performed depends on (a) the HTTP method
and (b) the URL address space. We now enumerate all combinations:
  1. DELETE circle and line:
  2. GET circle and line: give information back
  3. OPTIONS
  4. POST
    a. circle
    b. line
  5. PUT
    a. circle
    b. line


Performing a action in WebQR consists of the following steps (in that order):
  1. Check whether the current user has been initialized:
    a. has a user name,
    b. has a local graph,
    c. has a global graph,
    d. has a modeling language, and
    e. has prefered natural languages.
  2. Perform RESTful processing based on
    a. the HTTP method (either GET, DELETE, POST, or PUT),
    b. the request URL,
    c. the category of the request (either circle or line), and
    d. the user.
  3. Materialize the user's local and global graphs
     using RDF and RDFS deduction.

--

@author Wouter Beek
@author Sander Latour
@version 2013/03, 2013/07, 2013/09-2014/01
*/

:- use_remote_module(generics(codes_ext)).
:- use_remote_module(generics(db_ext)).
:- use_remote_module(http(rfc2616_response)).
:- use_module(library(apply)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/js_write)).
:- use_remote_module(owl(owl_read)).
:- use_remote_module(rdf(rdf_reification)).
:- use_remote_module(rdf_reasoning(rdf_mat)).
:- use_remote_module(rdfs(rdfs_label_ext)).
:- use_remote_module(server(app_ui)). % HTML application style.
:- use_remote_module(server(web_modules)).
:- use_remote_module(server(web_ui)). % JS generics.
:- use_remote_module(standards(json_ext)).
:- use_remote_module(webqr(rdf_tabular_webqr)).
:- use_remote_module(webqr(webqr_build)).
:- use_remote_module(webqr(webqr_generic)).
:- use_remote_module(xml(xml_namespace)).

:- xml_register_namespace(webqr, 'http://www.wouterbeek.com/webqr#').

% /webqr
http:location(webqr, root(webqr), []).
:- http_handler(root(webqr), webqr_web, [prefix]).
user:web_module('WebQR', webqr_web).

% /circle
:- http_handler(webqr(circle), rest_process_circle, [prefix]).

% /line
:- http_handler(webqr(line), rest_process_line, [prefix]).

% /css
user:file_search_path(css, webqr(css)).
:- if(predicate_property(user:debug_mode, visible)).
    :- html_resource(
      css('webqr.css'),
      [requires([css('bootstrap.css'), css('app_ui.css')])]
    ).
:- else.
    :- html_resource(css('webqr.css'), [requires([css('bootstrap.min.css')])]).
:- endif.

% /js
user:file_search_path(js, webqr(js)).
% d3
:- if(predicate_property(user:debug_mode, visible)).
  :- html_resource(
    js('bootstrap-debug-3.0.3.js'),
    [requires(js('jquery-debug-2.0.3.js'))]
  ).
  :- html_resource(
    js('d3-debug-3.3.11.js'),
    [requires(js('jquery-debug-2.0.3.js'))]
  ).
  :- html_resource(js('webqr.js'), [
    requires([
      js('bootstrap-debug-3.0.3.js'),
      js('d3-debug-3.3.11.js'),
      js('jquery-debug-2.0.3.js')
    ])
  ]).
:- else.
  :- html_resource(
    js('bootstrap-min-3.0.3.js'),
    [requires(js('jquery-min-2.0.3.js'))]
  ).
  :- html_resource(
    js('d3-min-3.3.11.js'),
    [requires(js('jquery-min-2.0.3.js'))]
  ).
  :- html_resource(js('webqr.js'), [
    requires([
      js('bootstrap-min-3.0.3.js'),
      js('d3-min-3.3.11.js'),
      js('jquery-min-2.0.3.js')
    ])
  ]).
:- endif.



% WEBSITE %

webqr_web(_Request):-
  reply_html_page(app_style, \webqr_head, \webqr_body).

webqr_body -->
  {
    maplist(webqr_endpoint, [circle,line], [CirclePath,LinePath]),
    prolog_to_json(
      json([api=json([endpoints=json([circle=CirclePath,line=LinePath])])]),
      InitDict
    ),
    http_location_by_id(rdf_tabular_webqr, Location),
    http_absolute_uri(Location, URI)
  },
  html([
    div(style='float:right;', a(href=URI, 'WebQR Tabular')),
    \html_requires(js('generics.js')),
    \html_requires(css('webqr.css')),
    \html_requires(js('webqr.js')),
    \js_script({|javascript(InitDict)||
      App.init(InitDict);
    |})
  ]).

webqr_head -->
  html(title('WebQR')).



% REST PROCESSING %

%! rest_process_circle(+Request:list) is det.
% HTTP handler for HTTP requests at `/webqr/circle`.

rest_process_circle(Request):-
  rest_process(Request, circle).

%! rest_process_line(+Request:list) is det.
% HTTP handler for HTTP requests at `/webqr/line`.

rest_process_line(Request):-
  rest_process(Request, line).

%! rest_process(+Request:list, +Category:oneof([circle,line])) is det.
% REST processing predicate for both circle and lines (i.e. `Category`).
%
% This sets:
%   1. User name / RDF graph name
%      If the user name is not given explicitly,
%      we use the HTTP session ID instead.
%   2. QR modeling language
%   3. Preferred natural languages (interpreted as a total order
%      from highest to lowest).
%
% @tbd Add user names that are not session IDs (extend [user_db]).
% @tbd Use HTTP `Accept-Languages` header for natural language preferences.

rest_process(Request, Category):-
  % Prepare
  memberchk(method(Method), Request),
  request_to_user_name(Request, User),
  webqr_create_model(User),
  request_to_local_uri(Request, LocalURI),

  % REST
  rest_process(Request, Method, Category, User, LocalURI).

/*
  % Materialize
  webqr_global_graph(User, GlobalGraph),
  webqr_local_graph(User, LocalGraph),
  maplist(
    materialize(
      [entailment_regimes([rdf,rdfs]),multiple_justifications(false)]
    ),
    [GlobalGraph,LocalGraph]
  ).
*/



% REST PROCESSING: HTTP METHODS %

adaptation(_, 200).

%! rest_process(
%!   +Request:list,
%!   +HTTP_Method:oneof([]),
%!   +Category:oneof([circle,line]),
%!   +User:atom,
%!   +LocalURI:url
%! ) is det.
% Different clauses treat different HTTP methods.

% DELETE 204 (No Content).
% The resource was deleted on the server.
%
% GET 404 (Not Found).
% The resource does not exist on the server.
rest_process(Request, delete, _, User, LocalURI):-
  webqr_exists(Request, User, LocalURI),
  webqr_remove_resource(User, LocalURI),
adaptation(204, Code),
  reply_json(json{}, [status(Code)]).


% GET 200 (OK).
% The resource exists, so its global IRI is returned.
%
% GET 404 (Not Found).
% The resource does not exist on the server.
rest_process(Request, get, _, User, LocalURI):-
  webqr_exists(Request, User, LocalURI),
  webqr_local_graph(User, LocalGraph),
  owl_resource_identity(LocalURI, GlobalURI, LocalGraph), !,
  reply_json(json{global_uri:GlobalURI}).
% GET 500 (Internal Server Error).
% The resource exists on the server,
%  but it has no global IRI associated with it.
% This should not be possible.
rest_process(_, get, _, User, LocalURI):-
  format(
    atom(Msg),
    'Resource [user:~w,local-uri:~w] exists but has no global counterpart.',
    [User,LocalURI]
  ),
  throw(http_reply(server_error(Msg))).


% OPTIONS 200 (OK).
% The `Allow` header is replied.
%
% OPTIONS 404 (Not Found).
% The resource does not exist on the server.
rest_process(Request, options, Category, User, LocalURI):-
  webqr_exists(Request, User, LocalURI),

  webqr_local_graph(User, LocalGraph),
  owl_resource_identity(LocalURI, _, LocalGraph),

  http_supported_methods(Category, Methods),

  'Response'(Request, 200, ['Allow'(Methods)], []).


% POST circle 201 (Created).
% The circle is created on the server.
%
% POST circle 400 (Bad Request).
rest_process(Request, post, circle, User, _LocalURI):- !,
  % Receive a local and a global URI, store these, connect these, etc.
  catch(
    (
      http_read_json_dict(Request, D),
      atom_string(LocalURI, D.local_uri),
      atom_string(GlobalURI1, D.global_uri)
    ),
    E,
    throw(http_reply(bad_request(E)))
  ),

  webqr_create_circle(User, LocalURI, GlobalURI1, GlobalURI2),

  % HTTP status 201 for a new `Created` resource.
  % Include the new resource's global URI in the `Location` reply header.
  user_pref_label(User, GlobalURI2, Label),
adaptation(201, Code),
  'JSON_Response'(Request, Code, ['Location'(GlobalURI2)], json{label:Label}).


% POST line 200 (OK).
% Information is sent about the relations that can be added.
%
% POST line 201 (Created).
% The given relation is added.
% The `Location` header gives the proposition that states the relation.
%
% POST line 400 (Bad Request).
% The request does not include the from and to circles correctly,
%
% POST line 404 (Not Found).
% If a circle or line's local URI cannot be found on the server.
rest_process(Request, post, line, User, _LocalURI):- !,
  % Retrieve the from and to local URIs.
  catch(
    (
      http_read_json_dict(Request, D),
      atom_string(FromLocalURI, D.from_local_uri),
      atom_string(ToLocalURI, D.to_local_uri)
    ),
    E,
    throw(http_reply(bad_request(E)))
  ),

  maplist(webqr_exists(Request, User), [FromLocalURI,ToLocalURI]),

  (
    catch(atom_string(RelationType, D.relation), _, fail)
  ->
    % A relation type is given in the request, so make a relational assertion.
    webqr_create_line(User, FromLocalURI, RelationType, ToLocalURI),

    % HTTP status code 201 for a new `Created` resource.
    % Include the newly asserted proposition in the reply header.
    webqr_local_graph(User, LocalGraph),
    rdf_assert_statement(
      FromLocalURI,
      RelationType,
      ToLocalURI,
      LocalGraph,
      Stmt
    ),
adaptation(201, Code),
    'JSON_Response'(Request, Code, ['Location'(Stmt)], json{})
  ;
    % Find the relations that can be placed between the given circles.
    webqr_possible_relations(
      [graph(User),modeling_language(qsim)],
      FromLocalURI,
      ToLocalURI,
      Relations
    ),

    % Find matching labels for the relations.
    findall(
      Dict,
      (
        member(Relation, Relations),
        user_pref_label(User, Relation, Label),
        dict_pairs(Dict, json, [label-Label,resource-Relation])
      ),
      Dicts
    ),

    % Reply with suggestions for relations between the two circles.
    reply_json(json{relations:Dicts})
  ).

% PUT circle 204 (No Content)
% The existing circle is changed.
%
% PUT circle 404 (Not Found)
% The local URI does not exist on the server side.
%
% PUT circle 400 (Bad Request)
rest_process(Request, put, circle, User, LocalURI):-
  % Notice that the local URI is not sent as data (as in a `POST` method),
  % but is the URI the HTTP request was sent to.
  webqr_exists(Request, User, LocalURI),

  % The global URI (i.e. concept name) is part of the data.
  catch(
    (
      http_read_json_dict(Request, D),
      atom_string(GlobalURI1, D.global_uri)
    ),
    E,
    throw(http_reply(bad_request(E)))
  ),

  webqr_update_circle(User, LocalURI, GlobalURI1, GlobalURI2),

  % Status code 204 for `No content`,
  %  since no new circle is created (only altered).
  user_pref_label(User, GlobalURI2, Label),
adaptation(204, Code),
  reply_json(json([label=Label]), [status(Code)]).



% HELPERS %

http_supported_methods(circle, [delete,get,options,put,post]).
http_supported_methods(line, [delete,get,options,put,post]).

%! request_to_local_uri(+Request:list, -LocalURI:url) is det.
% Identifies the resource that is indicated by the URL path.

request_to_local_uri(Request, LocalURI):-
  % As explained in `library(http/http_header)`,
  % the content of the `request_uri` is parsed into `path` and `search`.
  memberchk(path(Path), Request),
  http_absolute_uri(Path, LocalURI).

%! webqr_endpoint(+Category:oneof([circle,line]), -IRI:iri) is det.
% Returns the absolute URI that is the endpoint for
% objects of the given category.

webqr_endpoint(Category, URI):-
  atomic_list_concat([rest_process,Category], '_', Pred),
  http_location_by_id(Pred, Location),
  http_absolute_uri(Location, URI).

