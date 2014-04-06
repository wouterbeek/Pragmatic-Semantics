:- module(
  authorization,
  [
    authorized/3 % +Method:oneof([delete,get,post])
                 % +Request:list
                 % -User:atom
  ]
).

/** <module> Authorization

@author Torbjörn Lager
@author Jan Wielemaker
@author Wouter Beek
@see This code was originally taken from SWAPP:
     http://www.swi-prolog.org/git/contrib/SWAPP.git
@version 2009, 2013/10-2013/12
*/

:- use_module(library(http/http_authenticate)).
:- use_module(library(lists)).
:- use_remote_module(server(login_db)).
:- use_remote_module(server(password_db)).
:- use_remote_module(server(user_db)).

%! allow(
%!   ?Role:oneof([admin]),
%!   ?User:atom,
%!   ?Method:oneof([delete,get,post]),
%!   ?Path:atom,
%!   ?Request:list
%! ) is nondet.

:- dynamic(allow/5).
:- multifile(allow/5).

%! deny(
%!   ?Role:oneof([admin]),
%!   ?User:atom,
%!   ?Method:oneof([delete,get,post]),
%!   ?Path:atom,
%!   ?Request:list
%! ) is nondet.

:- dynamic(deny/5).
:- multifile(deny/5).



%! authorized(
%!   +Method:oneof([delete,get,post]),
%!   +Request:list,
%!   -User:atom
%! ) is semidet.

authorized(Method, Request, User):-
  memberchk(path(Path), Request),
  password_db_file_unix(File),
  (
    http_authenticate(basic(File), Request, [User|_Fields]), !
  ;
    logged_in(User), !
  ;
    User = anonymous
  ),
  (
    user(User, Properties),
    memberchk(roles(Roles), Properties),
    member(Role, Roles),
    (
      prolog:allow(Role, User, Method, Path, Request)
    ->
      (
        deny(Role, User, Method, Path, Request)
      ->
        throw(http_reply(authorise(basic, 'secure')))
      ;
        Done = true
      )
    ;
      throw(http_reply(authorise(basic, 'secure')))
    ),
    Done == true, !
  ;
    % Deny is the default.
    throw(http_reply(authorise(basic, 'secure')))
  ).

:- multifile(prolog:allow/5).
prolog:allow(_,     _, _,      '/rdf/db',           _).
prolog:allow(_,     _, _,      '/session/db',       _).
prolog:allow(_,     _, _,      '/session/eq',       _).
prolog:allow(admin, _, get,    '/statistics', _).

% Deny if user is using a Safari browser
% deny(_, _, _, _, Request) :-
%   memberchk(user_agent(UserAgent), Request),
%   sub_atom(UserAgent, _, _, _, 'Safari').

% Deny if too late in the night (server-side)
% deny(_, _, _, _, Request) :-
%   get_time(TimeStamp),
%   stamp_date_time(TimeStamp, DateTime, local),
%   date_time_value(hour, DateTime, Hour),
%   Hour >= 22.

% Deny if too much content
% deny(_, _, Method, _, Request) :-
%   memberchk(Method, [put, post]),
%   memberchk(content_length(Bytes), Request),
%   Bytes > 1000.

% Deny if no quota
% :- dynamic quota/2.
% deny(_, User, Method, Path, _) :-
%   (   quota(User, N)
%   ->  (   N > 0
%     ->  N1 is N - 1,
%       retractall(quota(User, _)),
%       assert(quota(User, N1)),
%       fail
%     ;   true
%     )
%   ;   assert(quota(User, 4)),
%     fail
%   ).

