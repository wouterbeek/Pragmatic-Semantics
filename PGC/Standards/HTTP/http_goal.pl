:- module(
  http_goal,
  [
    http_exception/1, % +Exception:compound
    http_goal/3, % +URL:atom
                 % +Options:list(nvpair)
                 % :Goal
    http_goal/4 % +URL:atom
                % +Options:list(nvpair)
                % :Goal
                % +Attempts:or([integer,oneof([inf])])
  ]
).

/** <module> HTTP goal

Execute HTTP goals while catching any exceptions and retrying
until the goal succeeds or fails.

@author Wouter Beek
@version 2013/11, 2014/01, 2014/04
*/

:- use_remote_module(generics(atom_ext)).
:- use_remote_module(http(rfc2616_status_line)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(option)).
:- use_remote_module(math(math_ext)).

:- meta_predicate(http_goal(+,+,1)).
:- meta_predicate(http_goal(+,+,1,+)).
:- meta_predicate(http_catcher(+,+,+,1,+)).
:- meta_predicate(http_process(+,+,1)).



cert_verify(_, _, _, _, _):- !.


%! http_goal(+URL:atom, +Options:list(nvpair), :Goal) is det.
%! http_goal(
%!   +URL:atom,
%!   +Options:list(nvpair),
%!   :Goal,
%!   +Attempts:or([integer,oneof([inf])])
%! ) is det.
% Executes the given goal on the content found at the given URL.
% Always succeeds.
%
% The number of attempts is the number of HTTP-related exceptions
%  that can be encountered before the predicate gives up.
%
% The arguments of `Goal` are appended with the argument `Stream`.
%
% The following options are supported:
%   * =|never_give_up(+NeverGiveUp:boolean)|=
%     Never give up upon receiving an HTTP 5xx status code.
%     Default: `false`.
%   * =|nocatch(+DoNotCatchExceptions:boolean)|=
%     When set to `true` exceptions are not caught
%     and no automated retrying occurs.
%     Default: `false`.
% Other options are given to http_open/3.

http_goal(URL, O1, Goal):-
  option(nocatch(true), O1, false), !,
  merge_options(
    [cert_verify_hook(cert_verify),status_code(Status),timeout(10)],
    O1,
    O2
  ),
  setup_call_cleanup(
    http_open(URL, Stream, O2),
    http_process(Status, Stream, Goal),
    close(Stream)
  ).
http_goal(URL, O1, Goal):-
  http_goal(URL, O1, Goal, 10).
http_goal(URL, O1, Goal, Attempts):-
  merge_options(
    [cert_verify_hook(cert_verify),status_code(Status),timeout(10)],
    O1,
    O2
  ),
  catch(
    setup_call_cleanup(
      http_open(URL, Stream, O2),
      http_process(Status, Stream, Goal),
      close(Stream)
    ),
    E,
    http_catcher(E, URL, O2, Goal, Attempts)
  ).


% Succeed.
http_catcher(exit, URL, _, Goal, _):- !,
  term_to_atom(Goal, Atom1),
  atom_truncate(Atom1, 120, Atom2),
  debug(http_low, 'Successfully performed goal ~w on URL ~w.', [Atom2,URL]).
% Permanently fail to receive resource over HTTP.
http_catcher(E, URL, O1, Goal, 0):- !,
  http_exception(E),
  (
    E = error(http_status(Status),_),
    between(500, 599, Status),
    option(never_give_up(true), O1, false)
  ->
    sleep(1),
    http_goal(URL, O1, Goal)
  ;
    fail
  ).
% Incidental fail: retry.
http_catcher(_, URL, O1, Goal, Attempts1):-
  count_down(Attempts1, Attempts2),
  http_goal(URL, O1, Goal, Attempts2).


%! http_exception(+Exception:compound) is det.
% Handle exceptions thrown by http_open/3.

% Retry after a while upon existence error.
http_exception(error(existence_error(url, URL),Context)):- !,
  debug(high, 'URL ~w does not exist (context: ~w).', [URL,Context]).
% HTTP status code.
http_exception(error(http_status(Status),_Context)):- !,
  'Status-Code'(Status, Reason),
  debug(high, '[HTTP-STATUS] ~d ~w', [Status,Reason]).
% Retry upon I/O error.
http_exception(error(io_error(read,_Stream),context(_Predicate,Reason))):- !,
  debug(high, '[IO-ERROR] ~w', [Reason]).
http_exception(
  error(permission_error(redirect,http,URL),context(_,Reason))
):- !,
  debug(high, '[PERMISSION-ERROR] ~w (reason: ~w)', [URL,Reason]).
% Retry upon socket error.
% Thrown by http_open/3.
http_exception(error(socket_error(Reason),_)):- !,
  debug(high, '[SOCKET-ERROR] ~w', [Reason]).
% `Mode` is either `read` or `write`.
http_exception(
  error(timeout_error(Mode,_Stream),context(PredSignature,_))
):- !,
  debug(high, '[TIMEOUT-ERROR] While ~wing ~w.', [Mode,PredSignature]).
% DEB
http_exception(E):-
gtrace, %DEB
  debug(high, '[UNRECOGNIZED-EXCEPTION] ~w', [E]).

% Success codes.
http_process(Status, Stream, Goal):-
  between(200, 299, Status), !,
  call(Goal, Stream).
% Non-success codes.
http_process(Status, _, _):-
  % The catcher has to make a new attempt (if there are any attempts left).
  throw(error(http_status(Status),_Context)).

