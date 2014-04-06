:- module(
  web_modules,
  [
    html_web_modules_list//0,
    web_modules/1 % -Pairs:ordset(pair)
  ]
).

/** <module> Web modules

Registration infrastructure for Web modules.

@author Wouter Beek
@version 2012/10, 2013/02-2013/06, 2013/11, 2014/01, 2014/03-2014/04
*/

:- use_remote_module(html(html)). % Meta-DCG.
:- use_remote_module(html(html_list)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).



%! html_web_modules_list// is det.
% Generates an HTML list of the currently registered Web modules.

html_web_modules_list -->
  {
    web_modules(Pairs1),
    findall(
      Location-Label,
      (
        member(Label-InternalName, Pairs1),
        http_location_by_id(InternalName, Location)
      ),
      Pairs2
    )
  },
  % The HTML DSL requires us to explicitly specify the parent module
  % for the DCG meta argument.
  html(\html_list([ordered(false)], html:html_link, Pairs2)).


%! web_module(?ExternalName:atom, ?InternalName:atom) is nondet.
% Modules that are currently registered with the web console.
% Only web modules can be sensibly registered, since the web console
% looks for =|_web|=-predicates exclusively. Web modules must be
% registered before their web methods can be accessed from the web
% console.
%
% @arg ExternalName The atomic name of a Prolog module for
%      intended for human consumption.
% @arg InternalName The atomic name of a Prolog module.
%      intended for internal use.

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).


%! web_modules(-Tuples:ordset(list(atom))) is det.
% Returns all modules that are currently registered with the web console.
%
% @arg Pairs A list of pairs of atomic modules name.
%      The first is the internal name, the second is the external name.

web_modules(Pairs):-
  aggregate_all(
    set(ExternalName-InternalName),
    user:web_module(ExternalName, InternalName),
    Pairs
  ).

