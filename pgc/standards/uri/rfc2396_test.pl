:- module(rfc2396_test, []).

/** <module> RFC 2396 test

@author Wouter Beek
@version 2013/07
*/

:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_remote_module(uri(rfc2396_dcg)).



:- begin_tests(rfc2396).

rfc2396_atom('https://www.example.com/a/b;g/c?q=aba#rr').
rfc2396_atom('https://11.22.33.44:5566/a/b;g/c?q=aba#rr').

rfc2396_term(http, 'www.example.com', [[a,b],[c,d],[e],[f]], 'q=aap', me).
rfc2396_term(
  https,
  authority(wouter,[44,55,33,11],7777),
  [[a,b],[c,d],[e],[f]],
  'q=aap',
  me
).

test(
  rfc2396_generate,
  [forall(rfc2396_term(Scheme, Authority, Path, Query, Fragment))]
):-
  once(
    phrase(
      rfc2396_uri_reference(Tree, Scheme, Authority, Path, Query, Fragment),
      Codes
    )
  ),
  atom_codes(URI, Codes),
  maplist(formatnl, [Tree,URI]).

test(rfc2396_parse, [forall(rfc2396_atom(URI))]):-
  atom_codes(URI, Codes),
  once(
    phrase(
      rfc2396_uri_reference(Tree, Scheme, Authority, Path, Query, Fragment),
      Codes
    )
  ),
  maplist(formatnl, [Tree, Scheme, Authority, Path, Query, Fragment]).

:- end_tests(rfc2396).
