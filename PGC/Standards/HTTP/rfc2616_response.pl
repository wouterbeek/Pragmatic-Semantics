:- module(
  rfc2616_response,
  [
    'Response'/4, % +Request:list
                  % +Status:between(100,999)
                  % +Headers:list(nvpair)
                  % +Body:list(code)
    'Response'//5 % -ParseTree:compound
                  % ?Version:compound
                  % ?Status:compound
                  % ?Headers:list(compound)
                  % ?Body:list(code)
  ]
).

/** <module> RFC 2616 response

DCG for RFC 2616 response.

@author Wouter Beek
@version 2013/12-2014/01
*/

:- use_module(dcg(parse_tree)).
:- use_module(generics(codes_ext)).
:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_generic_message)).
:- use_module(http(rfc2616_status_line)).
:- use_module(http_headers(rfc2616_entity_header)).
:- use_module(http_headers(rfc2616_general_header)).
:- use_module(http_headers(rfc2616_response_header)).
:- use_module(library(option)).



%! 'Response'(
%!   +Request:list,
%!   +Status:between(100,999),
%!   +Headers:list(nvpair),
%!   +Body:list(code)
%! ) is det.

'Response'(Request, Status, Headers1, Body):-
  memberchk(pool(client(_,_,_,Out)), Request),
  
  length(Body, Length),
  merge_options(Headers1, ['Content-Length'(Status,Length)], Headers2),
  
  phrase(
    'Response'(_, version(1,1), status(Status,_), Headers2, Body),
    Codes
  ),
  put_codes(user_output, Codes), flush_output(user_output), %DEB
  put_codes(Out, Codes).


%! 'Response'(
%!   -ParseTree:compound,
%!   ?Version:compound,
%!   ?Status:compound,
%!   ?MessageHeaders:list(pair),
%!   ?MessageBody:list(code)
%! )//
% After receiving and interpreting a request message,
%  a server responds with an HTTP response message.
%
% ~~~{.abnf}
% Response = Status-Line
%            *(( general-header | response-header | entity-header ) CRLF)
%            CRLF
%            [ message-body ]
% ~~~

'Response'(T0, Version, Status, Headers, Body) -->
  'Status-Line'(T1, Version, Status),
  headers(T2s, Headers),
  'CRLF',
  (
    'message-body'(T3, Body)
  ;
    "",
    {Body = []}
  ),
  {parse_tree(response, [T1,headers(T2s),T3], T0)}.

headers([], []) --> [].
headers([T1|Ts], [H|T]) -->
  'general-header'(T1, H),
  'CRLF',
  headers(Ts, T).
headers([T1|Ts], [H|T]) -->
  'response-header'(T1, H),
  'CRLF',
  headers(Ts, T).
headers([T1|Ts], [H|T]) -->
  'entity-header'(T1, H),
  'CRLF',
  headers(Ts, T).

