:- module(
  rfc2616_content_length,
  [
    'Content-Length'//2 % -ParseTree:compound
                        % +ContentLength:compound
  ]
).

/** <module> RFC 2616 Content-Length

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_cardinal)).



%! 'Content-Length'(-ParseTree:compound, +ContentLength:compound)// .
%
% # Syntax
%
% ~~~{.anf}
% Content-Length = "Content-Length" ":" 1*DIGIT
% ~~~
%
% # Semantics
%
% The `Content-Length` entity-header field indicates the size of
%  the entity-body, in decimal number of `OCTET`s, sent to the recipient or,
%  in the case of the `HEAD` method, the size of the entity-body that
%  would have been sent had the request been a `GET`.
%
% # Pragmatics
%
% Applications SHOULD use this field to indicate the transfer-length of
%  the message-body, unless this is prohibited by the rules in section 4.4.
%
% Any `Content-Length` greater than or equal to zero is a valid value.
% Section 4.4 describes how to determine the length of a message-body
%  if a `Content-Length` is not given.
%
% Note that the meaning of this field is significantly different from
%  the corresponding definition in MIME, where it is an optional field
%   used within the `message/external-body` content-type.
% In HTTP, it SHOULD be sent whenever the message's length
%  can be determined prior to being transferred, unless this is
%  prohibited by the rules in section 4.4.
%
% # Example
%
% ~~~{.http}
% Content-Length: 3495
% ~~~
%
% @arg ContentLength A compound term of the form
%      =|'Content-Length'(Status:between(100,999),Length:nonneg)|=

'Content-Length'(
  'Content-Length'(Length),
  'Content-Length'(Status,Length)
) -->
  {\+ between(100, 199, Status), \+ memberchk(Status, [204,304])},
  "Content-Length:",
  integer(Length).

