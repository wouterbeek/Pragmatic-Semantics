:- module(
  rfc2616_general_header,
  [
    'general-header'//2 % -ParseTree:compound
                        % ?GeneralHeader
  ]
).

/** <module> RFC 2616 general header

DCG for general headers (request and response) in RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http_headers(rfc2616_connection)).



%! 'general-header'(-ParseTree:compound, ?GeneralHeader)//
% # Syntax
% 'general-header' = 'Cache-Control'       ; Section 14.9
%                  | 'Connection'          ; Section 14.10
%                  | 'Date'                ; Section 14.18
%                  | 'Pragma'              ; Section 14.32
%                  | 'Trailer'             ; Section 14.40
%                  | 'Transfer-Encoding'   ; Section 14.41
%                  | 'Upgrade'             ; Section 14.42
%                  | 'Via'                 ; Section 14.45
%                  | 'Warning'             ; Section 14.46
%
% # Semantics
%
% Header fields that apply to the message, not to the entity transferred.
%
% There are a few header fields which have general applicability for both
%  request and response messages, but which do not apply to
%  the entity being transferred.
% These header fields apply only to the message being transmitted.
%
% # Pragmatics
%
% ## Extensions
%
% General-header field names can be extended reliably only in combination with
%  a change in the protocol version.
% However, new or experimental header fields may be given the semantics of
%  general header fields if all parties in the communication recognize them
%  to be general-header fields.
% Unrecognized header fields are treated as entity-header fields.

/*
'general-header'(T1, X) -->
  'Cache-Control'(T1, X).
*/
'general-header'(T1, X) -->
  'Connection'(T1, X).
/*
'general-header'(T1, X) -->
  'Date'(T1, X).
'general-header'(T1, X) -->
  'Pragma'(T1, X).
'general-header'(T1, X) -->
  'Trailer'(T1, X).
'general-header'(T1, X) -->
  'Transfer-Encoding'(T1, X).
'general-header'(T1, X) -->
  'Upgrade'(T1, X).
'general-header'(T1, X) -->
  'Via'(T1, X).
'general-header'(T1, X) -->
  'Warning'(T1, X).
*/

