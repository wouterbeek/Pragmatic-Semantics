:- module(
  rfc2616_content_type,
  [
    'Content-Type'//2 % ?ParseTree:compound
                      % ?ContentType:atom
  ]
).

/** <module> RFC 2616 Content-Length header

Support for the `Content-Length` header in RFC 2616.

@author Wouter Beek
@version 2014/01
*/

:- use_module(http_parameters(rfc2616_media_type)).



%! 'Content-Type'(-ParseTree:compound, +ContentType:compound)// is det.
% The `Content-Type` entity-header field indicates the media type
%  of the entity-body sent to the recipient or,
%  in the case of the `HEAD` method, the media type that would have been sent
%  had the request been a `GET`.
%
% ~~~{.abnf}
% Content-Type = "Content-Type" ":" media-type
% ~~~
%
% Media types are defined in section 3.7. An example of the field is
% ~~~{.http}
% Content-Type: text/html; charset=ISO-8859-4
% ~~~~
%
% Further discussion of methods for identifying the media type
%  of an entity is provided in section 7.2.1.

'Content-Type'('Content-Type'(T1), 'Content-Type'(MediaType)) -->
  "Content-Type:",
  'media-type'(T1, MediaType).
  
