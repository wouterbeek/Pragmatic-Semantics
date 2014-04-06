:- module(
  rfc2616_entity_tag,
  [
    'entity-tag'//3 % -ParseTree:compound
                    % ?Weak:boolean
                    % ?Tag:atom
  ]
).

/** <module> RFC 2616 entity tag

DCG for RFC 2616 entity tags.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http(rfc2616_basic)).
:- use_module(http(rfc2616_generic)).



%! 'entity-tag'(-ParseTree:compound, ?Weak:boolean, ?Tag:atom)//
% # Syntax
%
% An entity tag consists of an opaque quoted string,
%  possibly prefixed by a weakness indicator.
%
% ~~~{.abnf}
% entity-tag = [ weak ] opaque-tag
% ~~~
%
% # Semantics
%
% An entity tag MUST be unique across all versions of all entities
%  associated with a particular resource.
% A given entity tag value MAY be used for entities obtained by requests
%  on different URIs.
% The use of the same entity tag value in conjunction with
%  entities obtained by requests on different URIs does not imply
%  the equivalence of those entities.
%
% # Pragmatics
%
% Entity tags are used for comparing two or more entities from the same
%  requested resource.
% HTTP/1.1 uses entity tags in the `ETag`, `If-Match`, `If-None-Match`,
%  and `If-Range` header fields.
% The definition of how they are used and compared as cache validators
%  is in section 13.3.3.

'entity-tag'('entity-tag'(T1,T2), Weak, Tag) -->
  (
    weak(T1, Weak)
  ;
    "",
    {Weak = false}
  ),
  'opaque-tag'(T2, Tag).



%! 'opaque-tag'(-ParseTree:compound, ?Tag:atom)//
% ~~~{.abnf}
% opaque-tag = quoted-string
% ~~~

'opaque-tag'('opaque-tag'(Tag), Tag) -->
  'quoted-string'(Tag).



%! weak(-ParseTree:compound, ?Weak:boolean)//
% # Syntax
%
% ~~~{.abnf}
% weak = "W/"
% ~~~
%
% # Semantics
%
% A *|strong entity tag|* MAY be shared by two entities of a resource only if
%  they are equivalent by octet equality.
%
% A *|weak entity tag|*, indicated by the `W/` prefix,
%  MAY be shared by two entities of a resource only if
%  the entities are equivalent and could be substituted for each other
%  with no significant change in semantics.
% A weak entity tag can only be used for weak comparison.

weak(weak, true) -->
  "W/".

