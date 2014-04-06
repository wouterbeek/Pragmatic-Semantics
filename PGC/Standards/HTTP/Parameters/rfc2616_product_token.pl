:- module(
  rfc2616_product_token,
  [
    product//2 % -ParseTree:compound
               % ?Product:compound
  ]
).

/** <module> RFC 2616 product token

DCG for RFC 2616 product tokens.

# Datatypes

## Product

~~~{.pl}
product(
  Name:atom,
  Version:atom
)
~~~

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(parse_tree)).



%! product(-ParseTree:compound, ?Product:compound)//
% Product tokens are used to allow communicating applications to identify
%  themselves by software name and version.
%
% # Syntax
%
% Most fields using product tokens also allow sub-products which form
%  a significant part of the application to be listed,
%  separated by white space.
%
% ~~~{.abnf}
% product = token ["/" product-version]
% ~~~
%
% # Semantics
%
% By convention, the products are listed in order of their significance
%  for identifying the application.
%
% # Pragmatics
%
% Product tokens SHOULD be short and to the point.
% They MUST NOT be used for advertising or other non-essential information.
%
% # Examples
%
% ~~~{.http}
% User-Agent: CERN-LineMode/2.15 libwww/2.17b3
% Server: Apache/0.8.4
% ~~~

product(T0, product(Name,Version)) -->
  token(Name),
  (
    "/",
    'product-version'(T1, Version)
  ;
    ""
  ),
  {parse_tree(product, [Name,T1], T0)}.



%! 'product-version'(-ParseTree:compound, ?Version:atom)//
% The software version of the communicating application.
%
% # Syntax
%
% ~~~{.abnf}
% product-version = token
% ~~~
%
% # Pragmatics
%
% Although any token character MAY appear in a product-version,
%  this token SHOULD only be used for a version identifier
%  (i.e., successive versions of the same product SHOULD only differ in
%  the product-version portion of the product value).

'product-version'('product-version'(Version), Version) -->
  token(Version).

