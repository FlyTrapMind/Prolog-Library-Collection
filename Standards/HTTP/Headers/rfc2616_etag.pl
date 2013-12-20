:- module(
  rfc2616_etag,
  [
    'ETag'//3 % -ParseTree:compound
              % ?Weak:boolean
              % ?Tag:atom
  ]
).

/** <module> RFC 2616 ETag

DCG for the `ETag` response header of RFC 2616.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(http_parameters(rfc2616_entity_tag)).



%! 'ETag'(-ParseTree:compound, ?Weak:boolean, ?Tag:atom)//
% The `ETag` response-header field provides the current value of
%  the entity tag for the requested variant.
%
% The entity tag MAY be used for comparison with other entities
%  from the same resource.
%
% ~~~{.abnf}
% ETag = "ETag" ":" entity-tag
% ~~~
%
% # Examples
%
% ~~~{.http}
% ETag: "xyzzy"
% ETag: W/"xyzzy"
% ETag: ""
% ~~~

'ETag'('ETag'(T1), Weak-Tag) -->
  "ETag:",
  'entity-tag'(T1, Weak, Tag).

