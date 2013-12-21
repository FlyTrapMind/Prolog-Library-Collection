:- module(
  rfc2616_entity,
  [
    'entity-body'//2 % -ParseTree:compound
                     % ?EntityBody:list(octet)
  ]
).

/** <module> RFC 2616 entity

Request and Response messages MAY transfer an entity if not otherwise
 restricted by the request method or response status code.

An entity consists of entity-header fields and an entity-body,
 although some responses will only include the entity-headers.

Both sender and recipient refer to either the client or the server,
 depending on who sends and who receives the entity.

@author Wouter Beek
@see RFC 2616
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(http(rfc2616_basic)).



%! 'entity-body'(-ParseTree:compound, ?EntityBody:list(octet))// .
% ~~~{.abnf}
% entity-body = *OCTET
% ~~~

'entity-body'('entity-body'(EntityBody), EntityBody) -->
  dcg_multi('OCTET', _-_, EntityBody).

