:- module(
  http_accept_language,
  [
    'Accept-Language'//
  ]
).

/** <module> HTTP Accept-Language

The Accept-Language request-header field is similar to Accept,
but restricts the set of natural languages that are preferred
as a response to the request.
Language tags are defined in TODO.

~~~{.abnf}
Accept-Language = "Accept-Language" ":"
                  1#( language-range [ ";" "q" "=" qvalue ] )
language-range  = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
~~~

Each language-range MAY be given an associated quality value
which represents an estimate of the user's preference for
the languages specified by that range.
The quality value defaults to "q=1".

## Example

~~~{.http}
Accept-Language: da, en-gb;q=0.8, en;q=0.7
~~~
would mean: "I prefer Danish, but will accept British English
and other types of English."

## Prefix matching

A language-range matches a language-tag if it exactly equals the tag,
or if it exactly equals a prefix of the tag such that
the first tag character following the prefix is "-".
The special range "*", if present in the Accept-Language field,
matches every tag not matched by any other range present
in the Accept-Language field.

Note: This use of a prefix matching rule does not imply that
language tags are assigned to languages in such a way that it is
always true that if a user understands a language with a certain
tag, then this user will also understand all languages with tags
for which this tag is a prefix. The prefix rule simply allows the
use of prefix tags if this is the case.

## Quality factors

The language quality factor assigned to
  a language-tag by the Accept-Language field is
  the quality value of the longest language-range in the field
  that matches the language-tag.
If no language-range in the field matches the tag,
  the language quality factor assigned is 0.
If no Accept-Language header is present in the request,
  the server SHOULD assume that all languages are equally acceptable.
If an Accept-Language header is present,
  then all languages which are assigned a quality factor greater than 0
  are acceptable.

## Privacy concerns

It might be contrary to the privacy expectations of the user
  to send an Accept-Language header with the complete linguistic preferences
  of the user in every request.

## Application support for user-setting

As intelligibility is highly dependent on the individual user,
  it is recommended that client applications make
  the choice of linguistic preference available to the user.
If the choice is not made available,
  then the Accept-Language header field MUST NOT be given in the request.

Note: When making the choice of linguistic preference available to
the user, we remind implementors of  the fact that users are not
familiar with the details of language matching as described above,
and should provide appropriate guidance. As an example, users
might assume that on selecting "en-gb", they will be served any
kind of English document if British English is not available. A
user agent might suggest in such a case to add "en" to get the
best matching behavior.

--

@author Wouter Beek
@tbd Implement http_abnf_list//1 in supporting ABNF module.
@version 2013/12
*/

:- use_module(dcg(dcg_multi)).
:- use_module(http(http_abnf)).
:- use_module(http(http_quality_value)).



%! 'Accept-Language'//
% ~~~{.abnf}
% Accept-Language = "Accept-Language" ":"
%                   1#( language-range [ ";" "q" "=" qvalue ] )
% ~~~

'Accept-Language' -->
  "Accept-Language:",
  http_abnf_list('language-range-q', 1-_, LangTagsQs).

%! 'language-range'(-LangTags:list(atom))//
% ~~~{.abnf}
% language-range = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
% ~~~

language-range(LangTags) -->
  dcg_multi('language-tag', 1-8, LangTags).
language-range -->
  "*"

%! 'language-range-q'(
%!   -LanguageTagsPlusQualityValue:list(pair(atom,between(0.0,1.0)))
%! )//
% @see 'Accept-Language' for the ABNF definition.

'language-range-q'(LangTagsQ) -->
  'language-range'(LangTags),
  ("" ; ";q=" qvalue).

%! 'language-tag'(-LanguageTag:atom)//
% @see 'language-range' for the ABNF definition.

'language-tag'(LangTag) -->
  dcg_multi('ALPHA', 1-8, Cs),
  {atom_codes(LangTag, Cs)}.
