:- module(
  rfc5646,
  [
    rfc5646_language_tag//2 % -Tree:compound
                            % ?LanguageTag:compound
  ]
).

/** <module> RFC5646

Support for RFC 5646 on tags for identifying languages.

## Concepts

  * **Tag**
    A complete language tag, such as `sr-Latn-RS` or `az-Arab-IR`.
  * **Subtag**
    A specific section of a tag, delimited by a hyphen, such as the subtags
    `zh`, `Hant`, and `CN` in the tag `zh-Hant-CN`.
  * **Code**
    A value defined in external standards (used as a subtag in this document).
    E.g., `Hant` is an ISO15924 script code that was used to define the
    `Hant` script subtag for use in a language tag.
  * **Primary language subtag**
    The first language subtag in a language tag.
    I.e., language// in langtag//, x// in privateuse//,
    and i// in some grandfathered//.

## Case

Language tags and their subtags, including private use and extensions,
are case insensitive.
Consistent formatting and presentation of language tags does aid users.
The existing conventions for case in language tags are used:
  * *ISO639-1* recommends that language codes be written in lowercase
    Example: `mn` for Mongolian.
  * *ISO15924* recommends that script codes use lowercase with the
    initial letter capitalized.
    Example" `Cyrl` for Cyrillic.
  * *ISO3166-1* recommends that country codes be capitalized.
    Example: `MN` for Mongolia.

### How to implement case

All subtags, including extension and private use subtags, use lowercase
letters with two exceptions: 2-letter and 4-letter subtags that neither
appear at the start of the tag nor occur after singletons.
Such 2-letter subtags are all uppercase (e.g., `en-CA-x-ca`, `sgn-BE-FR`)
and 4-letter subtags are titlecase (e.g., `az-Latn-x-latn`).

### Case folding outside of the character set used for encoding

Case folding of ASCII letters in certain locales, unless
carefully handled, sometimes produces non-ASCII character values.
The Unicode Character Database file `SpecialCasing.txt`
defines the specific cases that are known to cause
problems with this. In particular, the letter 'i' (U+0069) in
Turkish and Azerbaijani is uppercased to `U+0130` (`LATIN CAPITAL LETTER
I WITH DOT ABOVE`). Implementers SHOULD specify a locale-neutral
casing operation to ensure that case folding of subtags does not
produce this value, which is illegal in language tags.  For example,
if one were to uppercase the region subtag `in` using Turkish locale
rules, the sequence `U+0130 U+004E` would result, instead of the
expected `IN`.

## Language tag formal properties

Language tags are designed so that each subtag type has unique length and
content restrictions.

Sequences of private use and extension subtags MUST occur at the end
of the sequence of subtags and MUST NOT be interspersed with subtags
defined elsewhere in this document. These sequences are introduced
by single-character subtags, which are reserved as follows:
  *  `x`
     Introduces a sequence of private use subtags. The interpretation of any
     private use subtag is defined solely by private agreement.
  *  `i`
     Used by some grandfathered tags, e.g. *i-default*, where it always
     appears in the first position and cannot be confused with an extension.
  *  All other single-letter and single-digit subtags are reserved to
     introduce standardized extension subtag sequences.

## Non-standard language tags

Some of the subtags in the IANA registry do not come from an underlying
standard. These can only appear in specific positions in a tag: they can only
occur as primary language subtags or as variant subtags.

## Well-formedness

A tag is considered well-formed if it conforms to the ABNF.
Language tags may be well-formed in terms of syntax but not valid
in terms of content.

## Validity

A tag is valid if:
  1. The tag is well-formed.
  2. Either the tag is in the list of grandfathered tags or all of its
     primary language, extended language, script, region, and variant
     subtags appear in the IANA Language Subtag Registry as of the
     particular registry date.
  3. There are no duplicate variant subtags.
  4. There are no duplicate singleton (extension) subtags.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(generics(print_ext)).
:- use_module(lang('iso639-1')). % Used in meta-call.
:- use_module(lang('iso639-2')). % Used in meta-call.
:- use_module(library(pio)).
:- use_module(library(plunit)).

:- meta_predicate(iana_find(//)).

:- db_add_novel(user:prolog_file_type(txt, text)).



... --> [].
... --> [_], ... .

%! iana_date(-Tree:compound, ?Date:atom)//
% @tbd Parse date subcomponents (existing standard for this?).

iana_date(date(Date), Date) -->
  "Added", colon, space,
  dcg_until([end_mode(inclusive),output_format(atom)], end_of_line, Date).

iana_description(description(Description), Description) -->
  "Description", colon, space,
  dcg_until([end_mode(inclusive),output_format(atom)], end_of_line, Description).

iana_find(Pattern):-
  absolute_file_name(
    lang(rfc5646_language_subtag_registry),
    File,
    [access(read), file_type(text)]
  ),
  phrase_from_file((..., Pattern, ...), File).

iana_registration(
  registration(T1,T2,T3,T4),
  Type,
  Subtag,
  Description,
  Date
) -->
  iana_type(T1, Type),
  iana_subtag(T2, Subtag),
  iana_description(T3, Description),
  iana_date(T4, Date).

iana_subtag(subtag(Subtag), Subtag) -->
  "Subtag", colon, space,
  dcg_until([end_mode(inclusive),output_format(atom)], end_of_line, Subtag).

iana_type(type(Type), Type) -->
  "Type", colon, space,
  dcg_until([end_mode(inclusive),output_format(atom)], end_of_line, Type).

%! extended_language_subtag(-Tree:compound, ?LanguageExtensions:atom)//
% Extended language subtags are used to identify certain specially
% selected languages that, for various historical and compatibility
% reasons, are closely identified with or tagged using an existing
% primary language subtag.
%
% All languages that have an extended language subtag in the registry
% also have an identical primary language subtag record in the
% registry.
% This primary language subtag is RECOMMENDED for forming the language tag.
%
% All extended language subtags are 3-letter subtags fom ISO 639-3.
%
% Although the ABNF production `extlang` permits up to three
% extended language tags in the language tag, extended language
% subtags MUST NOT include another extended language subtag in
% their `Prefix`. That is, the second and third extended language
% subtag positions in a language tag are permanently reserved and
% tags that include those subtags in that position are, and will
% always remain, invalid.
%
% ~~~{.abnf}
% extlang = 3ALPHA *2("-" 3ALPHA)
% ~~~
%
% ## Example
%
% Gan Chinese is represented with tags beginning `zh-gan` or `gan`.
%
% @tbd Find a better/shorter way to write this down: =|M*N(DCG_Rule)|=
%      with arguments.

extended_language_subtag(T0, L) -->
  dcg_multi_atom(letter, 3, X1),
  (
    "", {L = [X1]}
  ;
    hyphen_minus, {H1 = '-'},
    dcg_multi_atom(letter, 3, X2),
    (
      "",
      {L = [X1,X2]}
    ;
      hyphen_minus, {H2 = '-'},
      dcg_multi_atom(letter, 3, X3),
      (
        "",
        {L = [X1,X2,X3]}
      ;
        hyphen_minus, {H3 = '-'},
        dcg_multi_atom(letter, 3, X4),
        {L = [X1,X2,X3,X4]}
      )
    )
  ),
  {parse_tree(extended_language_subtag, [X1,H1,X2,H2,X3,H3,X4], T0)}.

%! extension(-Tree:compound, ?Extension:list(atomic))//
% Extensions provide a mechanism for extending language tags for use in
% various applications. They are intended to identify information that
% is commonly used in association with languages or language tags but
% that is not part of language identification.
%
% ~~~{.abnf}
% extension = singleton 1*("-" (2*8alphanum))
% ~~~
%
% ## Position in language tag
%
% Extension subtags MUST follow all primary language, extended language,
% script, region, and variant subtags in a tag and MUST precede any private
% use subtag sequences.
%
% An extension MUST follow at least a primary language subtag.
% Extensions extend language tags, they do not override or replace them.
% Note that extensions cannot be used in tags that are entirely private use
% (i.e., those starting with `x-`).
%
% ## Singleton subtags
%
% Extension subtags are separated from the other subtags defined in this
% document by a single-character subtag singleton//.
% The singleton MUST be one allocated to a registration authority and
% MUST NOT be the letter `x` which is reserved for private use.
%
% Singleton subtags MUST NOT be repeated.
% Note that the tag `en-a-bbb-x-a-ccc` is valid because the second
% appearance of the singleton `a` is in a private use sequence.
%
% Each singleton MUST be followed by at least one extension subtag.
%
% All subtags following the singleton and before another singleton are part
% of the extension.
% Example: in the tag `fr-a-Latn`, the subtag `Latn` does not represent the
% script subtag `Latn`; instead its meaning is defined by the extension `a`.
%
% ## Formal properties
%
% Each extension subtag MUST be from 2 to 8 characters long and consist
% solely of letters or digits, with each subtag separated by a single `-`.
% Normalized extension subtags are expected to be in lowercase.
%
% ## Canonical form
%
% In the event that more than one extension appears in a single tag,
% the tag SHOULD be canonicalized as described in Section 4.5, by ordering
% the various extension sequences into case-insensitive ASCII order.
% For example, if an extension were defined for the singleton `r` and
% it defined the subtags shown, then the following tag would be a valid
% example: `en-Latn-GB-boont-r-extended-sequence-x-private`.
%
% @tbd Document and implement canonical form.

extension(extension(T1, T2), [Singleton|ExtensionComponents]) -->
  singleton(T1, Singleton),
  extension_components(T2, ExtensionComponents).

%! extension_components(-Tree:compound, ?ExtensionComponents:list(atom))//

extension_components(extension_components('-',H,T1), [H|T]) -->
  hyphen_minus,
  dcg_multi_atom(alpha_numeric, between(2,8), H),
  extension_components(T1, T).
extension_components(extension_components('-',H), [H]) -->
  hyphen_minus,
  dcg_multi_atom(alpha_numeric, between(2,8), H).

%! extensions(-Tree:compound, ?Extensions:list(list(atomic)))//

extensions(extensions('-',T1,T2), [H|T]) -->
  hyphen_minus,
  extension(T1, H),
  extensions(T2, T).
extensions(extensions('-',T1), [H]) -->
  hyphen_minus,
  extension(T1, H).

%! grandfathered(-Tree:compound, ?LanguageTag:list(atom))//
% Non-redundant (see below) tags registered during the RFC 1766 and/or
% RFC 3066 era.
%
% ~~~{.abnf}
% grandfathered = irregular / regular
% ~~~
%
% ## Non-grandfathered redundant tags
%
% Many of the previously registered language tags are now redundant.
% A **redundant tag** is a grandfathered registration whose individual
% subtags appear with the same semantic meaning in the registry.
% For example, the tag `zh-Hant` (Traditional Chinese) can now be composed
% from the subtags `zh` (Chinese) and `Hant` (Han script traditional variant).
% These redundant tags are maintained in the registry as records of type
% `redundant`, mostly as a matter of historical curiosity.
%
% ## Grandfathered tags 1: Rergular
%
% Grandfathered tags that (appear to) match the langtag// production are
% considered regular grandfathered tags.
% Such a tag contains one or more subtags that either do not
% individually appear in the registry or appear but with a different
% semantic meaning: each tag, in its entirety, represents a language or
% collection of languages.
%
% ## Grandfathered tags 2: Non-rergular
%
% Grandfathered tags that do not match the langtag// production in the ABNF
% and would otherwise be invalid are considered irregular grandfathered tags.
% With the exception of `en-GB-oed`, which is a variant of `en-GB`, each of
% them, in its entirety, represents a language.
%
% ## Phasing out procedure for grandfathered tags
%
% Many of the grandfathered tags have been superseded by the subsequent
% addition of new subtags: each superseded record contains a `Preferred-Value`
% field that ought to be used to form language tags representing that value.
% For example, the tag `art-lojban` is superseded by the primary language
% subtag `jbo`.

grandfathered(grandfathered(T1), LanguageTag) --> regular(T1, LanguageTag).
grandfathered(grandfathered(T1), LanguageTag) --> irregular(T1, LanguageTag).

%! irregular(-Tree:compound, ?LanguageTag:list(atom))//
% Irregular tags do not match the langtag// production and would not otherwise
% be considered well-formed. These tags are all valid, but most are deprecated
% in favor of more modern subtags or subtag combinations.
%
% The single-character subtag `i` is used by some grandfathered tags.
% (Other grandfathered tags have a primary language subtag in their first
% position.)

irregular(irregular('en-GB-oed'), [en,'GB',oed]) --> "en-GB-oed".
irregular(irregular('i-ami'), [i,ami]) --> "i-ami".
irregular(irregular('i-bnn'), [i,bnn]) --> "i-bnn".
irregular(irregular('i-default'), [i,default]) --> "i-default".
irregular(irregular('i-enochian'), [i,enochian]) --> "i-enochian".
irregular(irregular('i-hak'), [i,hak]) --> "i-hak".
irregular(irregular('i-klingon'), [i,klingon]) --> "i-klingon".
irregular(irregular('i-lux'), [i,lux]) --> "i-lux".
irregular(irregular('i-mingo'), [i,mingo]) --> "i-mingo".
irregular(irregular('i-navajo'), [i,navajo]) --> "i-navajo".
irregular(irregular('i-pwn'), [i,pwn]) --> "i-pwn".
irregular(irregular('i-tao'), [i,tao]) --> "i-tao".
irregular(irregular('i-tay'), [i,tay]) --> "i-tay".
irregular(irregular('i-tsu'), [i,tsu]) --> "i-tsu".
irregular(irregular('sgn-BE-FR'), [sgn,'BE','FR']) --> "sgn-BE-FR".
irregular(irregular('sgn-BE-NL'), [sgn-'BE'-'NL']) --> "sgn-BE-NL".
irregular(irregular('sgn-CH-DE'), [sgn-'CH'-'DE']) --> "sgn-CH-DE".

%! langtag//
% ~~~{.abnf}
% langtag = language
%           ["-" script]
%           ["-" region]
%           *("-" variant)
%           *("-" extension)
%           ["-" privateuse]
% ~~~

langtag(
  T0,
  Primary,
  Extended,
  Script,
  Region,
  Variants,
  Extensions,
  PrivateLanguage
) -->
  language(T1, Primary, Extended),
  (hyphen_minus, {H1 = '-'}, script(T2, Script) ; ""),
  (hyphen_minus, {H2 = '-'}, region(T3, Region) ; ""),
  (variants(T4, Variants) ; ""),
  (extensions(T5, Extensions) ; ""),
  (hyphen_minus, {H3 = '-'}, privateuse(T6, PrivateLanguage) ; ""),
  {parse_tree(langtag, [T1,H1,T2,H2,T3,T4,T5,H3,T6], T0)}.

%! rfc5646_language_tag//
% ~~~{.abnf}
% Language-Tag  = langtag / privateuse / grandfathered
% ~~~

% Normal language tags.
rfc5646_language_tag(
  rfc5646_language_tag(T1),
  rfc5646_language_tag(
    Primary,
    Extended,
    Script,
    Region,
    Variants,
    Extensions,
    Private
  )
) -->
  langtag(
    T1,
    Primary,
    Extended,
    Script,
    Region,
    Variants,
    Extensions,
    Private
  ).
% Private use tags.
rfc5646_language_tag(rfc5646_language_tag(T1), LanguageTag) -->
  privateuse(T1, LanguageTag).
% Grandfathered tags.
rfc5646_language_tag(rfc5646_language_tag(T1), LanguageTag) -->
  grandfathered(T1, LanguageTag).

%! language(-Tree:compound, ?Language:atom, ?LanguageExtensions:list(atom))//
% The recommended format for language tags.
%
% 2-character language subtags are drawn from ISO 639-1.
%
% 3-character language subtags are drawn from ISO 639-2, -3, and -5.
%
% Subtags in the range `qaa`-`qtz` are reserved for private use
% (as in ISO 639-2).
%
% 4-character language subtags are reserved for future standardization.
%
% 5- to 8-character language subtags are reserved for future discouraged use.
%
% ~~~{.abnf}
% language = 2*3ALPHA ["-" extlang] / 4ALPHA / 5*8ALPHA
% ~~~
%
% ## Inclusion in multiple standards
%
% If languages have both a 2- and a 3-character ISO code, the 2-character code
% is defined in the IANA registry.
%
% When a language has no 2-character code and the ISO 639-2/T (Terminology)
% code differes from the ISO 639-2/B (Bibliographic) code, only the
% Terminology code is defined in the IANA registry.
%
% If a 2-character code is added to ISO 639-1 for a language for which a
% 3-character code was already included in either ISO 639-2 or ISO 639-3,
% the 2-character code MUST NOT be registered.

% The shortest ISO639 code, sometimes followed by extended language subtags.
language(T0, Language, LanguageExtensions) -->
  dcg_multi_atom(letter, between(2,3), Language),
  ("" ; hyphen_minus, extended_language_subtag(T2, LanguageExtensions)),
  {parse_tree(language, [Language,T2], T0)}.
% Reserved for future use.
language(language(Language), Language, []) -->
  dcg_multi_atom(letter, 4, Language).
% Registered language subtag.
language(language(Language), Language, []) -->
  dcg_multi_atom(letter, between(5,8), Language).

%! privateuse(-Tree:compound, ?PrivateLanguage:list(atom))//
% Private use subtags are used to indicate distinctions in language
% that are important in a given context by private agreement.
%
% ## Position in the language tag
%
% Private use subtags are separated from the other subtags by the reserved
% single-character subtag `x`.
%
% Private use subtags MUST follow all primary language, extended
% language, script, region, variant, and extension subtags in the tag.
%
% ## Formal properties
%
% The single-character subtag `x` as the primary (i.e., first) subtag
% indicates that the language tag consists solely of subtags whose meaning
% is defined by private agreement.
%
% Private use subtags MUST conform to the format and content constraints
% defined in the ABNF for all subtags; that is, they MUST consist solely of
% letters and digits and not exceed 8 characters in length.
%
% A tag MAY consist entirely of private use subtags.
%
% Private use subtags are NOT RECOMMENDED where alternatives exist or for
% general interchange.
%
% ## Example
%
% Suppose a group of scholars is studying some texts in medieval Greek.
% They might agree to use some collection of private use subtags to identify
% different styles of writing in the texts.
% They might use `el-x-koine` for documents in the "common" style while
% using `el-x-attic` for other documents that mimic the Attic style.
% These subtags would not be recognized by outside processes or systems,
% but might be useful in categorizing various texts for study by those
% in the group.
%
% ## Note on standards
%
% In the registry, there are also subtags derived from codes reserved
% by ISO 639, ISO 15924, or ISO 3166 for private use.  Do not confuse
% these with private use subtag sequences following the subtag 'x'.
%
% ~~~{.abnf}
% privateuse = "x" 1*("-" (1*8alphanum))
% ~~~

privateuse(pivateuse(T1), PrivateTags) -->
  x_lowercase,
  privateuse_components(T1, PrivateTags).

privateuse_components(privateuse_components('-',H), [H]) -->
  hyphen_minus,
  dcg_multi_atom(alpha_numeric, between(1,8), H).
privateuse_components(privateuse_components('-',H,T2), [H|T]) -->
  hyphen_minus,
  dcg_multi_atom(alpha_numeric, between(1,8), H),
  privateuse_components(T2, T).

%! region(-Tree:compound, ?Region:atomic)//
% Region subtags indicate linguistic variations associated with or appropriate
% to a specific country, territory, or region.
%
% ~~~{.abnf}
% region = 2ALPHA / 3DIGIT
% ~~~
%
% ## Intended use
%
% Typically, a region subtag is
% used to indicate variations such as regional dialects or usage, or
% region-specific spelling conventions. It can also be used to indicate that
% content is expressed in a way that is appropriate for use throughout a
% region, for instance, Spanish content tailored to be useful throughout Latin
% America.
%
% ## Position within language tag
%
% Region subtags MUST follow any primary language, extended language,
% or script subtags and MUST precede any other type of subtag.
%
% There MUST be at most one region subtag in a language tag and the region
% subtag MAY be omitted, as when it adds no distinguishing value to the tag.
%
% ## Possible values 1: Private use
%
% The region subtags `AA`, `QM`-`QZ`, `XA`-`XZ`, and `ZZ` are reserved for
% private use (as in ISO 3166.
%
% ## Possible values 2: ISO
%
% 2-letter region subtags are defined according to ISO 3166-1, using the list
% of alpha-2 country codes.
% In addition, the codes that are "exceptionally reserved" (as opposed to
% "assigned") in ISO 3166-1 were also defined in the registry, with
% the exception of `UK`, which is an exact synonym for the assigned code `GB`.
%
% ## Possible values 2: UN
%
% 3-character region subtags consist solely of digit (number) characters
% and are defined according to the assignments found in the UN Standard
% Country or Area Codes for Statistical Use (UN_M.49).
% Not all of the UN M.49 codes are defined in the IANA registry.
% The following rules define which codes are entered into the registry as
% valid subtags:
%   1. UN numeric codes assigned to `macro-geographical (continental)` or
%      sub-regions MUST be registered in the registry. These codes are not
%      associated with an assigned ISO 3166-1 alpha-2 code and represent
%      supra-national areas, usually covering more than one nation, state,
%      province, or territory.
%   2. UN numeric codes for `economic groupings` or `other groupings`
%      MUST NOT be registered in the IANA registry and MUST NOT be used
%      to form language tags.
%   3. When ISO 3166-1 reassigns a code formerly used for one country or
%      area to another country or area and that code already is present in
%      the registry, the UN numeric code for that country or area MUST be
%      registered in the registry and MUST be used to form language tags
%      that represent the country or region for which it is defined
%      (rather than the recycled ISO 3166-1 code).
%   4. UN numeric codes for countries or areas for which there is an
%      associated ISO 3166-1 alpha-2 code in the registry MUST NOT be
%      entered into the registry and MUST NOT be used to form language
%      tags.
%   5. For historical reasons, the UN numeric code 830 (Channel Islands),
%      which was not registered at the time this document was adopted
%      and had, at that time, no corresponding ISO 3166-1 code, MAY be
%      entered into the IANA registry.
%   6. All other UN numeric codes for countries or areas that do not
%      have an associated ISO 3166-1 alpha-2 code MUST NOT be
%      entered into the registry and MUST NOT be used to form
%      language tags.
%   7. The alphanumeric codes in Appendix X of the UN document MUST NOT
%      be entered into the registry and MUST NOT be used to form language
%      tags.
%
% ## Examples
%
% `de-AT` represents German (`de`) as used in Austria (`AT`).
%
% `sr-Latn-RS` represents Serbian (`sr`) written using Latin script
% (`Latn`) as used in Serbia (`RS`).
%
% `es-419` represents Spanish (`es`) appropriate to the UN-defined
% Latin America and Caribbean region (`419`).

region(region(Region), Region) -->
  dcg_multi_atom(letter, 2, Region).
region(region(Region), Region) -->
  dcg_multi(decimal_digit, 3, Codes),
  {number_codes(Region, Codes)}.

%! regular(-Tree:compound, ?LanguageTag:list(atom))//
% These tags match the langtag// production, but their subtags are not
% extended language or variant subtags: their meaning is defined by
% their registration and all of these are deprecated in favor of a more
% modern subtag or sequence of subtags.

regular(regular('art-lojban'), [art,lojban]) --> "art-lojban".
regular(regular('cel-gaulish'), [cel,gaulish]) --> "cel-gaulish".
regular(regular('no-bok'), [no,bok]) --> "no-bok".
regular(regular('no-nyn'), [no,nyn]) --> "no-nyn".
regular(regular('zh-guoyu'), [zh,guoyu]) --> "zh-guoyu".
regular(regular('zh-hakka'), [zh,hakka]) --> "zh-hakka".
regular(regular('zh-min'), [zh,min]) --> "zh-min".
regular(regular('zh-min-nan'), [zh,min,nan]) --> "zh-min-nan".
regular(regular('zh-xiang'), [zh,xiang]) --> "zh-xiang".

%! script(-Tree:compound, ?Script:atom)//
% Script or writing system variations that distinguish the written forms of a
% language or its dialects.
%
% ~~~{.abnf}
% script = 4ALPHA
% ~~~
%
% ## Position in language tag
%
% Script subtags MUST follow any primary and extended language subtags and
% MUST precede any other type of subtag.
%
% ## Supported values
%
% Script subtags consist of 4 letters and are defined according to the
% assignments in ISO 15924.
%
% ## Private use
%
% Script subtags `Qaaa`-`Qabx` are reserved for private use (as in ISO 15924).
%
% ## Intended use
%
% There MUST be at most one script subtag in a language tag, and the script
% subtag SHOULD be omitted when it adds no distinguishing value to the tag or
% when the primary or extended language subtag's record in the subtag registry
% includes a `Suppress-Script` field listing the applicable script subtag.
%
% ## Example
%
% `sr-Latn` represents Serbian written using the Latin script.

script(script(Script), Script) -->
  dcg_multi_atom(letter, 4, Script).

%! singleton(-Tree:compound, ?Char:atomic)//
% Single alphanumerics. x// is reserved for private use.
%
% ~~~{.abnf}
% singleton = DIGIT         ; 0 - 9
%             / %x41-57     ; A - W
%             / %x59-5A     ; Y - Z
%             / %x61-77     ; a - w
%             / %x79-7A     ; y - z
% ~~~
%
% @see extension//2

singleton(singleton(Singleton), Singleton) -->
  decimal_digit(Singleton).
singleton(singleton(Singleton), Singleton) -->
  letter(Code),
  {
    \+ memberchk(Code, [88,120]),
    char_code(Singleton, Code)
  }.

%! variant(-Tree:compound, ?Variant:atom)//
% Variant subtags indicate additional, well-recognized variations that define
% a language or its dialects that are not covered by other available subtags.
%
% ~~~{.abnf}
% variant = 5*8alphanum / (DIGIT 3alphanum)
% ~~~
%
% ## Position in language tag
%
% Variant subtags MUST follow any primary language, extended language,
% script, or region subtags and MUST precede any extension or private use
% subtag sequences.
%
% More than one variant MAY be used to form the language tag.
%
% The same variant subtag MUST NOT be used more than once within
% a language tag.
%
% ## Relation to standards
%
% Variant subtags, as a collection, are not associated with any particular
% external standard.
%
% ## Formal properties
%
% In order to distinguish variants from other types of subtags, variants
% MUST meet the following length and content restrictions:
%   1.  Variant subtags that begin with a letter (a-z, A-Z) MUST be
%       at least 5 characters long.
%   2.  Variant subtags that begin with a digit (0-9) MUST be at
%       least 4 characters long.
%
% ## Combinations
%
% Most variants that share a prefix are mutually exclusive.
% For example, the German orthographic variations `1996` and `1901` SHOULD
% NOT be used in the same tag, as they represent the dates of different
% spelling reforms.
% A variant that can meaningfully be used in combination with another
% variant SHOULD include a 'Prefix' field in its registry record that lists
% that other variant.
% For example, if another German variant `example` were created that made
% sense to use with `1996`, then `example` should include two `Prefix`
% fields: `de` and `de-1996`.
%
% ## Examples
%
% `sl-nedis` represents the Natisone or Nadiza dialect of Slovenian.
%
% `de-CH-1996` represents German as used in Switzerland and as
% written using the spelling reform beginning in the year 1996 C.E.

variant(variant(Variant), Variant) -->
  decimal_digit(_N, H),
  dcg_multi(alpha_numeric, 3, T),
  {atom_codes(Variant, [H|T])}.
variant(variant(Variant), Variant) -->
  dcg_multi_atom(alpha_numeric, between(5,8), Variant).

%! variants(-Tree:compound, ?Variants:list(atom))//

variants(variants('-',T1,T2), [H|T]) -->
  hyphen_minus,
  variant(T1, H),
  variants(T2, T).
variants(variants('-',T1), [H]) -->
  hyphen_minus,
  variant(T1, H).



:- begin_tests(rfc5646).

:- use_module(generics(print_ext)).
:- use_module(library(apply)).

rfc5646_atom('zh').
rfc5646_atom('zh-Latn').
rfc5646_atom('zh-Latn-CN').
rfc5646_atom('zh-Latn-CN-variant1').
rfc5646_atom('zh-Latn-CN-variant1-a-extend1').
rfc5646_atom('zh-Latn-CN-variant1-a-extend1-x-wadegile').
rfc5646_atom('zh-Latn-CN-variant1-a-extend1-x-wadegile-private1').

test(rfc5646_parse, [forall(rfc5646_atom(LanguageTag))]):-
  atom_codes(LanguageTag, Codes),
  once(phrase(rfc5646_language_tag(Tree, Term), Codes)),
  maplist(formatnl, [Tree, Term]).

:- end_tests(rfc5646).

