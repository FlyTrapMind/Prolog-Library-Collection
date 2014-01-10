:- module(
  xsd_boolean,
  [
    booleanCanonicalMap/2, % +Boolean:boolean
                           % -LEX:list(code)
    booleanLexicalMap/2 % ?LEX:list(code)
                        % ?Boolean:boolean
  ]
).

/** <module> XSD_BOOLEAN
*Boolean* represents the values of two-valued logic.

#### Value space

The value space of two-valued logic:  $\{ \text{true}, \text{false} \}$.

#### Lexical representation

~~~{.ebnf}
booleanRep ::= 'true' | 'false' | '1' | '0'
~~~

The lexical mapping for =boolean= is booleanLexicalMap/2.
The canonical mapping for =boolean= is booleanCanonicalMap/2.

#### Facets

Constraining facets:
  * =assertions=
  * =pattern=
  * =|whitespace = collapse (fixed)|=

Values for the funcamental facets:
  * =|bounded = false|=
  * =|cardinality = finite|=
  * =|numeric = false|=
  * =|ordered = false|=

--

@author Wouter Beek
@version 2013/08
*/



%! booleanCanonicalMap(+Boolean:boolean, -LEX:list(code)) is det.
% Maps a boolean value to a booleanRep//1.
%
% Returns `true` when `Boolean` is true, and
% returns `false` otherwise (i.e., when `Boolean` is false).
%
% @arg Boolean A boolean value.
% @arg LEX A literal matching booleanRep//1.

booleanCanonicalMap(Boolean1, LEX):-
  to_boolean(Boolean1, Boolean2),
  phrase(booleanRep(Boolean2), LEX).

%! booleanLexicalMap(?LEX:list(code), ?Boolean:boolean) is det.
% Maps a literal matching the booleanRep//1 production to a boolean value.
%
% Returns true when =LEX= is `true` or `1` , and
% returns false otherwise (i.e., when `LEX` is `false` or `0`).
%
% @arg LEX A literal matching booleanRep//1.
% @arg Boolean A boolean value.

booleanLexicalMap(LEX, Boolean):-
  phrase(booleanRep(Boolean), LEX).

%! booleanRep(?Boolean:boolean)//

booleanRep(true) -->
  ("true" ; "1").
booleanRep(false) -->
  ("false" ; "0").

% Prolog native.
to_boolean(true, true).
to_boolean(false, false).
% Prolog DSL for JSON.
to_boolean(@(true), true).
to_boolean(@(false), false).
% Integer boolean.
to_boolean(1, true).
to_boolean(0, false).
% CKAN boolean.
to_boolean('True', true).
to_boolean('False', false).
% Electric switch.
to_boolean(on, true).
to_boolean(off, false).

