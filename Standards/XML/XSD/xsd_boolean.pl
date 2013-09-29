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
% @param Boolean A boolean value.
% @param LEX A literal matching booleanRep//1.

booleanCanonicalMap(Boolean, LEX):-
  phrase(booleanRep(Boolean), LEX).

%!·booleanLexicalMap(?LEX:list(code), ?Boolean:boolean) is det.
% Maps a literal matching the booleanRep//1 production to a boolean value.
%
% Returns true when =LEX= is `true` or `1` , and
% returns false otherwise (i.e., when `LEX` is `false` or `0`).
%
% @param LEX A literal matching booleanRep//1.
% @param Boolean A boolean value.

booleanLexicalMap(LEX, Boolean):-
  phrase(booleanRep(Boolean), LEX).

%! booleanRep(?Boolean:boolean)//

booleanRep(true) -->
  ("true" ; "1").
booleanRep(false) -->
  ("false" ; "0").
