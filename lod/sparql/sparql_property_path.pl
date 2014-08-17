:- module(
  sparql_property_path,
  [
  ]
).

/** <module> SPARQL Property Path

Grammar for property paths in SPARQL.

@author Wouter Beek
@version 2014/08
*/

:- use_module(dcg(dcg_generic)).



%! 'BlankNodePropertyListPath'(?BNodePropertyListPath:compound)// is det.
% ~~~{.ebnf}
% BlankNodePropertyListPath ::= '[' PropertyListPathNotEmpty ']'
% ~~~
%
% @compat SPARQL 1.1 Query [101].

'BlankNodePropertyListPath'(rdf(bnode(anon), ProperyList)) -->
  bracketed(angular, 'PropertyListPathNotEmpty'(ProperyList)).



%! 'CollectionPath'// is det.
% ~~~{.ebnf}
% CollectionPath ::= '(' GraphNodePath+ ')'
% ~~~
%
% @compat SPARQL 1.1 Query [103].

'CollectionPath' -->
  bracketed(round, 'GraphNodePath+').
'GraphNodePath+' -->
  'GraphNodePath',
  'GraphNodePath+'.
'GraphNodePath+' -->
  'GraphNodePath'.



%! 'GraphNodePath'// is det.
% ~~~{.ebnf}
% GraphNodePath ::= VarOrTerm | TriplesNodePath
% ~~~
%
% @compat SPARQL 1.1 Query [105].

'GraphNodePath' -->
  'VarOrTerm'.
'GraphNodePath' -->
  'TriplesNodePath'.



%! 'ObjectListPath'// is det.
% ~~~{.ebnf}
% ObjectListPath ::= ObjectPath ( ',' ObjectPath )*
% ~~~
%
% @compat SPARQL 1.1 Query [86].

'ObjectListPath' -->
  'ObjectPath'.
'ObjectListPath' -->
   comma_separator,
  'ObjectPath,
  'ObjectListPath'.



%! 'ObjectPath'// is det.
% ~~~{.ebnf}
% ObjectPath ::= GraphNodePath
% ~~~
%
% @compat SPARQL 1.1 Query [87].

'ObjectPath' -->
  'GraphNodePath'.



%! 'Path'// is det.
% ~~~{.ebnf}
% Path ::= PathAlternative
% ~~~
%
% @compat SPARQL 1.1 Query [88].

'Path' -->
  'PathAlternative'.



%! 'PathAlternative'// is det.
% ~~~{.ebnf}
% PathAlternative ::= PathSequence ( '|' PathSequence )*
% ~~~

'PathAlternative' -->
  'PathSequence'.
'PathAlternative' -->
  'WS+', "|", 'WS+',
  'PathSequence',
  'PathAlternative'.



%! 'PathElt'// is det.
% ~~~{.ebnf}
% PathElt ::= PathPrimary PathMod?
% ~~~
%
% @compat SPARQL 1.1 Query [91].

'PathElt' -->
  'PathPrimary',
  ('PathMod' ; "").



%! 'PathEltOrInverse'// is det.
% ~~~{.ebnf}
% PathEltOrInverse ::= PathElt | '^' PathElt
% ~~~
%
% @compat SPARQL 1.1 Query [92].

'PathEltOrInverse' -->
  'PathElt'.
'PathEltOrInverse' -->
  "^", 'PathElt'.



%! 'PathMod'// is det.
% ~~~{.ebnf}
% PathMod	  ::=  	'?' | '*' | '+'
% ~~~
%
% @compat SPARQL 1.1 Query [93].

'PathMod' -->
  "?".
'PathMod' -->
  "*".
'PathMod' -->
  "+".



%! 'PathNegatedPropertySet'// is det.
% ~~~{.ebnf}
% PathNegatedPropertySet ::=   PathOneInPropertySet
%                            | '('
%                                (
%                                  PathOneInPropertySet
%                                  ( '|' PathOneInPropertySet )*
%                                )?
%                              ')'
% ~~~

'PathNegatedPropertySet' -->
  'PathOneInPropertySet'.
'PathNegatedPropertySet' -->
  bracketed(round, 'PathOneInPropertySet*').
'PathOneInPropertySet*' -->
  'PathOneInPropertySet'.
'PathOneInPropertySet*' -->
  'WS+', "|", 'WS+',
  'PathOneInPropertySet',
  'PathOneInPropertySet*'.



%! 'PathOneInPropertySet'// is det.
% ~~~{.ebnf}
% PathOneInPropertySet ::= iri | 'a' | '^' ( iri | 'a' )
% ~~~
%
% @compat SPARQL 1.1 Query [96].

'PathOneInPropertySet'(P) -->
  iri(P).
'PathOneInPropertySet'(P) -->
  {rdf_global_id(rdf:type, P)},
  "a".
'PathOneInPropertySet' -->
  "^" (iri ; "a" ).



%! 'PathPrimary'// is det.
% ~~~{.ebnf}
% PathPrimary ::= iri | 'a' | '!' PathNegatedPropertySet | '(' Path ')'
% ~~~
%
% @compat SPARQL 1.1 Query [94].

'PathPrimary' -->
  iri.
'PathPrimary' -->
  'a'.
'PathPrimary' -->
  "!",
  'PathNegatedPropertySet'.
'PathPrimary' -->
  bracketed(round, 'Path').



%! 'PathSequence'// is det.
% ~~~{.ebnf}
% PathSequence ::= PathEltOrInverse ( '/' PathEltOrInverse )*
% ~~~
%
% @compat SPARQL 1.1 Query [90].

'PathSequence' -->
  'PathEltOrInverse'.
'PathSequence' -->
  'WS+', "/", 'WS+',
  'PathEltOrInverse',
  'PathSequence'.



%! 'PropertyListPathNotEmpty'(?PropertyListPath:list(pair))// is det.
% ~~~{.ebnf}
% PropertyListPathNotEmpty ::= ( VerbPath | VerbSimple )
%                              ObjectListPath
%                              (
%                                ';'
%                                ( ( VerbPath | VerbSimple ) ObjectList )?
%                              )*
% ~~~
%
% @compat SPARQL 1.1 Query [83].

'PropertyListPathNotEmpty'([P-Os]) -->
  ('VerbPath'(P) ; 'VerbSimple'(P)),
  'ObjectListPath'(Os).
'PropertyListPathNotEmpty'([P-Os|T]) -->
  ('VerbPath'(P) ; 'VerbSimple'(P)),
  'ObjectListPath'(Os),
  'PropertyListPathNotEmpty'(T).



%! 'TriplesNodePath'// is det.
% ~~~{.ebnf}
% TriplesNodePath ::= CollectionPath | BlankNodePropertyListPath
% ~~~
%
% @compat SPARQL 1.1 Query [100].

'TriplesNodePath' -->
  'CollectionPath'.
'TriplesNodePath' -->
  'BlankNodePropertyListPath'.



%! 'VerbPath'(?Path)// is det.
% ~~~{.ebnf}
% VerbPath ::= Path
% ~~~
%
% @compat SPARQL 1.1 Query [84].

'VerbPath' -->
  'Path'.
