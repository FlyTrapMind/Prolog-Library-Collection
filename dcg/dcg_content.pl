:- module(
  dcg_content,
  [
    capitalize//0,
    indent//0,
    indent//1, % +Indent:nonneg
    indent//2, % +Indent:nonneg
               % :Dcg
    nl//0,
    pl_term//1, % +PrologTerm
    void//0
  ]
).
:- reexport(
  library(dcg/basics),
  [
    alpha_to_lower//1,
    blank//0,
    blanks//0,
    blanks_to_nl//0,
    nonblank//1,
    nonblanks//1,
    prolog_var_name//1,
    whites//0
  ]
).

/** <module> DCG: Content

DCG rules for parsing/generating often-occuring content.

@author Wouter Beek
@version 2013/07-2013/09, 2013/11-2014/05, 2014/10
*/

:- use_module(library(lists), except([delete/3])).
:- use_module(library(settings)).

:- use_module(generics(atom_ext)).
:- use_module(pl(pl_log)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii), [line_feed//0,space//0]).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_meta)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The number of spaces that go into one indent.'
).

:- meta_predicate(indent(+,//,?,?)).



%! capitalize// .

capitalize, [Upper] -->
  [Lower],
  {code_type(Upper, to_upper(Lower))}, !,
  dcg_copy.
capitalize -->
  dcg_end.



%! indent// is det.
%! indent(+Indent:nonneg)// is det.
%! indent(+Indent:nonneg, :Dcg)// is det.

indent -->
  indent(1).

indent(I) -->
  {
    setting(indent_size, Size),
    NumberOfSpaces is I * Size
  },
  '#'(NumberOfSpaces, space, []), !.

indent(I, Dcg) -->
  indent(I),
  dcg_call_cp(Dcg).



%! nl// is det.

nl --> line_feed.



%! pl_term(+PrologTerm)// is det.

pl_term(PrologTerm) -->
  {with_output_to(codes(Codes), write_canonical_blobs(PrologTerm))},
  Codes.



%! void// .

void --> "".
