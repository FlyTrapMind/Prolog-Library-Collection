:- module(
  dcg_collection,
  [
    collection//2, % +Options:list(nvpair)
                   % +Elements:list
    nvpair//3, % +Options:list(nvpair)
               % :Name:dcg
               % :Value:dcg
    pair//3,  % +Options:list(nvpair)
              % +Element1
              % +Element2
    proof//2, % +Options:list(nvpair)
              % +Proof:compound
    set//2,  % +Options:list(nvpair)
             % +Elements:list
    tuple//2 % +Options:list(nvpair)
             % +Elements:list
  ]
).

/** <module> DCG collections

DCG rules for parsing/generating collections.

@author Wouter Beek
@version 2013/07-2013/09, 2013/11-2013/12
*/

:- use_module(generics(option_ext)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_error)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(library(option)).

:- meta_predicate(collection(:,+,?,?)).
:- meta_predicate(list(:,+,?,?)).
:- meta_predicate(nvpair(:,+,+,?,?)).
:- meta_predicate(pair(:,+,+,?,?)).
:- meta_predicate(proof(:,+,?,?)).
:- meta_predicate(set(:,+,?,?)).
:- meta_predicate(tuple(:,+,?,?)).



%! collection(+Options:list(nvpair), +Collection:list) is det.
% The following options are supported:
%   1. =|begin(+Begin:atom)|=
%   2. =|end(+End:atom)|=
%   3. =|separator(+Separator:atom)|=
%   4. =|ordering(:Pred)|=
%      The binary predicate that is applied to the collection
%      to determine the order in which its elements occur.
%   5. =|write_method(:Pred)|=
%      The unary predicate that is used for writing the individual items
%      in the collection.

collection(O1, Collection1) -->
  {meta_options(is_meta, O1, O2)},

  % E.g., list -> set.
  {
    option(ordering(P), O2, =),
    once(call(P, Collection1, Collection2))
  },

  % Open a collection.
  {option(begin(Begin), O2)},
  % First we try to include the options.
  % If this DCG rule does not exist, then we exclude the options.
  dcg_catch(
    dcg_call(Begin, O2),
    error(existence_error(procedure,_Predicate1),_Context1),
    phrase(Begin)
  ),

  % The contents of the collection.
  collection_(O2, Collection2),

  % End a collection.
  {option(end(End), O2)},

  % First we try to include the options.
  % If this DCG rule does not exist, then we exclude the options.
  dcg_catch(
    dcg_call(End, O2),
    error(existence_error(procedure,_Predicate2),_Context2),
    phrase(End)
  ).

% Done!
collection_(_O1, []) --> !.
% Nested collection.
collection_(O1, [H1|T]) -->
  {is_list(H1)}, !,

  % Notice that set members that are sets may contain multiple occurrences,
  % since they will first be explicitly converted to ordset format.
  {
    option(ordering(P), O1, =),
    once(call(P, H1, H2))
  },

  collection(O1, H2),

  collection_(O1, T).
% Next set member.
collection_(O1, [H|T]) -->
  {option(write_method(P), O1, atom)},
  dcg_call(P, H),

  % The separator does not occur after the last collection member.
  {option(separator(Separator), O1)},
  (
    {T == []}, !
  ;
    phrase(Separator)
  ),

  collection_(O1, T).

% Meta-options used by predicates in this module.
is_meta(begin).
is_meta(end).
is_meta(ordering).
is_meta(separator).
is_meta(write_method).

langle(O1) -->
  {option(brackets(ascii), O1, ascii)},
  "<".
langle(O1) -->
  {option(brackets(html), O1, ascii)},
  "&lang;".

%! list(+Options:list(nvpair), +List:list)// is det.
% Lists are printed recursively, using indentation relative to the given
% indentation level.

list(O1, List) -->
  {
    meta_options(is_meta, O1, O2),
    merge_options(
      O2,
      [
        begin(opening_square_bracket),
        end(closing_square_bracket),
        separator(comma)
      ],
      O3
    )
  },
  collection(O3, List).

nvpair(O1, N, V) -->
  {
    meta_options(is_meta, O1, O2),
    merge_options(O2, [begin(''),end(';'),separator(': ')], O3)
  },
  collection(O3, [N,V]).

%! pair(+Options:list(nvpair), +X, +Y)// is det.
% Prints the given pair.
%
% The following options are supported:
%   * =|brackets(+Brackets:oneof([ascii,html]))|=
%     The brackets that are printed for this tuple,
%     either using the ASCII characters `<` and `>` (value `ascii`, default)
%     or using the HTML escape sequences `&lang;` and `&rang;`
%     (value `html`).
%   * The options that are supported for print_collection/2.

pair(O1, X, Y) -->
  {
    merge_options(O1, [begin(langle),end(rangle),separator(comma)], O2),
    meta_options(is_meta, O2, O3)
  },
  collection(O3, [X,Y]).

proof(O1, Proof) -->
  {
    meta_options(is_meta, O1, O2),
    default_option(O2, indent, 0, O3)
  },
  proof_(O3, Proof).

proof_(O1, Proof) -->
  {Proof =.. [Rule,Premises,Conclusion]},

  % Indentation.
  {update_option(O1, indent, succ, I, O2)},
  indent(I),

  % The name of the rule that was used for deduction.
  "[", atom(Rule), "]",

  % Separator between rule name and conclusion.
  space,

  % The conclusion.
  proposition(O1, Conclusion),
  newline,

  % Print premises / subproofs.
  dcg_multi1(proof(O2), Premises).

proposition(O1, Proposition) -->
  {
    option(transformation(Predicate), O1, identity),
    call(Predicate, Proposition, Atom)
  },
  atom(Atom).

rangle(O1) -->
  {option(brackets(ascii), O1, ascii)},
  ">".
rangle(O1) -->
  {option(brackets(html), O1, ascii)},
  "&rang;".

set(O1, List) -->
  {
    merge_options(
      O1,
      [
        begin(opening_curly_bracket),
        end(closing_curly_bracket),
        ordering(list_to_ord_set),
        separator(comma)
      ],
      O2
    ),
    meta_options(is_meta, O2, O3)
  },
  collection(O3, List).

%! tuple(+Options:list(nvpair), +List:list)// is det.
% The following options are supported:
%   * =|brackets(+Brackets:oneof([ascii,html]))|=
%     The brackets that are printed for this tuple,
%     either using the ASCII signs `<` and `>` (value `ascii`, default)
%     or using the HTML escape sequences `&lang;` and `&rang;`
%     (value `html`).
%   * The options that are supported for print_collection/2.

tuple(O1, List) -->
  {
    meta_options(is_meta, O1, O2),
    merge_options(O2, [begin(langle),end(rangle),separator(comma)], O3)
  },
  collection(O3, List).
