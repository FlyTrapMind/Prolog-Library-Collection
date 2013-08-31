:- module(
  dcg_generic,
  [
% ALL/UNTIL
    dcg_all//0,
    dcg_all//1, % -Codes:list(code)
    dcg_all_atom//1, % -Atom:atom
    dcg_end//0,
    dcg_until//2, % :End:dcg
                  % ?Value
    dcg_until//3, % +Options:list(nvpair)
                  % :End:dcg
                  % ?Value

% DEBUG
    dcg_debug//0,
    gtrace//0,

% LIST
    dcg_separated_list//2, % :Separator:dcg
                           % ?Codess:list(list(codes))

% META-DCG RULES
    dcg_apply//2, % :DCG_Body
                  % +Arguments:list
    dcg_call//1,
    dcg_call//2,
    dcg_call//3,
    dcg_call//4,
    dcg_call//5,
    dcg_switch//2, % +Value
                   % +Map:list
    dcg_switch//3, % +Value
                   % +Map:list
                   % +Default

% PARSE TREES
    parse_tree/3, % +TreeName:atom
                  % +SubTrees:list
                  % -Tree:compound

% PEEK
    dcg_peek//1, % :DCG_Body:dcg
    dcg_peek_length//2, % ?Length:integer
                        % ?Codes:list(code)

% PHRASE EXTENSION
    dcg_phrase/2, % :DCG_Body:dcg
                  % ?In:atom
    dcg_phrase/3, % :DCG_Body:dcg
                  % +In:atom
                  % -Out:atom

% REPLACE
    dcg_replace//2 % +From:list(code)
                   % +To:list(code)
  ]
).
:- reexport(
  library(dcg/basics),
  [
    string//1, % -Codes:list(code)
    string_without//2 % +End:list(code)
                      % -Codes:list(code)
  ]
).

/** <module>

Generic DCG clauses. DCGs allow the definition of a complex grammar in
a modular way.

## Concepts

  * *|Lexical analysis|*
    *Tokenization*
    The process of converting characters to tokens
    (i.e., strings of characters).

--

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/08
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(cowspeak)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).

% ALL/UNTIL
:- meta_predicate(dcg_until(//,?,?,?)).
:- meta_predicate(dcg_until(+,//,?,?,?)).
:- meta_predicate(dcg_until_(+,//,?,?,?)).
% LIST
:- meta_predicate(dcg_separated_list(//,?,?,?)).
:- meta_predicate(dcg_separated_list_nonvar(//,+,?,?)).
:- meta_predicate(dcg_separated_list_var(//,-,?,?)).
% META-DCG RULES
:- meta_predicate(dcg_apply(//,+,?,?)).
:- meta_predicate(dcg_call(2,?,?)).
:- meta_predicate(dcg_call(3,?,?,?)).
:- meta_predicate(dcg_call(4,?,?,?,?)).
:- meta_predicate(dcg_call(5,?,?,?,?,?)).
:- meta_predicate(dcg_call(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_call(7,?,?,?,?,?,?,?)).
:- meta_predicate(dcg_switch(+,+,2,?,?)).
% PEEK
:- meta_predicate(dcg_peek(//,?,?)).
:- meta_predicate(dcg_peek_length(?,?,?,?)).
:- meta_predicate(dcg_peek_length(+,?,?,?,?)).
% PHRASE EXTENSIONS
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,+,-)).
% REPLACE
:- meta_predicate(dcg_replace(//,//,?,?)).



% ALL/UNTIL %

dcg_all --> dcg_all(_).

dcg_all([]) --> [].
dcg_all([H|T]) --> [H], dcg_all(T).

dcg_all_atom(Atom) -->
  {var(Atom)},
  dcg_all(Codes),
  {atom_codes(Atom, Codes)}.
dcg_all_atom(Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_all(Codes).

%! dcg_until(:DCG_End, ?Value)// is det.
% @see dcg_until//3 with the default options.

dcg_until(DCG_End, Value) -->
  dcg_until([], DCG_End, Value).

%! dcg_until(+Options:list(nvpair), :DCG_End, ?Value)// is det.
% Returns the codes that occur before `DCG_End` can be consumed.
%
% The following options are supported:
%   * =|end_mode(?EndMode:oneof([exclusive,inclusive]))|=
%     Whether the codes that satisfy the DCG rule are included in
%     (`inclusive`) or excluded from (`exclusive`, default) the results.
%   * =|output_format(?OutFormat:oneof([atom,codes]))|=
%     Whether the results should be returned in codes (`codes`, default)
%     or as an atom (`atom`).
%
% @param Options A list of name-value pairs.
% @param DCG_End Not an arbitrary DCG body, since disjunction
%      does not play out well.
% @param Value Either an atom or a list of codes (see options).

dcg_until(O, DCG_End, Out) -->
  {var(Out)}, !,
  dcg_until_(O, DCG_End, Codes),
  {
    option(output_format(OutFormat), O, codes),
    (
      OutFormat == atom
    ->
      atom_codes(Out, Codes)
    ;
      Out = Codes
    )
  }.
dcg_until(O, DCG_End, In) -->
  {nonvar(In)}, !,
  {
    option(output_format(OutFormat), O, codes),
    (
      OutFormat == atom
    ->
      atom_codes(In, Codes)
    ;
      Codes = In
    )
  },
  dcg_until_(O, DCG_End, Codes).

dcg_until_(O, DCG_End, EndCodes), InclusiveExclusive -->
  DCG_End, !,
  {
    option(end_mode(EndMode), O, exclusive),
    (
      EndMode == inclusive
    ->
      InclusiveExclusive = dcg_void,
      phrase(DCG_End, EndCodes)
    ;
      InclusiveExclusive = DCG_End,
      EndCodes = []
    )
  }.
dcg_until_(O, DCG_End, [H|T]) -->
  [H],
  dcg_until_(O, DCG_End, T).



% DEBUG %

gtrace -->
  {gtrace}.

dcg_debug(Codes, []):-
  atom_codes(Atom, Codes),
  format(
    atom(Text),
    'Wouter, I have the feeling that something is wrong here.\n\c
     Unable to phrase <~w>\n',
    [Atom]
  ),
  thread_create(cowspeak(Text), _ID, []),
  gtrace. %DEB



% LIST %

%! dcg_separated_list(
%!   +Separator:dcg_rule,
%!   ?CodeLists:list(list(code))
%! )// is det.
% @tbd This does not work for the following string:
% ~~~
% "error(permission_error(delete,file,\'c:/users/quirinus/.webqr/export.svg\'),context(system:delete_file/1,\'Permission denied\'))"
% ~~~

dcg_separated_list(Sep, L) -->
  {nonvar(L)}, !,
  dcg_separated_list_nonvar(Sep, L).
dcg_separated_list(Sep, L) -->
  {var(L)}, !,
  dcg_separated_list_var(Sep, L).

dcg_separated_list_nonvar(_Sep, [H]) --> !,
  H.
dcg_separated_list_nonvar(Sep, [H|T]) -->
  H,
  Sep,
  dcg_separated_list_nonvar(Sep, T).

dcg_separated_list_var(Sep, [H|T]) -->
  dcg_until([end_mode(exclusive),output_format(codes)], Sep, H),
  Sep, !,
  dcg_separated_list_var(Sep, T).
dcg_separated_list_var(_Sep, [H]) -->
  dcg_all(H).



% META-DCG RULES %

dcg_apply(DCG_Body, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(DCG_Body, Args2).

dcg_call(DCG_Body, X, Y):-
  call(DCG_Body, X, Y).

dcg_call(DCG_Body, A1, X, Y):-
  call(DCG_Body, A1, X, Y).

dcg_call(DCG_Body, A1, A2, X, Y):-
  call(DCG_Body, A1, A2, X, Y).

dcg_call(DCG_Body, A1, A2, A3, X, Y):-
  call(DCG_Body, A1, A2, A3, X, Y).

dcg_call(DCG_Body, A1, A2, A3, A4, X, Y):-
  call(DCG_Body, A1, A2, A3, A4, X, Y).

%! dcg_switch(+Value, +Maps:list)// is det.

dcg_switch(Value, Maps) -->
  dcg_switch(Value, Maps, dcg_end).

%! dcg_switch(+Value, +Map:list, +Default)// is det.

dcg_switch(Value, Map, _Default) -->
  {member(Value-Goal, Map)}, !,
  % Make sure the variables in the goal are bound outside the switch call.
  dcg_call(Goal).
dcg_switch(_Value, _Map, Default) -->
  % Make sure the variables in the goal are bound outside the switch call.
  dcg_call(Default).



% PARSE TREES

%! parse_tree(+TreeName:atom, +SubTrees:list, -Tree:compound)// is det.
% Constructs a tree based on a list of direct subtrees and variables
% (excluded).
%
% The variables come from unused optional rules in the DCG body.
%
% @param TreeName The atomic name of the grammar rule for which
%      the tree is constructed.
% @param SubTrees A list of compound terms (direct subtrees)
%      and variables (excluded from the created tree).
% @param Tree A compound term representing a parse tree.

parse_tree(P, SubT1, T):-
  include(nonvar, SubT1, SubT2),
  T =.. [P | SubT2].



% PEEK %

%! dcg_peek(:DCG_Body)// is det.
% Returns the next code in the codes list, if any.
% Does not consume anything.

dcg_peek(DCG_Body), DCG_Body -->
  DCG_Body.

%! dcg_peek_length(?Length:integer, ?Peek:list(code))// is nondet.

% In order to prevent the generative call of DCG rule to result in an
% infinite loop over increasingly bigger lengths in length/2, we
% explicitly instigate an upper bound for length.
% This upper bound is the length of the code list that is parsed.
dcg_peek_length(Length, Peek, In, Out):-
  length(In, MaxLength),
  dcg_peek_length(MaxLength, Length, Peek, In, Out).
dcg_peek_length(MaxLength, Length, Peek), Peek -->
  {
    % Effectuate the upper bound to length.
    % In the generative call this is the predicate that backtracks, causing
    % the peek list to grow until it reaches the maximum allowed size.
    between(0, MaxLength, Length),
    % The length is always instantiated at this point.
    length(Peek, Length)
  },
  Peek.



% PHRASE EXTENSION

dcg_phrase(DCG_Body, InAtom):-
  var(InAtom), !,
  phrase(DCG_Body, InCodes),
  atom_codes(InAtom, InCodes).
dcg_phrase(DCG_Body, InAtom):-
  atom_codes(InAtom, InCodes),
  phrase(DCG_Body, InCodes).

dcg_phrase(DCG_Body, InAtom, OutAtom):-
  atom(InAtom),
  atom_codes(InAtom, InCodes),
  phrase(DCG_Body, InCodes, OutCodes),
  atom_codes(OutAtom, OutCodes).



% REPLACE %

dcg_end([], []).

%! dcg_replace(:From:dcg, :To:dcg)// is det.
% @author http://stackoverflow.com/users/1613573/mat
% @see http://stackoverflow.com/questions/6392725/using-a-prolog-dcg-to-find-replace-code-review

dcg_replace(_From, _To) -->
  dcg_end, !.
dcg_replace(From, To), To -->
  From, !,
  dcg_replace(From, To).
dcg_replace(From, To), [X] -->
  [X],
  dcg_replace(From, To).

