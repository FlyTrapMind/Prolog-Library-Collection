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

% MULTIPLE OCCURRENCES
    dcg_list//1, % +DCG_Bodies:list(dcg)
    dcg_list//2, % +DCG_Bodies:list(dcg)
                 % :Separator:dcg
    dcg_multi//1, % :DCG_Body:dcg
    dcg_multi//2, % :DCG_Body:dcg
                  % ?Occurrences:or([integer,between/3])
    dcg_multi//3, % :DCG_Body:dcg
                  % ?Occurrences:or([integer,between/3])
                  % ?Arguments1:list
    dcg_multi//4, % :DCG_Body:dcg
                  % ?Occurrences:or([integer,between/3])
                  % ?Arguments1:list
                  % ?Arguments2:list
    dcg_multi_atom//3, % :DCG_Body
                       % ?Occurrences
                       % ?Value:atom
    dcg_multi_list//2, % :DCG_Body:dcg
                       % +List:list
    dcg_multi_list//3, % :DCG_Body:dcg
                       % :DCG_Separator
                       % +List:list

% PARSE TREES
    parse_tree/3, % +TreeName:atom
                  % +SubTrees:list
                  % -Tree:compound

% PEEK
    dcg_peek//1, % :DCG_Body
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
    atom//1,
    string//1,
    string_without//2
  ]
).

/** <module>

Generic DCG clauses.

Shall we use DCGs for parsing or REs?

We need DCGs, because they allow us to define a complex grammar in a
modular way.

@author Wouter Beek
@tbd The combination of meta_predicate/1 and rdf_meta/1.
@tbd The combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/08
*/

:- use_module(dcg(dcg_content)).
:- use_module(generics(cowspeak)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(math(math_ext)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).

% ALL/UNTIL
:- meta_predicate(dcg_until(+,//,?,?,?)).
:- meta_predicate(dcg_until_(+,//,?,?,?)).
:- meta_predicate(dcg_until__(+,//,?,?,?)).
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
% MULTIPLE OCCURRENCES
:- meta_predicate(dcg_list(//,?,?)).
:- meta_predicate(dcg_list(//,//,?,?)).
:- meta_predicate(dcg_list_(+,+,//,?,?)).
:- meta_predicate(dcg_multi(//,?,?)).
:- meta_predicate(dcg_multi(2,?,?)).
:- meta_predicate(dcg_multi(//,?,?,?)).
:- meta_predicate(dcg_multi(2,?,?,?)).
:- meta_predicate(dcg_multi(//,?,?,?,?)).
:- meta_predicate(dcg_multi(3,?,?,?,?)).
:- meta_predicate(dcg_multi(//,?,?,?,?,?)).
:- meta_predicate(dcg_multi(4,?,?,?,?,?)).
:- meta_predicate(dcg_multi_atom(//,?,?,?,?)).
:- meta_predicate(dcg_multi_list(3,+,?,?)).
:- meta_predicate(dcg_multi_list(3,//,+,?,?)).
:- meta_predicate(dcg_multi_nonvar(2,+,+,-,?,?)).
:- meta_predicate(dcg_multi_nonvar(//,+,+,-,?,?)).
:- meta_predicate(dcg_multi_nonvar(3,+,+,-,?,?,?)).
:- meta_predicate(dcg_multi_nonvar(//,+,+,-,?,?,?)).
:- meta_predicate(dcg_multi_nonvar(4,+,+,-,?,?,?,?)).
:- meta_predicate(dcg_multi_nonvar(//,+,+,-,?,?,?,?)).
:- meta_predicate(dcg_multi_var(2,?,?,?)).
:- meta_predicate(dcg_multi_var(//,?,?,?)).
:- meta_predicate(dcg_multi_var(3,?,?,?,?)).
:- meta_predicate(dcg_multi_var(//,?,?,?,?)).
:- meta_predicate(dcg_multi_var(4,?,?,?,?,?)).
:- meta_predicate(dcg_multi_var(//,?,?,?,?,?)).
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
:- meta_predicate(dcg_until(//,?,?,?)).

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
% @arg Options A list of name-value pairs.
% @arg DCG_End Not an arbitrary DCG body, since disjunction
%      does not play out well.
% @arg Value Either an atom or a list of codes (see options).

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

dcg_separated_list(Separator, L) -->
  {nonvar(L)}, !,
  dcg_separated_list_nonvar(Separator, L).
dcg_separated_list(Separator, L) -->
  {var(L)}, !,
  dcg_separated_list_var(Separator, L).
dcg_separated_list_nonvar(Separator, [H|T]) -->
  H,
  dcg_separated_list_nonvar(Separator, T).
dcg_separated_list_var(Separator, [H|T]) -->
  string(H),
  blanks, Separator, blanks, !,
  dcg_separated_list(Separator, T).
dcg_separated_list_var(_Separator, [H]) -->
  string(H).
dcg_separated_list_var(_Separator, []) --> [].



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



% MULTIPLE OCCURRENCES %

dcg_list(L) -->
  dcg_list(L, dcg_void).

dcg_list(Mod:L, Sep) -->
  dcg_list_(Mod, L, Sep).

dcg_list_(_Mod, [], _Sep) --> [].
dcg_list_(Mod, [H], _Sep) --> !, Mod:H.
dcg_list_(Mod, [H|T], Sep) -->
  Mod:H, Sep,
  dcg_list_(Mod, T, Sep).

%! dcg_multi(:DCG_Body)//

dcg_multi(DCG_Body) -->
  dcg_multi(DCG_Body, _N).

%! dcg_multi(:DCG_Body, ?Occurrences)//
% Counts the consecutive occurrences of the given DCG body.
% Or produces the given number of occurrences of the given DCG body.
%
% The nesting of applications of dcg_multi//2 is allowed (see example).
%
% ### Example
%
% ~~~
% ?- phrase(dcg_multi(dcg_multi(one, 3), 4, X), Y).
% X = ["111", "111", "111", "111"],
% Y = "111111111111".
% ~~~
%
% @arg DCG_Body A DCG rule.
% @arg Occurrences When  instantiated, either an integer or a compound term
%      of the form=|`between(+Min:integer,+Max:integer)|=.
%      When uninstantiated, this can instantiate with an integer.

dcg_multi(DCG_Body, Occurrences) -->
  {nonvar(Occurrences)}, !,
  {dcg_multi_occurrences(Occurrences, Min, Max)},
  dcg_multi_nonvar(DCG_Body, Max, 0, Count),
  {dcg_multi_between(Min, Max, Count)}.
dcg_multi(DCG_Body, N) -->
  {var(N)}, !,
  dcg_multi_var(DCG_Body, N).

%! dcg_multi(:DCG_Body, ?Occurrences, ?Arguments1:list)//

dcg_multi(DCG_Body, Occurrences, A1s) -->
  {nonvar(Occurrences)}, !,
  {dcg_multi_occurrences(Occurrences, Min, Max)},
  dcg_multi_nonvar(DCG_Body, Max, 0, Count, A1s),
  {dcg_multi_between(Min, Max, Count)}.
dcg_multi(DCG_Body, N, A1s) -->
  {var(N)}, !,
  dcg_multi_var(DCG_Body, N, A1s).

dcg_multi(DCG_Body, Occurrences, A1s, A2s) -->
  {nonvar(Occurrences)}, !,
  {dcg_multi_occurrences(Occurrences, Min, Max)},
  dcg_multi_nonvar(DCG_Body, Max, 0, Count, A1s, A2s),
  {dcg_multi_between(Min, Max, Count)}.
dcg_multi(DCG_Body, N, A1s, A2s) -->
  {var(N)}, !,
  dcg_multi_var(DCG_Body, N, A1s, A2s).

%! dcg_multi_atom(:DCG_Body, ?Occurrences, ?Value:atom)//
% ### Example
%
% ~~~
% ?- phrase(dcg_multi(dcg_multi_atom(one, 3), 4, X), Y).
% X = ['111', '111', '111', '111'],
% Y = "111111111111".
% ~~~
%
% @see Like dcg_multi//3, but returns an atom.

dcg_multi_atom(DCG_Body, Occurrences, Atom) -->
  {nonvar(Atom)}, !,
  {atom_codes(Atom, Codes)},
  dcg_multi(DCG_Body, Occurrences, Codes).
dcg_multi_atom(DCG_Body, Occurrences, Atom) -->
  dcg_multi(DCG_Body, Occurrences, Codes),
  {atom_codes(Atom, Codes)}.

dcg_multi_between(Min, Max, N):-
  var(Min), !,
  dcg_multi_between(0, Max, N).
dcg_multi_between(Min, Max, N):-
  var(Max), !,
  dcg_multi_between(Min, inf, N).
dcg_multi_between(Min, Max, N):-
  between(Min, Max, N).

dcg_multi_nonvar(_DCG_Body, N, SolC, SolC) -->
  {N == 0}, !, [].
dcg_multi_nonvar(DCG_Body, N, C, SolC) -->
  dcg_call(DCG_Body), !,
  {dcg_multi_pred(N, NewN), NewC is C + 1},
  dcg_multi_nonvar(DCG_Body, NewN, NewC, SolC).
dcg_multi_nonvar(_DCG_Body, _N, SolC, SolC) --> [].

dcg_multi_nonvar(_DCG_Body, N, SolC, SolC, []) -->
  {N == 0}, !, [].
dcg_multi_nonvar(DCG_Body, N, C, SolC, [A1|A1s]) -->
  dcg_call(DCG_Body, A1), !,
  {dcg_multi_pred(N, NewN), NewC is C + 1},
  dcg_multi_nonvar(DCG_Body, NewN, NewC, SolC, A1s).
dcg_multi_nonvar(_DCG_Body, _N, SolC, SolC, []) --> [].

dcg_multi_nonvar(_DCG_Body, N, SolC, SolC, [], []) -->
  {N == 0}, !, [].
dcg_multi_nonvar(DCG_Body, N, C, SolC, [A1|A1s], [A2|A2s]) -->
  dcg_call(DCG_Body, A1, A2), !,
  {dcg_multi_pred(N, NewN), NewC is C + 1},
  dcg_multi_nonvar(DCG_Body, NewN, NewC, SolC, A1s, A2s).
dcg_multi_nonvar(_DCG_Body, _N, SolC, SolC, [], []) --> [].

dcg_multi_occurrences(between(Min,Max), Min, Max):-
  integer(Min), integer(Max), !.
dcg_multi_occurrences(between(_Min,Max), 0, Max):-
  integer(Max), !.
dcg_multi_occurrences(between(Min,_Max), Min, _Inf):-
  integer(Min), !.
dcg_multi_occurrences(N, N, N):-
  integer(N), !.

dcg_multi_pred(inf, inf):- !.
dcg_multi_pred(M,   N  ):-
  N is M - 1.

dcg_multi_var(DCG_Body, N) -->
  DCG_Body,
  dcg_multi_var(DCG_Body, N_),
  {N is N_ + 1}.
dcg_multi_var(_DCG_Body, 0) --> [].

dcg_multi_var(DCG_Body, NewN, [A1|A1s]) -->
  dcg_call(DCG_Body, A1),
  dcg_multi_var(DCG_Body, N, A1s),
  {NewN is N + 1}.
dcg_multi_var(_DCG_Body, 0, []) --> [].

dcg_multi_var(DCG_Body, NewN, [A1|A1s], [A2|A2s]) -->
  dcg_call(DCG_Body, A1, A2),
  dcg_multi_var(DCG_Body, N, A1s, A2s),
  {NewN is N + 1}.
dcg_multi_var(_DCG_Body, 0, [], []) --> [].

%! dcg_multi_list(:DCG_Body, ?List:list)//
% Parses/generates multiple occurrences of a DCG rule based on
% a list of items (one item for each occurrence).

dcg_multi_list(_DCG_Body, []) --> [].
dcg_multi_list(DCG_Body, [H|T]) -->
  dcg_call(DCG_Body, H),
  dcg_multi_list(DCG_Body, T).

%! dcg_multi_list(:DCG_Body, :DCG_Separator, ?List:list)//
% Parses/generates multiple occurrences of a DCG rule based on
% a list of items (one item for each occurrence) with interspersed
% separator content.
%
% @see Like dcg_multi_list//2, but parses/generates the separator DCG
%      in between list items that are parsed/generated according to
%      the DCG body.

dcg_multi_list(_DCG_Body, _Sep, []) --> [].
dcg_multi_list(DCG_Body, _Sep, [H]) -->
  dcg_call(DCG_Body, H).
dcg_multi_list(DCG_Body, Sep, [H|T]) -->
  dcg_call(DCG_Body, H),
  Sep,
  dcg_multi_list(DCG_Body, Sep, T).



% PARSE TREES

%! parse_tree(+TreeName:atom, +SubTrees:list, -Tree:compound)// is det.
% Constructs a tree based on a list of direct subtrees and variables
% (excluded).
%
% The variables come from unused optional rules in the DCG body.
%
% @arg TreeName The atomic name of the grammar rule for which
%      the tree is constructed.
% @arg SubTrees A list of compound terms (direct subtrees)
%      and variables (excluded from the created tree).
% @arg Tree A compound term representing a parse tree.

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

