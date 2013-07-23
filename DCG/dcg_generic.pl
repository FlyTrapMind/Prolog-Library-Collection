:- module(
  dcg_generic,
  [
% ALL/UNTIL
    dcg_all//0,
    dcg_all//1, % -Codes:list(code)
    dcg_all_atom//1, % -Atom:atom
    dcg_end//0,
    dcg_until//2, % :End:dcg
                  % -Codes:list(code)
    dcg_until_atom//2, % :End:dcg
                       % -Atom:atom
    dcg_without//2, % :End:dcg
                    % -Codes:list(code)
    dcg_without_atom//2, % :End:dcg
                         % -Atom:atom

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
    dcg_list//3, % +DCG_Bodies:list(dcg)
                 % :Separator:dcg
                 % +StaticArgument
    dcg_list//4, % +DCG_Bodies:list(dcg)
                 % :Separator:dcg
                 % -Trees:list(compound)
                 % +StaticArguments:list
    dcg_multi//2, % :DCG_Body:dcg
                  % ?Occurrences:integer
    dcg_multi//3, % :DCG_Body:dcg
                  % +Max:integer
                  % +Min:integer
    dcg_multi_list//2, % :DCG_Body:dcg
                       % +List:list
    dcg_multi_list//3, % :DCG_Body:dcg
                       % :DCG_Separator
                       % +List:list
    dcg_void//0,

% PARSE TREES
    parse_tree/3, % +TreeName:atom
                  % +SubTrees:list
                  % -Tree:compound

% PEEK
    dcg_peek//1, % ?X:code
    dcg_peek_atom//1, % -Atom:atom
    dcg_peek_char//1, % ?Char:char
    dcg_peek_length//2, % ?Length:integer
                        % ?Codes:list(code)

% PHRASE EXTENSION
    dcg_phrase/2, % :DCG_Body:dcg
                  % ?In:atom
    dcg_phrase/3, % :DCG_Body:dcg
                  % ?In:atom
                  % ?Out:atom

% RE
    dcg_plus//1, % :DCG_Body:dcg
    dcg_questionmark//1, % :DCG_Body:dcg
    dcg_star//1, % :DCG_Body:dcg

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

REs allow easy quantification using the _Wilcards_ =|*|=, =|+|=, =|?|=,
and the positive integers. This is why we add the DCG rules:
  * dcg_plus//1
  * dcg_questionmark//1
  * dcg_star//1

@author Wouter Beek
@tbd Ask about the combination of meta_predicate/1 and rdf_meta/1.
@tbd Ask about the combination of DCGs (e.g., `//`) and meta-DCGs (e.g., `3`).
@version 2013/05-2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(cowspeak)).
:- use_module(generics(list_ext)).
:- use_module(html(html)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(os(os_ext)).

% The number of spaces that go into one indent.
:- setting(
  indent_size,
  integer,
  2,
  'The default indentation used by the print predicates.'
).

% ALL/UNTIL %
:- meta_predicate(dcg_until(//,-,+,-)).
:- meta_predicate(dcg_until_atom(//,-,+,-)).
:- meta_predicate(dcg_without(//,-,+,-)).
:- meta_predicate(dcg_without_atom(//,-,+,-)).
% LIST %
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
% MULTIPLE OCCURRENCES %
:- meta_predicate(dcg_list(//,?,?)).
:- meta_predicate(dcg_list(//,//,?,?)).
:- meta_predicate(dcg_list(//,//,+,?,?)).
:- meta_predicate(dcg_list(//,//,-,+,?,?)).
:- meta_predicate(dcg_list_(+,+,//,?,?)).
:- meta_predicate(dcg_list_(+,+,//,+,?,?)).
:- meta_predicate(dcg_list_(+,+,//,-,+,?,?)).
:- meta_predicate(dcg_multi(//,?,?,?)).
:- meta_predicate(dcg_multi(//,+,+,?,?)).
:- meta_predicate(dcg_multi_list(3,+,?,?)).
:- meta_predicate(dcg_multi_list(3,//,+,?,?)).
:- meta_predicate(dcg_multi_nonvar(//,?,?,?)).
:- meta_predicate(dcg_multi_var(//,?,?,?)).
% PEEK %
:- meta_predicate(dcg_peek_length(?,?,?,?)).
:- meta_predicate(dcg_peek_length(+,?,?,?,?)).
% PHRASE EXTENSIONS %
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).
% RE %
:- meta_predicate(dcg_plus(//,?,?)).
:- meta_predicate(dcg_questionmark(//,?,?)).
:- meta_predicate(dcg_star(//,?,?)).
% REPLACE %
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

%! dcg_until(+End:dcg, -Codes:list(code))// is det.
% Returns the codes that occur before =End= can be consumed.
%
% =End= is not an arbitrary DCG body, since disjunction does not play out
% well (note that =End= occurs before and fater the =|-->|=).
%
% We enforce determinism after the first occurrence of =End= is consumed.
%
% @tbd There are problems with list elements: meta_predicate/1 cannot
%      identity the modules for DCGs in this case.

dcg_until(End, []), End --> End, !.
dcg_until(End, [H|T]) -->
  [H],
  dcg_until(End, T).

%! dcg_until_atom(+End:dcg, -Atom:atom)// is det.
% @see dcg_until//2

dcg_until_atom(End, Atom) -->
  {var(Atom)},
  dcg_until(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_until_atom(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_until(End, Codes).

%! dcg_without(+End:dcg, -Codes:list(code))// is det.

dcg_without(End, []) --> End, !.
dcg_without(End, [H|T]) -->
  [H],
  dcg_without(End, T).

dcg_without_atom(End, Atom) -->
  {var(Atom)},
  dcg_without(End, Codes),
  {atom_codes(Atom, Codes)}.
dcg_without_atom(End, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_without(End, Codes).



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
%! ) is det.
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

dcg_call(DCG_Body, A1, A2, A3, A4, A5, X, Y):-
  call(DCG_Body, A1, A2, A3, A4, A5, X, Y).

%! dcg_switch(+Value, +Maps:list) is det.

dcg_switch(Value, Maps) -->
  dcg_switch(Value, Maps, dcg_end).

%! dcg_switch(+Value, +Map:list, +Default) is det.

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

%! dcg_list(:DCG_Bodies:list, :DCG_Separator, +Argument)//
% Inserts the same argument as the frist parameter of each DCG_Body.
%
% For example, this is used to insert a shared namespace.

dcg_list(Mod:L, Sep, A1) -->
  dcg_list_(Mod, L, Sep, A1).

%! dcg_list(:DCG_Bodies:list, :DCG_Separator, +Argument)//
% Builds a list of arguments by inserting them as the frist parameter
% of each DCG_Body.
% In addition to that, insertes the same arguments as the second parameter
% of each DCG_Body.
%
% For example, this is used to collect subtrees for each DCG rule,
% while also inserting a shared namespace.

dcg_list(Mod:L, Sep, L, As) -->
  dcg_list_(Mod, L, Sep, L, As).

dcg_list_(_Mod, [], _Sep) --> [].
dcg_list_(Mod, [H], _Sep) --> !, Mod:H.
dcg_list_(_Mod, [H|T], Sep) -->
  Mod:H, Sep,
  dcg_list_(Mod, T, Sep).

dcg_list_(_Mod, [], _Sep, _A1) --> [].
dcg_list_(Mod, [H], _Sep, A1) --> !,
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [A1|Args]).
dcg_list_(_Mod, [H|T], Sep, A1) -->
  {H =.. [P|Args]},
  dcg_apply(Mod:P, [A1|Args]), Sep,
  dcg_list_(Mod, T, Sep, A1).

dcg_list_(_Mod, [], _Sep, [], _As) --> [].
dcg_list_(Mod, [H], _Sep, [Tree], As) --> !,
  {H =.. [P|Args1], append([Tree|As], Args1, Args2)},
  dcg_apply(Mod:P, Args2).
dcg_list_(_Mod, [H|T], Sep, [Tree|Trees], As) -->
  {H =.. [P|Args1], append([Tree|As], Args1, Args2)},
  dcg_apply(Mod:P, Args2), Sep,
  dcg_list_(Mod, T, Sep, Trees, As).

%! dcg_multi(:DCG_Body, ?Occurrences:integer)
% Counts the consecutive occurrences of the given DCG body.
% Or produces the given number of occurrences of the given DCG body.

dcg_multi(DCG_Body, N) -->
  {nonvar(N)}, !,
  dcg_multi_nonvar(DCG_Body, N).
dcg_multi(DCG_Body, N) -->
  {var(N)}, !,
  dcg_multi_var(DCG_Body, N).

dcg_multi(DCG_Body, Max, Min) -->
  {Max >= Min},
  dcg_multi(DCG_Body, Max), !.
dcg_multi(DCG_Body, Max, Min) -->
  {Max >= Min},
  {NewMax is Max - 1},
  dcg_multi(DCG_Body, NewMax, Min).

dcg_multi_nonvar(_DCGBody, 0) --> !, [].
dcg_multi_nonvar(DCG_Body, N) -->
  DCG_Body,
  {NewN is N - 1},
  dcg_multi_nonvar(DCG_Body, NewN).

dcg_multi_var(DCG_Body, N) -->
  DCG_Body, !,
  dcg_multi_var(DCG_Body, N_),
  {N is N_ + 1}.
dcg_multi_var(_DCGBody, 0) --> [].

%! dcg_multi_list(:DCG_Body, +List:list)//
% Parses/generates multiple occurrences of a DCG rule based on
% a list of items (one item for each occurrence).

dcg_multi_list(_DCG_Body, []) --> [].
dcg_multi_list(DCG_Body, [H|T]) -->
  dcg_call(DCG_Body, H),
  dcg_multi_list(DCG_Body, T).

%! dcg_multi_list(:DCG_Body, :DCG_Separator, +List:list)//
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

dcg_void --> [].



% PARSE TREES

%! parse_tree(+TreeName:atom, +SubTrees:list, -Tree:compound) is det.
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

%! dcg_peek(?X:code) is det.
% Returns the next code in the codes list, if any.
% Does not consume anything.

dcg_peek(X), [X] -->
  [X].

%! dcg_peek_atom(+Atom:atom) is semidet.
% Succeeds if the given atom occurs next in the codes list,
% but does not consume anything.

dcg_peek_atom(Atom), Codes -->
  {atom_codes(Atom, Codes)},
  Codes.

%! dcg_peek_char(?Char) is semidet.

dcg_peek_char(Char), [Code] -->
  {nonvar(Char)},
  {char_code(Char, Code)},
  [Code].
dcg_peek_char(Char), [Code] -->
  {var(Char)},
  [Code],
  {char_code(Char, Code)}.

%! dcg_peek_length(?Length:integer, ?Peek:list(code)) is nondet.

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

% Codes match codes.
dcg_phrase(DCG_Body, In):-
  is_list(In), !,
  dcg_phrase(DCG_Body, In, []).
% Atom matches atom.
dcg_phrase(DCG_Body, In):-
  dcg_phrase(DCG_Body, In, '').

dcg_phrase(DCG_Body, In, Out):-
  is_list(In), !,
  phrase(DCG_Body, In, Out).
dcg_phrase(DCG_Body, In1, Out1):-
  atomic(In1), !,
  atom_codes(In1, In2),
  dcg_phrase(DCG_Body, In2, Out2),
  atom_codes(Out1, Out2).



% RE %

%! dcg_plus(:DCG_Body:dcg) is nondet.
% Applies the given DCG_Body one or more times to the codes list.
%
% The longest codes list that can be parsed by the given DCG body
% is returned first. The singleton codes list is retuned last.
%
% @compat Inspired by the regular expression operator =+=.

dcg_plus(DCG_Body) -->
  DCG_Body.
dcg_plus(DCG_Body) -->
  DCG_Body,
  dcg_plus(DCG_Body).

dcg_questionmark(DCG_Body) -->
  (DCG_Body ; "").

%! dcg_star(:DCG_Body:dcg) is nondet.
% Applies the given DCG_Body zero or more times to the codes list.
%
% The longest codes list that can be parsed by the given DCG body
% is returned first. The empty codes list is retuned last.
%
% @compat Inspired by the regular expression operator =*=.

dcg_star(_DCGBody) --> [].
dcg_star(DCG_Body) -->
  DCG_Body,
  dcg_star(DCG_Body).



% REPLACE %

dcg_end([], []).

%! dcg_replace(:From:dcg, :To:dcg) is det.
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

