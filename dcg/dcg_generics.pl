:- module(
  dcg_generics,
  [
    dcg_all//0,
    dcg_all//2, % +Options:list(nvpair)
                % -Result:or([atom,list(code)])
    dcg_between//2, % :Between
                    % :Dcg
    dcg_between//3, % :Begin
                    % :Dcg
                    % :End
    dcg_copy//0,
    dcg_done//0,
    dcg_end//0,
    dcg_separated_list//2, % :Separator:callable
                           % ?Codess:list(list(codes))
    dcg_phrase/2, % :Dcg
                  % ?AtomicOrCodes:or([atom,list(code),number])
    dcg_phrase/3, % :Dcg
                  % ?AtomicOrCodes1:or([atom,list(code),number])
                  % ?AtomicOrCodes2:or([atom,list(code),number])
    dcg_rest//1, % -Rest:list(code)
    dcg_switch//2, % +Value
                   % +Map:list
    dcg_switch//3, % +Value
                   % +Map:list
                   % +Default
    dcg_until//2, % :End
                  % ?Value
    dcg_until//3, % :End
                  % ?Value
                  % +Options:list(nvpair)
    dcg_void//0,
    dcg_with_output_to/2, % +Output:compound
                          % :Dcg
    dcg_yn_separator//2 % +Tail:list
                        % :Separator
  ]
).

/** <module> DCG: Generics

Generic support for DCG rules.

Concepts
========

  - Lexical analysis
  - Tokenization
    The process of converting characters to tokens
    (i.e., strings of characters).

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2014/01, 2014/03, 2014/05, 2014/10, 2014/12
*/

:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).

:- use_module(plc(generics/code_ext)).

is_meta(convert).

:- meta_predicate(dcg_between(//,//,?,?)).
:- meta_predicate(dcg_between(//,//,//,?,?)).
:- meta_predicate(dcg_phrase(//,?)).
:- meta_predicate(dcg_phrase(//,?,?)).
:- meta_predicate(dcg_separated_list(//,?,?,?)).
:- meta_predicate(dcg_separated_list_nonvar(//,+,?,?)).
:- meta_predicate(dcg_separated_list_var(//,-,?,?)).
:- meta_predicate(dcg_switch(+,+,2,?,?)).
:- meta_predicate(dcg_until(//,?,?,?)).
:- meta_predicate(dcg_until(//,?,:,?,?)).
:- meta_predicate(dcg_until0(//,?,+,?,?)).
:- meta_predicate(dcg_with_output_to(+,//)).
:- meta_predicate(dcg_yn_separator(+,//,?,?)).

:- predicate_options(dcg_until//2, 2, [
     convert(+callable),
     end_mode(+oneof([exclusive,inclusive]))
   ]).





%! dcg_all// is det.
%! dcg_all(+Options:list(nvpair), -Result:or([atom,list(code)]))// is det.
% The following options are available:
%   - `output_format(+Format:oneof([atom,codes]))`

dcg_all -->
  dcg_all([], _).

dcg_all(O1, Result) -->
  dcg_all_(Codes),
  {(
    option(output_format(atom), O1, codes)
  ->
    atom_codes(Result, Codes)
  ;
    Result = Codes
  )}.

dcg_all_([H|T]) -->
  [H],
  dcg_all_(T).
dcg_all_([]) -->
  [].



%! dcg_between(:Between, :Dcg)// .

dcg_between(Between, Dcg) -->
  dcg_between(Between, Dcg, Between).

%! dcg_between(:Begin, :Dcg, :End)// .

dcg_between(Begin, Dcg, End) -->
  Begin,
  Dcg,
  End.



dcg_copy(X, X).



dcg_done(_, _).



dcg_end([], []).



%! dcg_phrase(:DCG, ?AtomicOrCodes:or([atom,list(code),number]))// is nondet.
%! dcg_phrase(
%!   :DCG,
%!   ?AtomicOrCodes1:or([atom,list(code),number]),
%!   ?AtomicOrCodes2:or([atom,list(code),number])
%! )// is nondet.

dcg_phrase(DCG, X1):-
  nonvar(X1), !,
  atomic_codes(X1, X2),
  phrase(DCG, X2).
dcg_phrase(DCG, X1):-
  phrase(DCG, X2),
  atomic_codes(X1, X2).

dcg_phrase(DCG, X1, Y1):-
  atomic_codes(X1, X2),
  phrase(DCG, X2, Y2),
  atomic_codes(Y1, Y2).



%! dcg_rest(-Rest:list(code))// is det.

dcg_rest(X, X, []).



%! dcg_separated_list(
%!   +Separator:dcg_rule,
%!   ?CodeLists:list(list(code))
%! )// is det.
% @tbd This does not work for the following string:
% ```
% "error(permission_error(delete,file,\'c:/users/quirinus/.webqr/export.svg\'),context(system:delete_file/1,\'Permission denied\'))"
% ```

dcg_separated_list(Sep, L) -->
  {nonvar(L)}, !,
  dcg_separated_list_nonvar(Sep, L).
dcg_separated_list(Sep, L) -->
  {var(L)}, !,
  dcg_separated_list_var(Sep, L).

dcg_separated_list_nonvar(_, [H]) --> !,
  H.
dcg_separated_list_nonvar(Sep, [H|T]) -->
  H,
  Sep,
  dcg_separated_list_nonvar(Sep, T).

dcg_separated_list_var(Sep, [H|T]) -->
  dcg_until(Sep, H, [end_mode(exclusive)]),
  Sep, !,
  dcg_separated_list_var(Sep, T).
dcg_separated_list_var(_Sep, [H]) -->
  dcg_all([], H), !.



%! dcg_switch(+Value, +Maps:list)// is det.

dcg_switch(Value, Maps) -->
  dcg_switch(Value, Maps, dcg_end).

%! dcg_switch(+Value, +Map:list, +Default)// is det.

dcg_switch(Value, Map, _Default) -->
  {member(Value-Goal, Map)}, !,
  % Make sure the variables in the goal are bound outside the switch call.
  phrase(Goal).
dcg_switch(_Value, _Map, Default) -->
  % Make sure the variables in the goal are bound outside the switch call.
  phrase(Default).



%! dcg_until(:End, ?Value)// is det.
%! dcg_until(:End, ?Value, +Options:list(nvpair))// is det.
% Returns the codes that occur before End can be consumed.
%
% The following options are supported:
%   - `convert`(+callable)
%     If given, an arbitrary conversion applied to Value.
%   - `end_mode(?oneof([exclusive,inclusive]))`
%     Whether the codes that satisfy End are included in
%     (`inclusive`) or excluded from (`exclusive`, default) Value.

dcg_until(End, Value) -->
  dcg_until(End, Value, []).

dcg_until(End, Value_out, Options1, X, Y):-
  meta_options(is_meta, Options1, Options2),
  option(convert(Conv), Options2, =),
  option(end_mode(EndMode), Options2, exclusive),
  (   var(X)
  ->  call(Conv, Value_in, Value_out),
      dcg_until0(End, Value_in, EndMode, X, Y)
  ;   dcg_until0(End, Value_in, EndMode, X, Y),
      call(Conv, Value_in, Value_out)
  ).

dcg_until0(End, Codes, EndMode), InclusiveExclusive -->
  End, !,
  {(  EndMode == inclusive
  ->  InclusiveExclusive = dcg_void,
      % This returns the correct list of codes.
      phrase(End, Codes)
  ;   InclusiveExclusive = End,
      Codes = []
  )}.
dcg_until0(End, [H|T], Options) -->
  [H],
  dcg_until0(End, T, Options).



dcg_void --> [].



%! dcg_with_output_to(+Output:compound, :DCG) is det.

dcg_with_output_to(Out, DCG):-
  once(phrase(DCG, Codes)),
  with_output_to(Out, put_codes(Codes)).



%! dcg_yn_separator(+Tail:list, :Separator)// .
% Decides whether a separator is needed or not for the given tail.

dcg_yn_separator([], _) --> [].
dcg_yn_separator([_|_], Separator) --> Separator.
