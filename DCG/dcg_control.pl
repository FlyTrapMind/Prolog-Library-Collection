:- module(
  dcg_control,
  [
    dcg_all//0,
    dcg_all//1, % -Codes:list(code)
    dcg_switch//2, % +Value
                   % +Map:list
    dcg_switch//3, % +Value
                   % +Map:list
                   % +Default
    dcg_until//2, % :End:dcg
                  % ?Value
    dcg_until//3 % +Options:list(nvpair)
                 % :End:dcg
                 % ?Value
  ]
).

/** <module> DCG control

Control structures implemented in meta-DCG rules.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12
*/

:- use_module(dcg(dcg_generic)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate(dcg_switch(+,+,2,?,?)).
:- meta_predicate(dcg_until(//,?,?,?)).
:- meta_predicate(dcg_until(+,//,?,?,?)).
:- meta_predicate(dcg_until_(+,//,?,?,?)).



dcg_all --> dcg_all(_).

dcg_all([H|T]) --> [H], dcg_all(T).
dcg_all([]) --> [].

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
% @arg Options A list of name-value pairs.
% @arg DCG_End Not an arbitrary DCG body, since disjunction
%      does not play out well.
% @arg Value Either an atom or a list of codes (see options).

dcg_until(O1, DCG_End, Out) -->
  {var(Out)}, !,
  dcg_until_(O1, DCG_End, Codes),
  {
    option(output_format(OutFormat), O1, codes),
    (
      OutFormat == atom
    ->
      atom_codes(Out, Codes)
    ;
      Out = Codes
    )
  }.
dcg_until(O1, DCG_End, In) -->
  {nonvar(In)}, !,
  {
    option(output_format(OutFormat), O1, codes),
    (
      OutFormat == atom
    ->
      atom_codes(In, Codes)
    ;
      Codes = In
    )
  },
  dcg_until_(O1, DCG_End, Codes).

dcg_until_(O1, DCG_End, EndCodes), InclusiveExclusive -->
  DCG_End, !,
  {
    option(end_mode(EndMode), O1, exclusive),
    (
      EndMode == inclusive
    ->
      InclusiveExclusive =.. [void,_,_],
      phrase(DCG_End, EndCodes)
    ;
      InclusiveExclusive = DCG_End,
      EndCodes = []
    )
  }.
dcg_until_(O1, DCG_End, [H|T]) -->
  [H],
  dcg_until_(O1, DCG_End, T).
