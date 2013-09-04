:- module(
  assoc_multi,
  [
    get_assoc/3, % ?Key
                 % +Assoc:assoc
                 % ?Value
    put_assoc/3, % +Key
                 % +Name:atom
                 % +Value
    put_assoc/4, % +Key
                 % +OldAssoc:assoc
                 % +Value
                 % ?NewAssoc:assoc
% REGISTRATION
    assoc_by_name/2, % ?Name:atom
                     % ?Assoc:assoc
    register_assoc/2, % ?Name:atom
                      % ?Assoc:assoc
% DEBUG
    write_assoc/1, % +Assoc:assoc
    write_assoc/4 % +Out
                  % +Indent:integer
                  % :KeyTransform
                  % +Assoc:or(assoc,atom)
  ]
).
:- reexport(
  library(assoc),
  except([
    get_assoc/3,
    put_assoc/4
  ])
).

/** <module> ASSOC_MULTI

An association list with multiple values per keys, using ordered sets.

This extends library assoc by overloading get_assoc/3 and put_assoc/4,
and by adding ord_member/2.

@author Wouter Beek
@version 2013/04-2013/05, 2013/07-2013/08
*/

:- use_module(generics(db_ext)).
:- use_module(generics(print_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- meta_predicate(write_assoc(+,+,2,+)).

:- dynamic(assoc_by_name/2).

:- nodebug(assoc_multi).



%! get_assoc(?Key, +Assoc, ?Value) is nondet.

get_assoc(Key, Assoc, Value):-
  assoc:get_assoc(Key, Assoc, Ordset),
  ord_member(Value, Ordset).

%! ord_member(?Member, ?List:list) is nondet.

ord_member(Value, Ordset):-
  nonvar(Value), !,
  ord_memberchk(Value, Ordset).
ord_member(Value, Ordset):-
  member(Value, Ordset).

%! put_assoc(+Key, +AssocName:atom, +Value) is det.

put_assoc(Key, AssocName, Value):-
  setup_call_cleanup(
    retract(assoc_by_name(AssocName, OldAssoc)),
    put_assoc(Key, OldAssoc, Value, NewAssoc),
    assert(assoc_by_name(AssocName, NewAssoc))
  ).

%! put_assoc(+Key, +OldAssoc:assoc, +Value, ?NewAssoc:assoc) is semidet.

% Put the given value into the existing ordset.
put_assoc(Key, OldAssoc, Value, NewAssoc):-
  assoc:get_assoc(Key, OldAssoc, OldOrdset), !,
  ord_add_element(OldOrdset, Value, NewOrdset),
  assoc:put_assoc(Key, OldAssoc, NewOrdset, NewAssoc),
  length(NewOrdset, NewOrdsetLength), %DEB
  debug(
    assoc_multi,
    'Added <~w,~w> to existing assoc of length ~w.',
    [Key, Value, NewOrdsetLength]
  ).
% Create a new ordset.
put_assoc(Key, OldAssoc, Value, NewAssoc):-
  assoc:put_assoc(Key, OldAssoc, [Value], NewAssoc),
  debug(assoc_multi, 'Added <~w,~w> to NEW assoc.', [Key, Value]).

register_assoc(Name, Assoc):-
  db_add_novel(assoc_by_name(Name, Assoc)).

write_assoc(Assoc):-
  write_assoc(user_output, 0, term_to_atom, Assoc).

write_assoc(Out, KeyIndent, KeyTransform, Assoc):-
  is_assoc(Assoc), !,
  assoc_to_keys(Assoc, Keys),
  ValueIndent is KeyIndent + 1,
  forall(
    member(Key, Keys),
    (
      indent(Out, KeyIndent),
      call(KeyTransform, Key, KeyName),
      format(Out, '~w:\n', [KeyName]),
      forall(
        get_assoc(Key, Assoc, Value),
        (
          indent(Out, ValueIndent),
          call(KeyTransform, Value, ValueName),
          format(Out, '~w\n', [ValueName])
        )
      )
    )
  ).
write_assoc(Out, KeyIndent, KeyTransform, AssocName):-
  atom(AssocName), !,
  assoc_by_name(AssocName, Assoc),
  write_assoc(Out, KeyIndent, KeyTransform, Assoc).

