:- module(
  deb_ext,
  [
    fail_mode/1, % +FailMode:compound
    if_debug/2, % +Flag:atom
                % :Goal
    test/2, % +Goal:term
            % +Stream
    test/3 % +Goal:term
           % +TestDescription:atom
           % +Stream
  ]
).

/** <module> Debug extensions

Extensions for debugging and running in debug mode.

Methods that are used while developing and inspecting code.

@author Wouter Beek
@version 2011/11-2012/07, 2012/09, 2013/06, 2013/10, 2013/12-2014/01
*/

:- use_module(library(debug)).

:- meta_predicate(if_debug(+,:)).
:- meta_predicate(test(0,+)).
:- meta_predicate(test(0,+,+)).



fail_mode(debug(Category-Format-Args)):- !,
  debug(Category, Format, Args).
fail_mode(error(E)):- !,
  throw(E).
fail_mode(fail):- !,
  fail.
fail_mode(ignore):-
  true.


%! if_debug(+Flag:atom, :Goal) .
% Calls the given goal only if the given flag is an active debugging topic.
%
% @see library(debug)

if_debug(Flag, _Goal):-
  debugging(Flag, false), !.
if_debug(_Flag, Goal):-
  call(Goal).


%! test(:Goal, +Stream) is det.
% Runs the given goal as a test.
%! test(:Goal, +TestName:atom, +Stream) is det.
% Runs a test that is the given goal.
%
% @arg Goal A compound term that is the goal that is tested.
% @arg TestName The name of the test.
% @arg Stream The stream to which the test results are written.

test(Goal, Stream):-
  term_to_atom(Goal, TestName),
  test(Goal, TestName, Stream).
test(Goal, TestName, Stream):-
  get_time(BeginTime),
  catch(
    (
      call(Goal)
    ->
      Status = 'OK'
    ;
      Status = 'FAILED'
    ),
    _Catcher,
    Status = 'ERROR'
  ),
  get_time(EndTime),
  DeltaTime is EndTime - BeginTime,
  format(
    Stream,
    'Test: ~w. Status: ~w. Time taken: ~2f.\n',
    [TestName,Status,DeltaTime]
  ),
  flush_output(Stream).

