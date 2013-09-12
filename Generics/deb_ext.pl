:- module(
  deb_ext,
  [
    printw/1, % +Term
    test/2, % +Goal:term
            % +Stream
    test/3 % +Goal:term
           % +TestDescription:atom
           % +Stream
  ]
).

/** <module> DEB_EXT

Extensions for debugging and running in debug mode.

Methods that are used while developing and inspecting code.

# SWI-Prolog changelog

~~~
http://www.swi-prolog.org/ChangeLog?branch=stable&from=5.10.4&to=6.4.0
~~~

@author Wouter Beek
@tbd Test this module.
@version 2011/11-2012/07, 2012/09, 2013/06
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- meta_predicate(test(0,+)).
:- meta_predicate(test(0,+,+)).

:- rdf_meta(rdf_class_status(r)).

:- debug(deb_ext).



printw(Atomic):-
  atomic(Atomic), !,
  format(user_output, '~w\n', [Atomic]),
  flush_output(user_output).
printw(Codes):-
  atom_codes(Atom, Codes),
  printw(Atom).

%! test(:Goal, +Stream) is det.
% Runs the given goal as a test.
%
% @param Goal A compound term that is the goal that is tested.
% @param Stream The stream to which the test results are written.
% @see test/3 allows the name of the test to be specified.

test(Goal, Stream):-
  term_to_atom(Goal, TestName),
  test(Goal, TestName, Stream).

%! test(:Goal, +TestName:atom, +Stream) is det.
% Runs a test that is the given goal.
%
% @param Goal A compound term that is the goal that is tested.
% @param TestName The name of the test.
% @param Stream The stream to which the test results are written.

test(Goal, TestName, Stream):-
  get_time(BeginTime),
  catch(
    (
      call(Goal),
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
    [TestName, Status, DeltaTime]
  ),
  flush_output(Stream).

