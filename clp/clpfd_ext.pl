:- module(
  clpfd_ext,
  [
    clpfd_copysign/3, % ?Absolute:nonneg
                      % ?Sign:integer
                      % ?Integer:integer
    clpfd_digits/2 % ?Integer:integer
                   % ?Digits:list(between(0,9))
  ]
).

/** <module> CLP(FD) extensions

@author Wouter Beek
@version 2015/06
*/

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(lists)).

:- use_module(plc(dcg/abnf_core_rules)).
:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(math/radix)).

:- multifile(clpfd:run_propagator/2).





clpfd_copysign(Abs, Sg, N):-
  clpfd:make_propagator(clpfd_copysign(Abs, Sg, N), Prop),
  clpfd:init_propagator(Abs, Prop),
  clpfd:init_propagator(Sg, Prop),
  clpfd:init_propagator(N, Prop),
  clpfd:trigger_once(Prop).

% If we do not include the `var/1` cases then the following will
% run out of global stack: `?- phrase(signed_integer(1.1), Cs)`.
% Since 1.1 is not an integer clpfd_copysign/3 should not succeed here.
% Otherwise, integer//1 will be called with `N` uninstantiated,
% which starts generating all integers.
clpfd:run_propagator(clpfd_copysign(Abs, Sg, N), MState):-
  (   integer(N)
  ->  clpfd:kill(MState),
      Abs is abs(N),
      Sg is sign(N)
  ;   integer(Abs),
      integer(Sg)
  ->  clpfd:kill(MState),
      N is copysign(Abs, Sg)
  ;   var(N)
  ->  true
  ;   var(Abs),
      var(Sg)
  ).



%! clpfd_digits(+Integer:integer, +Digits:list(between(0,9))) is semidet.
%! clpfd_digits(+Integer:integer, -Digits:list(between(0,9))) is det.
%! clpfd_digits(-Integer:integer, +Digits:list(between(0,9))) is det.
% ### Example
%
% ```prolog
% year(Y) -->
%   {widgits(Y, [Y1,Y2,Y3,Y4])},
%   '#'(4, 'DIGIT', [Y1,Y2,Y3,Y4], []).
% ```

clpfd_digits(N, Ds):-
  clpfd:make_propagator(clpfd_digits(N, Ds), Prop),
  clpfd:init_propagator(N, Prop),
  maplist(flip_init_propagator(Prop), Ds),
  clpfd:trigger_once(Prop).

clpfd:run_propagator(clpfd_digits(N, Ds), MState):-
  (   (   integer(N)
      ;   maplist(error:has_type(between(0, 9)), Ds)
      )
  ->  clpfd:kill(MState),
      weights_nonneg(Ds, N)
  ;   \+ ground([N|Ds])
  ).





% HELPERS %

flip_init_propagator(Prop, Arg):-
  clpfd:init_propagator(Arg, Prop).
