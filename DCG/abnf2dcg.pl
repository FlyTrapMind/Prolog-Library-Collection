:- module(abnf2dcg, []).

/** <module> ABNF2DCG

Converts ABNF grammars to DCGs.

### Examples of term expansion

~~~{.dcg}
system:goal_expansion(From, To):-
  From =.. [:-,a,B],
  write(found),
  To =.. [:-,b,B].

a:-
  write(a).

system:term_expansion(From, To):-
  From =.. [-->,c,C],
  write(found),
  To =.. [-->,d,C].

c --> [].
~~~

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_multi)).
:- use_module(standards(abnf)).

:- dynamic(term_expansion/2).
:- multifile(term_expansion/2).



% PREDICATES USED IN TERM EXPANSIONS %
% These must appear before term_expansion/2.

%! rule(-Rules:list)//
% Note that one ABNF rule may correspond to multiple DCG rules.

rule(Rules) -->
  rulename(Name),
  spaces,
  "=",
  spaces,
  'num-val'(Name, Rules).

%! 'hex-val'(-ValueRange:pair(nonneg,nonneg))//

'hex-val'(N-N) -->
  "x",
  hexadecimal_number(N).
'hex-val'(N1-N2) -->
  "x",
  hexadecimal_number(N1),
  "-",
  hexadecimal_number(N2).

%! 'num-val'(+Name, -Rules:list)//

%'num-val'(N) --> "%', 'bin-val'(N).
%'num-val'(N) --> "%', 'dec-val'(N).
'num-val'(Name, Rules) -->
  "%",
  'hex-val'(N1-N2),
  (
    {N2 - N1 =< 3}
  ->
    {
      findall(
        Rule,
        (
          between(N1, N2, N),
          Rule =.. [-->,Name,dcg_code(N)]
        ),
        Rules
      )
    }
  ;
    {
      Rule =.. [-->,Name,(dcg_code(C), {between(N1, N2, C)})],
      Rules = [Rule]
    }
  ).

%! rulename(-Name:atom)//

rulename(Name) -->
  'ALPHA'(H),
  dcg_multi(rulename_, 1-_, T, []),
  {atom_codes(Name, [H|T])}.
rulename_(X) --> 'ALPHA'(X).
rulename_(X) --> 'DIGIT'(X).
rulename_(X) --> hyphen(X).



% TERM EXPANSIONS %

system:term_expansion(ABNF, DCGs):-
  ABNF =.. [:-,abnf(Atom),_],
write(Atom),
  atom_codes(Atom, Codes),
  once(phrase(rule(DCGs), Codes)),
write(DCGs).



% TERMS TO BE EXPANDED %

% Resulting DCG:
% ~~~{.dcg}
%'DIGIT'(D, C) --> decimal_digit(D, C).
% ~~~

abnf('MIDGET = %x30-31'):- true.

system:term_expansion(From, To):-
  From =.. [-->,feed,C],
write(found),
  To1 =.. [-->,baboon,C],
  To2 =.. [-->,goon,C],
  To = [To1,To2],
write(To).

feed --> [].

