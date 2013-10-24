:- module(
  tms_print,
  [
    tms_print_justification/3, % +Options:list(nvpair)
                               % +TMS:atom
                               % +Justification:iri
    tms_print_node/3 % +Options:list(nvpair)
                     % +TMS:atom
                     % +Node:iri
  ]
).

/** <module> TMS print

Support for printing (aspects of) a TMS.

@author Wouter Beek
@version 2013/05, 2013/09-2013/10
*/

:- use_module(generics(option_ext)).
:- use_module(generics(print_ext)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdfs(rdfs_label_read)).
:- use_module(tms(tms)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(tms, 'http://www.wouterbeek.com/tms.owl#').

:- rdf_meta(tms_print_justification(+,+,r)).
:- rdf_meta(tms_print_node(+,+,r)).



%! tms_print_justification(
%!   +Options:list(nvpair),
%!   +TMS:atom,
%!   +Justification:iri
%! ) is det.

tms_print_justification(O1, TMS, J):-
  tms_justification(TMS, As, R, C, J),
  default_option(O1, indent, 0, I, O2),
  indent(I),
  write('['), write(R), write(']'),
  write(' '),
  tms_print_node(O2, C),
  nl,
  update_option(O2, indent, succ, _I, O3),
  maplist(tms_print_justification(O3, TMS), As).

%! tms_print_node(+Options:list(nvpair), +Node:iri) is det.

tms_print_node(O, N):-
  option(lang(Lang), O, en),
  rdfs_preferred_label(N, Lang, _PreferredLang, L),
  write(L).

%! tms_print_node(
%!   +Options:list(nvpair),
%!   +TMS:atom,
%!   +Conclusion:iri
%! ) is det.

tms_print_node(O1, TMS, C):-
  default_option(O1, indent, 0, O2),
  tms_print_node_(O2, TMS, C).

tms_print_node_(O, TMS, C):-
  tms_node(TMS, C),
  rdf_has(J, tms:has_consequent, C), !,
  tms_print_justification(O, TMS, J).
tms_print_node_(O, _TMS, C):-
  option(indent(I), O, 0),
  indent(I),
  write('[rdf] '),
  tms_print_node(O, C),
  nl.

