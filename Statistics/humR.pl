:- module(
  humR,
  [
    r_comp_web/4, % +Predicate1:iri
                  % +Predicate2:iri
                  % ?NumberOfColumns:positive_integer
                  % -DOM:list
    r_web/3 % +Predicate:iri
            % ?NumberOfColumns:positive_integer
            % -DOM:list
  ]
).

/** <module> humR

R-statistics generation for humanities data.

@author Wouter Beek
@tbd Use datatype ordering instead of Prolog term ordering.
@version 2013/10
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(real)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).
:- use_module(svg(svg_file)).
:- use_module(xsd(xsd)).

:- meta_predicate(counts_as_member_of_(2,+,+)).

:- rdf_meta(r_comp_web(r,r,?,-)).
:- rdf_meta(r_web(r,?,-)).

:- debug(humR).



r_comp_web(P11, P21, _N1, SVG):-
  rdf_global_id(P11, P12),
  rdf_global_id(P21, P22),
  once(rdf_has(SomeS1, P12, SomeO1)),
  once(rdf_has(SomeS2, P12, SomeO2)),
  resource_class(SomeS1, S_Class),
  resource_class(SomeS2, S_Class),
  resource_class(SomeO1, O_Class),
  resource_class(SomeO2, O_Class),
  with_output_to(atom(Y_Axis), rdf_term_name([], S_Class)),
  with_output_to(atom(X_Axis), rdf_term_name([], O_Class)),
  setoff(
    O,
    (rdf_has(_S1, P12, O), once(rdf_has(_S2, P22, O))),
    Os1
  ),
  findall(
    NumberOfS1,
    (
      member(O, Os1),
      aggregate_all(count, rdf_has(_, P12, O), NumberOfS1)
    ),
    Bars1
  ),
  findall(
    NumberOfS2,
    (
      member(O, Os1),
      aggregate_all(count, rdf_has(_, P22, O), NumberOfS2)
    ),
    Bars2
  ),
  maplist(rdf_term_name([]), Os1, Os2),
  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  with_output_to(atom(Legend1), rdf_term_name([], P12)),
  with_output_to(atom(Legend2), rdf_term_name([], P22)),
  format(atom(Main), '~w vs ~w', [Legend1,Legend2]),
  <- barplot(
    [Bars1,Bars2],
    % Columns are not stacked on top of each other,
    % but are placed beside each other.
    beside='TRUE',
    % Scaling of the font size of x-axis labels.
    cex..names=0.8,
    % Colors help distinguish between the compared properties.
    col=[blue,red],
    % Labels perpendicular to axis.
    las=2,
    % Logarithmic scale.
    %%%%log=+y,
    % Caption.
    main=+Main,
    % Text labels for x-axis.
    names..arg=Os2,
    ylab=+Y_Axis
  ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+X_Axis, line=5),
  <- legend(
    % Legend position.
    +topleft,
    % Background color.
    bg=+white,
    % Text for the legend
    legend=[Legend1,Legend2],
    % Legend color map.
    fill=[blue,red]
  ),
  <- dev..off(.),
  file_to_svg(File, SVG).

%! r_web(+Predicate:iri, ?NumberOfColumns:positive_integer, -SVG:list) is det.
% This plots the subject-object pairs that occur in triples
% with the given predicate.
%
% The subjects are plotted on the Y axis.
% The objects are plotted on the X axis.

r_web(P11, N1, SVG):-
  rdf_global_id(P11, P12),

  % Settings.
  default(N1, 15, N2),

  % Axis labels.
  once(rdf_has(SomeS, P12, SomeO)),
  axis_label(SomeO, X_Axis),
  axis_label(SomeS, Y_Axis),

  % Collect the unique object terms and extract their values.
  setoff(O, rdf_has(_, P12, O), Os1),
  %%%%terms_to_values(Os1, O_Type, Os2),
  gtrace,
  % Totally ordered domains need to be discretzed before being represented
  % using bars.
  (
    is_total_order(SomeO)
  ->
    discretize(Os1, N2, Os3)
  ;
    Os3 = Os1
  ),

  % Contruct the bars by counting the number of subjects for
  % the given discretized object value scale.
  construct_bars(P12, O_Type, Os3, Bars),
  
  % Labels for the discretized intervals on the X axis.
  maplist(interval_label(O_Type), Os3, Os4),

  % Caption for the figure.
  with_output_to(codes(Main), rdf_term_name([], P12)),
  
  % Colors.
  length(Bars, NumberOfBars),
  
  % Do it!
  absolute_file_name(test, File, [access(write),extensions([svg])]),
  <- svg(+File),
  <- barplot(
    Bars,
    % Scaling of the font size of x-axis labels.
    cex..names=0.8,
    col=rainbow(NumberOfBars),
    % Number of stripes in columns.
    % Incompatible with logarithmic scale!
    %%%%density=10,
    % Labels perpendicular to axis.
    las=2,
    % Logarithmic scale.
    %log=+y,
    % Caption text.
    main=+Main,
    % Text labels for x-axis.
    names..arg=Os4,
    ylab=+Y_Axis
  ),
  % Label for x-axis needs special positioning.
  <- title(xlab=+X_Axis, line=5),
  <- legend(
    +topleft,
    fill=rainbow(NumberOfBars),
    legend=Os4
  ),
  <- dev..off(.),
  file_to_svg(File, SVG).



% SUPPORT PREDICATES %

%! axis_label(+Resource:or([bnode,iri,literal]), -Label:atom) is det.
% Returns an atomic label for resources _|such as|_ the given resource.

axis_label(R, Label):-
  resource_class(R, Class),
  rdf_term_name([], Class, Label).

%! construct_bars(
%!   +Predicate:iri,
%!   +Type:or([oneof([term]),iri]),
%!   +Values:or([list(ordset),ordset]),
%!   -Bars:list(nonneg)
%! ) is det.
% Bars are constructed by counting the number of subjects that occur
% for the given predicate and object pairs.
%
% The object terms are either drawn from a set or from a list of sets.
% In the latter case a count occurs if the found object term is in the
% object term set.

construct_bars(P, _Type, O_Sets, Bars):-
  findall(
    NumberOfS,
    (
      member(O_Set, O_Sets),
      aggregate_all(
        count,
        (
          rdf_has(_, P, O_Term), memberchk(O_Term, O_Set)
          %%%%term_to_value(O_Term, O_Value),
          %%%%counts_as_member_of(Type, O_Value, O_ValueOrValues)
        ),
        NumberOfS
      )
    ),
    Bars
  ).

%! counts_as_member_of(
%!   +Type:or([oneof([term]),iri]),
%!   +Value,
%!   +ValueOrValues
%! ) is semidet.
% Succeeds if the former value counts as an occurrence of the latter value.
% The latter argument is either a single value or a (discretized) range
% of values.

counts_as_member_of(term, O1, O2):-
  counts_as_member_of_(@=<, O1, O2).
counts_as_member_of(Type, O1, O2):-
  xsd_order(Type, LEQ),
  counts_as_member_of_(LEQ, O1, O2).

counts_as_member_of_(LEQ, O, Os):-
  is_list(Os), !,
  first(Os, O1),
  call(LEQ, O1, O),
  last(Os, O2),
  call(LEQ, O, O2).
counts_as_member_of_(LEQ, O1, O2):-
  call(LEQ, O1, O2),
  call(LEQ, O2, O1).

%! determine_range(
%!   +Min:integer,
%!   +Max:integer,
%!   -Begin:integer,
%!   -End:integer,
%!   -Step:nonneg
%! ) is det.

determine_range(Min, Max, Begin, End, Step):-
  Min =< Max,
  Diff is Max - Min,
  Step is 10 ** ceil(log10(Diff / 100)),
  Begin is floor(Min / Step) * Step,
  End is ceil(Max / Step) * Step.

%! discretize(
%!   +Values:ordset,
%!   +Intervals:positive_integer,
%!   -Discretized:list(ordset)
%! ) is det.
% Discretized the given set of totally ordered values.

discretize(Set, N, Disc):-
  split_list_by_number_of_sublists(Set, N, Disc).

%! interval_label(
%!   +Type:or([oneof([term]),iri]),
%!   +ValueOrValues,
%!   -Label:atom
%! ) is det.
% Returns a descriptive label for the given set of values.
%
% Single values are considered to be intervals of length 1.
% In these cases the label of this single value is given.

interval_label(Type, Set, Label):-
  is_list(Set),
  length(Set, Length),
  Length > 1, !,
  first(Set, A1),
  last(Set, Z1),
  maplist(value_to_label(Type), [A1,Z1], [A2,Z2]),
  format(atom(Label), '~w..~w', [A2,Z2]).
interval_label(Type, [SingleValue], Label):- !,
  interval_label(Type, SingleValue, Label).
interval_label(_Type, [], ''):- !.
interval_label(Type, SingleValue, Label):-
  value_to_label(Type, SingleValue, Label).

%! is_total_order(+Resource:iri) is semidet.
% Succeeds if resources _|such as|_ the given resource are totally ordered.

is_total_order(R):-
  resource_class(R, C),
  rdf_memberchk(C, [xsd:decimal,xsd:gYear,xsd:integer]).

%! resource_class(+Resource:or([bnode,iri,literal]), -Class:iri) is det.
% Returns the most descriptive RDFS class for the given resource.
%
% The *|most descriptive|* class is the one of which the resource is
% a direct member, i.e. subclasses of the most descriptive class do not
% have the resource as an instance.
%
% Since RDFS reasoning (with datatype extension) is not 100% yet,
% we include some simple hacks here.

resource_class(R, C):-
  rdf_is_typed_literal(R), !,
  rdf_typed_literal(R, C, _).
resource_class(R, C):-
  rdf(R, rdf:type, C), !.
resource_class(R, C):-
  rdfs_individual(m(t,f,f), R, C, _).

%! term_to_value(
%!   +RDF_Term:or([bnode,iri,literal]),
%!   -Value:or([atom,boolean,compound,float,integer,iri])
%! ) is det.

% For typed literals we need the values they denote to work with
% their total order.
term_to_value(T, V):-
  rdf_is_typed_literal(T), !,
  T = literal(type(D, LEX)),
  xsd_lexicalMap(D, LEX, V).
% For blank nodes, iris, and plain literals we use the Prolog order on terms.
term_to_value(T, T).

%! terms_to_values(
%!   +Terms:ordset,
%!   -Type:or([oneof([term]),iri]),
%!   -Values:ordset
%! ) is det.

terms_to_values(Ts, Type, Vs2):-
  % We assuem that all terms have the same ordering relation and
  % the same type.
  first(Ts, SomeT),

  % Determine the type of the term.
  % This is either the datatype of a typed literal or `term`.
  (
    rdf_is_typed_literal(SomeT)
  ->
    SomeT = literal(type(Type, _LEX))
  ;
    Type = term
  ),

  % Translate terms to values.
  maplist(term_to_value, Ts, Vs1),

  % We need to sort again, because differen terms may denote the same value.
  sort(Vs1, Vs2).

%! value_to_label(-Type:or([oneof([term]),iri]), +Value, -Label:atom) is det.

value_to_label(term, V, L):- !,
  rdf_term_name([], V, L).
value_to_label(D, V, Label):-
  xsd_canonicalMap(D, V, LEX),
  atom_codes(Label, LEX).

