:- module(
  ckan_to_rdf,
  [
    ckan_to_rdf/1 % +Options:list(nvpair)
  ]
).

/** <module> CKAN to RDF conversion

Automated CKAN to RDF conversion.

@author Wouter Beek
@version 2014/01
*/

:- use_module(ckan(ckan)).
:- use_module(library(debug)).
:- use_module(library(lists)).



:- meta_predicate(goal_list(3,+,+,-)).
:- meta_predicate(goal_list(3,+,+,+,-)).

goal_list(Goal, Msg, Ins, Outs):-
  length(Ins, L),
  goal_list(Goal, Msg, 1-L, Ins, Outs).

goal_list(_, Msg, L-L, [], []):- !.
goal_list(Goal1, I1-L, [In|Ins], Outs):-
  Goal1 =.. [Pred|Args1],
  append(Args1, [In,Out], Args2),
  Goal2 =.. [Pred|Args2],
  call(Goal2), !,
  debug(ckan, '~a ~d/~d', [Msg,I1,L]),
  I2 is I1 + 1,
  goal_list(Goal1, Msg, I2-L, Ins, Outs).
goal_list(Goal, I1-L, [X|Ins], [X|Outs]):-
  I2 is I1 + 1,
  goal_list(Goal, I2-L, Ins, Outs).

%! ckan_to_rdf(+Options:list(nvpair)) is det.
% @see Options are passed to the predicates in module [ckan].

ckan_to_rdf(O1):-
  format(current_output, 'Begin CKAN-to-RDF conversion.\n', []),

  % Make sure the CKAN site is online.
  site_read(O1), !,

  % Groups
  group_list(O1, _, _, _, _, Groups),
  goal_list(group_show(O1, _), 'Groups', Groups, GroupsRemaining),
  debug(ckan, 'Remaining groups: ~w', [GroupsRemaining]),

  % Licenses.
  license_list(O1, _),

  % Organizations.
  organization_list(O1, _, _, _, _, Organizations),
  goal_list(
    organization_show(O1, _),
    'Organizations',
    Organizations,
    OrganizationsRemaining
  ),
  debug(ckan, 'Remaining organizations: ~w', [RemainingOrganizations]),

  % Packages.
  package_list(O1, _, _, Packages),
  goal_list(package_show(O1, _), 'Packages', Packages, RemainingPackages),
  debug(ckan, 'Remaining packages: ~w', [RemainingPackages]),

  % Revisions.
  revision_list(O1, Revisions),
  goal_list(
    revision_show(O1, _),
    'Revisions',
    Revisions,
    RemaniningRevisions
  ),
  debug(ckan, 'Remaining revisions: ~w', [RemainingRevisions]),

  % Tags.
  tag_list(O1, _, _, _, Tags),
  goal_list(tag_show(O1, _), 'Tags', Tags, RemainingTags),
  debug(ckan, 'Remaining tags: ~w', [RemainingTags]),

  % Users.
  user_list(O1, _, _, Users),
  goal_list(user_show(O1, _), 'Users', Users, RemainingUsers),
  debug(ckan, 'Remaining users: ~w', [RemainingUsers]),

  format(current_output, 'End CKAN-to-RDF conversion.\n', []).

