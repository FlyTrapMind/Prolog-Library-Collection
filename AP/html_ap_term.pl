:- module(
  html_ap_term,
  [
    html_ap_term//1 % +AP_Status:compound
  ]
).

/** <module> HTML AP term

Generates HTML for AP status compound terms.

@author Wouter Beek
@version 2014/01
*/

:- use_module(dcg(dcg_content)). % Used as meta-argument in nvpair//2.
:- use_module(html(html_pl_term)).
:- use_module(library(http/html_write)).



html_ap_term(ap(status(Status),Message)) --> !,
  {atomic_list_concat([ap,Status], '_', Class)},
  html(
    div(class=Class,
    \html_ap_message(Message))
  ).
html_ap_term(PL_Term) -->
  html_pl_term(PL_Term).


% Downloaded.
html_ap_message(download(File)) --> !,
  html(
    div(class=download, [
      div(class=action ,'downloaded'),
      \html_file(File)
    ])
  ).

% Error thrown.
html_ap_message(error(Formal,Context)) --> !,
  html_pl_term(error(Formal,Context)).

% Archive extracted.
html_ap_message(extract_archive(OnFiles)) --> !,
  html(
    div(class=extract_archive, [
      div(class=action, 'extracted archive'),
      \on_files(OnFiles)
    ])
  ).

% Property of files.
html_ap_message(property(OfFiles)) --> !,
  html(
    div(class=property, [
      div(class=action, property),
      \of_files(OfFiles)
    ])
  ).

% RDF conversion.
html_ap_message(rdf_conversion(Files)) --> !,
  html(
    div(class=rdf_conversion, [
      div(class=action, 'to turtle'),
      \html_files(Files)
    ])
  ).

% Other
html_ap_message(Message) -->
  html(
    span(class=ap_message,
      \html_pl_term(Message)
    )
  ).


mime(MIME) -->
  html_mime(MIME).


of_file(of_file(_,nvpair(Property,Value))) -->
  html(
    div(class=of_file,
      \html_nvpair(Property,Value)
    )
  ).


of_files([]) --> !, [].
of_files([H|T]) -->
  of_file(H),
  of_files(T).


on_file(on_file(File,Operation)) -->
  html(
    div(class=on_file, [
      \operation(Operation),
      '@',
      \html_file(File)
    ])
  ).


on_files([]) --> !, [].
on_files([H|T]) -->
  html([
    \on_file(H),
    \on_files(T)
  ]).


operation(Operation) -->
  html(span(class=operation, Operation)).

