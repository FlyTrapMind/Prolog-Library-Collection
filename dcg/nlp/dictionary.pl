:- module(
  dictionary,
  [
    random_word/3, % +Language:atom
                   % -Word:atom
                   % -Something:atom
    word/3 % ?Language:atom
           % ?Word:atom
           % ?Something:atom
  ]
).

/** <module> Dictionary

Support for natural language dictionaries.

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(aggregate)).
:- use_module(library(archive)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(persistency)).
:- use_module(library(random)).
:- use_module(library(readutil)).
:- use_module(library(uri)).

:- use_module(dcg(dcg_generic)).
:- use_module(generics(db_ext)).
:- use_module(os(archive_ext)).
:- use_module(os(file_ext)).

:- db_add_novel(user:prolog_file_type(dic, dictionary)).

:- persistent(
     word(language:atom,index:positive_integer,word:atom,something:atom)
  ).

cert_verify(_, _, _, _, _):- !.

:- initialization(dictionary_init).



%! random_word(+Language:atom, -Word:atom, -Something:atom) is det.

random_word(Lang, Word, Something):-
  aggregate_all(
    count,
    word(Lang, _, Word, _),
    NumberOfWords
  ),
  random_between(1, NumberOfWords, I),
  word(Lang, I, Word, Something).


%! word(?Language:atom, ?Word:atom, ?Something:atom) is nondet.

word(Lang, Word, Something):-
  word(Lang, _, Word, Something).



% Initialization

%! dictionary_assert_stream(+Language:atom, +Read:blob) is det.

dictionary_assert_stream(Lang, Read):-
  setup_call_cleanup(
    archive_open(Read, Archive, []),
    dictionary_assert_archive(Lang, Archive),
    archive_close(Archive)
  ).

dictionary_assert_archive(Lang, Archive):-
  flag(dictionary(Lang), _, 1),

  % Construct the entry path that is to be extracted from the archive.
  once(user:prolog_file_type(Ext, dictionary)),
  file_name_extension(Lang, Ext, EntryName0),
  atomic_list_concat([dictionaries,EntryName0], '/', EntryName),
  archive_named_entry(EntryName, Archive, Read),

  % @tbd Is this the number of lines?
  read_line_to_codes(Read, Codes),
  number_codes(Number, Codes),
  writeln(Number),

  % Assert the dictionary words.
  dictionary_assert_words(Lang, Read).

%! dictionary_assert_words(+Language:atom, +Read:blob) is det.

dictionary_assert_words(_, Read):-
  at_end_of_stream(Read), !.
dictionary_assert_words(Lang, Read):-
  read_line_to_codes(Read, Codes),

  % Parse and assert a single entry in the dictionary.
  phrase(word_entry(Word,Something), Codes),
  flag(dictionary(Lang), I, I + 1),
  assert_word(Lang, I, Word, Something),

  dictionary_assert_words(Lang, Read).


%! dictionary_download(+Language:atom) is det.

dictionary_download(Lang):-
  dictionary_url(Lang, UriComponents),
  uri_components(Url, UriComponents),
  setup_call_cleanup(
    http_open(Url, Read, [cert_verify_hook(cert_verify)]),
    dictionary_assert_stream(Lang, Read),
    close(Read)
  ).


%! dictionary_file(+Language:atom, -File:atom) is det.

dictionary_file(Lang, File):-
  absolute_file_name(
    data(Lang),
    File,
    [access(write),file_type(dictionary)]
  ).


%! dictionary_init is det.

dictionary_init:-
  forall(
    dictionary_url(Lang, _),
    dictionary_init(Lang)
  ).

dictionary_init(Lang):-
  dictionary_file(Lang, File),
  safe_db_attach(File),
  file_age(File, Age),
  dictionary_update(Lang, Age).


%! dictionary_update(+Language:atom, +Age:float) is det.

% The persistent store is still fresh.
dictionary_update(Lang, Age):-
  once(word(Lang, _, _, _)),
  Age < 8640000, !.
% The persistent store has become stale, so refresh it.
dictionary_update(Lang, _):-
  retractall_word(Lang, _, _, _),
  dictionary_download(Lang).


%! dictionary_url(?Language:atom, ?UriComponents:compound) is nondet.

dictionary_url(
  'en-US',
  uri_components(
    https,
    'addons.mozilla.org',
    '/firefox/downloads/file/199368/united_states_english_spellchecker-7.0-fx+sm+tb.xpi',
    'src=dp-btn-primary',
    _
  )
).


%! safe_db_attach(+File:atom) is det.

safe_db_attach(File):-
  exists_file(File), !,
  db_attach(File, []).
safe_db_attach(File):-
  touch_file(File),
  safe_db_attach(File).


%! word_entry(-Word:atom, -Something:atom)// is det.

word_entry(Word2, Something2) -->
  word_part(Word1), !,
  {atom_codes(Word2, Word1)},
  dcg_rest(Something1),
  {atom_codes(Something2, Something1)}.
word_entry(Word2, '') -->
  dcg_rest(Word1),
  {atom_codes(Word2, Word1)}.

word_part([]) --> `/`, !.
word_part([H|T]) -->
  [H],
  word_part(T).

