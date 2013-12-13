:- module(
  replace_in_file,
  [
    replace_in_file/4, % +OldFile:atom
                       % :DCG_From
                       % :DCG_To
                       % ?NewFile
    trim_spaces/2 % +OldFile:atom
                  % ?NewFile:atom
  ]
).

/** <module> Replace in file

Make arbitrary consecutive replacements in text files.

@author Wouter Beek
@version 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_replace)).
:- use_module(generics(codes_ext)).
:- use_module(library(debug)).
:- use_module(os(file_ext)).

:- meta_predicate(replace_in_file(+,//,//,-)).
:- meta_predicate(replace_in_file(+,//,//,?,?)).

:- debug(replace_in_file).



%! replace_in_file(+OldFile:atom, :DCG_From, :DCG_To, ?NewFile:atom) is det.

replace_in_file(F1, DCG_From, DCG_To, F2):-
  % Type checks.
  is_absolute_file_name2(F1),
  (
    is_absolute_file_name2(F2), !
  ;
    new_file(F1, F2)
  ),
  setup_call_cleanup(
    open(F2, write, Out, [encoding(utf8),type(text)]),
    phrase_from_file(replace_in_file(Out, DCG_From, DCG_To), F1),
    close(Out)
  ).

%! replace_in_file(+Outpu:stream, :DCG_From, :DCG_To)// is det.

replace_in_file(Out, DCG_From, DCG_To) -->
  dcg_until([end_mode(inclusive),output_format(codes)], end_of_line, L1), !,
  {
    % Replace line
    phrase(dcg_replace(DCG_From, DCG_To), L1, L2),
    
    % DEB
    (
      L1 \= L2
    ->
      atom_codes(A1, L1),
      atom_codes(A2, L2),
      debug(replace_in_file, '~w\n~w\n', [A1,A2])
    ;
      true
    ),
    
    % Write line
    put_codes(Out, L2),
    flush_output(Out)
  },
  replace_in_file(Out, DCG_From, DCG_To).
replace_in_file(_Out, _DCG_From, _DCG_To) --> [].

%! trim_spaces(+OldFile:atom, ?NewFile:atom) is det.

trim_spaces(F1, F2):-
  replace_in_file(F1, dcg_multi(space, 1-_), space, F2).

