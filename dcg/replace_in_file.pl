:- module(
  replace_in_file,
  [
    replace_in_file/4, % +OldFile:atom
                       % :FromDCG
                       % :ToDCG
                       % ?NewFile
    trim_spaces/2 % +OldFile:atom
                  % ?NewFile:atom
  ]
).

/** <module> Replace in file

Make arbitrary consecutive replacements in text files.

@author Wouter Beek
@version 2013/09, 2014/10-2014/12
*/

:- use_module(library(debug)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_replace)).
:- use_module(plc(generics/atom_ext)). % Meta-option.
:- use_module(plc(generics/code_ext)).
:- use_module(plc(io/file_ext)).
:- use_module(plc(prolog/pl_control)).

:- meta_predicate(replace_in_file(+,//,//,-)).
:- meta_predicate(replace_in_file(+,//,//,?,?)).





%! replace_in_file(+OldFile:atom, :FromDCG, :ToDCG, ?NewFile:atom) is det.

replace_in_file(F1, FromDCG, ToDCG, F2):-
  % Type checks.
  is_absolute_file_name(F1),
  xor(
    % Catch instantiation_error thrown by is_absolute_file(-).
    catch(is_absolute_file_name(F2), _, fail),
    new_file_name(F1, F2)
  ),
  setup_call_cleanup(
    open(F2, write, Out, [encoding(utf8),type(text)]),
    phrase_from_file(replace_in_file(Out, FromDCG, ToDCG), F1),
    close(Out)
  ).



%! replace_in_file(+Output:stream, :FromDCG, :ToDCG)// is det.

replace_in_file(Out, FromDCG, ToDCG) -->
  dcg_until(end_of_line, L1, [convert(codes_atom),end_mode(inclusive)]), !,
  {
    % Replace line
    phrase(dcg_replace(FromDCG, ToDCG), L1, L2),

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
  replace_in_file(Out, FromDCG, ToDCG).
replace_in_file(_Out, _DCG_From, _DCG_To) --> [].



%! trim_spaces(+OldFile:atom, ?NewFile:atom) is det.

trim_spaces(F1, F2):-
  replace_in_file(F1, '+'(space, []), space, F2).

