:- module(
  abnf2dcg,
  [
    abnf2dcg/1 % +File:atom
  ]
).

/** <module> ABNF2DCG

Converts ABNF grammars to DCGs.

--

@author Wouter Beek
@version 2013/08
*/

:- use_module(dcg(dcg_content)).
:- use_module(library(pio)).



abnf2dcg(File):-
  access_file(File, read),
  phrase_from_file(abnf, File).

abnf_rule -->
  abnf_name, blanks, "=", blanks, abnf_elements, crlf.

abnf_name -->
  dcg_cistring.

%  BIT            =  "0" / "1"


