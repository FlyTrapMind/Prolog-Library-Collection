module(
  latex,
  [
    latex_code_convert/1 % +PrologFile:atom
  ]
).

/** <module> LaTeX

LaTeX DCGs.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_replace)).
:- use_module(generics(codes_ext)).



%! file_to_latex_title(+PrologFile:atom, -Title:atom) is det.
% Returns the title for the TeX file that is generated based on the given
% Prolog source file.
%
% For the title we choose the Prolog module name.
% Non-module files get their title based on the local file name.
% Note that the module name and the local file name will ideally
% be the same for module files.
%
% Underscore characters must be escaped in LaTeX.

file_to_latex_title(PrologFile, Title):-
  module_property(Module, file(PrologFile)),

  % Beware for plunit submodules!
  \+ module_property(Module, class(test)), !,

  % Underscores must be escaped in LaTeX.
  dcg_phrase(dcg_replace(`_`, `\\_`), Module, Title).
file_to_latex_title(PrologFile, Local):-
  file_name(PrologFile, _, Local, _).


%! latex(+Command:oneof([begin,end]))// is semidet.
% Succeeds if the codes list starts with a LaTeX commmand.
%
% Currently the commands =|begin(latex)|= and =|end(latex)|= are defined.

latex(Command) -->
  % Allow Prolog multiline commenting.
  (
    forward_slash, asterisk % Avoid colorization in bad editors.
  ;
    ``
  ),
  atom(Command),
  bracketed(round, atom(latex)),
  
  % Allow Prolog multiline commenting.
  (
    asterisk, forward_slash % Avoid colorization in bad editors.
  ;
    ``
  ).



%! latex_code_convert(+FileOrDirectory:atom) is det.
% Either an absolute file name or a relative file name that can
% be resolved relative to the given directory.
% File names may denote directory or non-directory files.
% In the case of a directory, all the containing files (including
% subdirectories) are processed as well.

% Process Prolog files.
latex_code_convert(PrologFile):-
  is_absolute_file_name(PrologFile), !,
  file_alternative(PrologFile, _, _, tex, TexFile),
  setup_call_cleanup(
    (
      open(PrologFile, read, Read, [encoding(utf8),type(text)]),
      open(TeXFile, write, Write, [encoding(utf8),type(test)])
    ),
    (
      file_to_latex_title(PrologFile, Title),
      write_latex_header(
        Write,
        [
          arbitrary_lines([
            '\\newtheorem{convention}{Convention}',
            '\\newtheorem{definition}{Definition}',
            '\\newtheorem{theorem}{Theorem}',
            '\\theoremstyle{definition}',
            '',
            '\\newenvironment{boxdefinition}',
            '  {\\begin{mdframed}\\begin{definition}}',
            '  {\\end{definition}\\end{mdframed}}',
            '',
            '\\setlength{\\parskip}{\\baselineskip}',
            '\\setlength{\\parindent}{0cm}'
          ]),
          author('Wouter Beek'),
          document_attributes(['10pt',a4paper,draft,twocolumn,twoside]),
          packages([amsfonts,amsmath,amsthm,latexsym,listings,mdframed]),
          title(Title)
        ]
      ),
      latex_code_convert(Read, Write, none),
      write_latex_footer(Write)
    ),
    (
      close(Read),
      close(Write)
    )
  ).
% Dive into directories.
latex_code_convert(Directory):-
  exists_directory(Directory), !,
  directory_files(Directory, Files),
  maplist(latex_code_convert, Files).
% Skip non-Prolog files.
latex_code_convert(_).

%! latex_code_convert(
%!   +Read:stream,
%!   +Write:stream,
%!   +Mode:oneof([latex,none,prolog])
%! ) is det.

% End of stream.
latex_code_convert(Read, Write, Mode):-
  at_end_of_stream(Read), !,
  if_then(
    Mode == prolog,
    (
      write(Write, '\\end{lstlisting}'),
      nl(Write)
    )
  ).
% No mode.
latex_code_convert(Read, Write, none):- !,
  read_line_to_codes(Read, Codes),
  (
    phrase(latex(begin), Codes)
  ->
    Mode = latex
  ;
    Mode = none
  ),
  latex_code_convert(Read, Write, Mode).
% LaTeX mode.
latex_code_convert(Read, Write, latex):- !,
  read_line_to_codes(Read, Codes),
  (
    phrase(latex(end), Codes)
  ->
    nl(Write),
    write(Write, '\\begin{lstlisting}'),
    LaTeXMode = prolog
  ;
    write_latex_codes_nl(Write, Codes),
    LaTeXMode = latex
  ),
  latex_code_convert(Read, Write, LaTeXMode).
% Prolog mode.
latex_code_convert(Read, Write, prolog):- !,
  read_line_to_codes(Read, Codes),
  (
    phrase(latex(begin), Codes)
  ->
    % LaTeX begin found: end listing.
    write(Write, '\\end{lstlisting}'),
    nl(Write),
    Mode = latex
  ;
    write_latex_codes_nl(Write, Codes),
    Mode = prolog
  ),
  latex_code_convert(Read, Write, Mode).


write_latex_codes(Stream, Codes1):-
  phrase(dcg_replace(`_`, `\\_`), Codes1, Codes2),
  put_codes(Stream, Codes2).

write_latex_codes_nl(Stream, Codes):-
  write_latex_codes(Stream, Codes),
  nl(Stream).

write_latex_documentclass(Stream, DocumentClass, Options1):-
  atomic_list_concat(Options1, ',', Options2),
  format(Stream, '\\documentclass[~w]{~w}\n', [Options2, DocumentClass]).

write_latex_footer(Stream):-
  % End of document.
  nl(Stream),
  format(Stream, '\\end{document}\n', []).

write_latex_header(Stream, Options):-
  % The document class.
  option(document_attributes(DocumentAttributes), Options, []),
  write_latex_documentclass(Stream, article, DocumentAttributes),
  nl(Stream),

  % Use packages.
  if_then(
    (
      option(packages(Packages), Options),
      Packages \== []
    ),
    (
      maplist(write_latex_package(Stream), Packages),
      nl(Stream)
    )
  ),

  % Arbitary lines, since we cannot cater for *every* possible header setting.
  if_then(
    option(arbitrary_lines(ArbitraryLines), Options),
    (
      maplist(format(Stream, '~w\n'), ArbitraryLines),
      nl(Stream)
    )
  ),

  % Information for the title.
  if_then(
    option(author(Author), Options),
    format(Stream, '\\author{~w}\n', [Author])
  ),
  if_then(
    option(title(Title), Options),
    format(Stream, '\\title{~w}\n', [Title])
  ),
  if_then(
    (
      option(author(_Author1), Options)
    ;
      option(title(_Title1), Options)
    ),
    nl(Stream)
  ),

  % End of header.
  format(Stream, '\\begin{document}\n', []),
  nl(Stream),

  % Display the title.
  if_then(
    (
      option(author(_), Options)
    ;
      option(title(_), Options)
    ),
    (
      format(Stream, '\\maketitle\n', []),
      nl(Stream)
    )
  ).

write_latex_package(Stream, Package):-
  format(Stream, '\\usepackage{~w}\n', [Package]).

