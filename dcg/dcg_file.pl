:- module(
  dcg_file,
  [
    directory_name//1, % -Dir:atom
    file_path_name//1, % -Path:atom
    local_file_name//1, % % -Local:atom
    root_prefix//0
  ]
).

/** <module> DCG: File

Grammar snippets for files.

@author Wouter Beek
@version 2014/11
*/

:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(os_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_meta)).



%! directory_name(-Dir:atom)// is semidet.
% Parses legal directory names,
% where both Unix and Windows formats are supported.
%
% ## Example
%
% Unix directory input:
% ```
% /home/wbeek/Dropbox/IOTW
% ```
%
% Windows directory input:
% ```
% C:\Users\Quirinus\Dropbox\IOTW
% ```

% Relative directory with respect to the home directory (Unix only).
directory_name(Dir) -->
  "~/", !,
  '*'(directory_segment, DirSegments, [separator(directory_separator)]),
  {
    directory_subdirectories(RelPath, DirSegments),
    relative_file_path(Dir, '~', RelPath)
  }.
% Absolute directory.
directory_name(Dir) -->
  root_prefix,
  '*'(directory_segment, DirSegments, [separator(directory_separator)]),
  {directory_subdirectories(Dir, DirSegments)}.



%! file_path_name(-Path:atom)// is semidet.

% Directory relative to the user's home directory.
file_path_name(Path) -->
  directory_name(Dir),
  local_file_name(File),
  {directory_file_path(Dir, File, Path)}.



%! local_file_name(-Local:atom)// .

local_file_name(Local) -->
  dcg_atom_codes(local_file_char, Local).



%! root_prefix// .

root_prefix -->
  {root_prefix(RootPrefix)},
  atom(RootPrefix).



% HELPERS

%! directory_char(?Code:nonneg)// .
% Character that are allowed to occur in a file path.

directory_char(Code) --> letter(Code).
directory_char(Code) --> decimal_digit(Code).
directory_char(Code) --> dot(Code).
directory_char(Code) --> minus_sign(Code).
directory_char(Code) --> plus_sign(Code).
directory_char(Code) --> underscore(Code).



%! directory_segment(-Segment:atom)// .

directory_segment(Segment) -->
  dcg_atom_codes(directory_char, Segment).



%! directory_separator// .

:- if(is_unix).
directory_separator --> "/".
:- endif.
:- if(is_windows).
directory_separator --> "\\".
:- endif.



%! local_file_char(?Code:nonneg)// .

local_file_char(Code) --> alpha_numeric(Code).
local_file_char(Code) --> dot(Code).
