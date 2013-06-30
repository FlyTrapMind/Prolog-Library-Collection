:- module(
  xml_stream,
  [
    xml_stream/3 % +File:atom
                 % +Tag:atom
                 % :Goal
  ]
).

/** <module> XML_STREAM

This module allows a huge XML file's DOM to be processed
using an arbitrary Prolog goal.

This would normally be impossible since it would be required
to load the DOM of the entiry file into memory all at once.

The limitation of the here presented predicate is that the user
must give the name of the XML tag that is at the granularity
level at which DOM can be read into memory.

This means that this method is only applicable to
large XML files with relatively short chunks of XML
in the same tab construct.

Everything that appears outside of the indicated tag constructs
is ignored by this methods.

@author Wouter Beek
@version 2013/06
*/

:- use_module(os(io_ext)).

:- meta_predicate(+,+,1).



xml_stream(File, Tag, Goal):-
  is_absolute_file_name(File), !,
  format(atom(StartTag), '<~w>', [Tag]),
  format(atom(EndTag), '</~w>', [Tag]),
  setup_call_cleanup(
    open(File, read, Stream),
    xml_stream0(Stream, StartTag-EndTag, Goal),
    close(Stream)
  ).

% End of stream.
xml_stream0(Stream, _Tags, _Goal):-
  at_end_of_stream(Stream), !.
% Starts an entry
xml_stream0(Stream, StartTag-EndTag, Goal):-
  peek_atom(Stream, StartTag), !,
  tmp_file_stream(utf8, _TmpFile, TmpStream),
  copy_stream_line(Stream, TmpStream)
  % Note that xml_stream0/4 is going to call xml_stream0/3 back.
  xml_stream0(Stream, StartTag-EndTag, Goal, TmpStream).
% Skips a line. Notify user.
xml_stream0(Stream, Tags, Goal):-
  read_line_to_codes(Stream, Codes),
  atom_codes(Atom, Codes),
  cowsay(energylabels, 'Skipping: ~w', [Atom]),
  xml_stream0(Stream, Tags, Goal).

% Closes an entry.
xml_stream0(Stream, TmpStream, _StartTag-EndTag, Goal):-
  peek_atom(Stream, EndTag), !,
  
  % Finalize the XML.
  copy_stream_line(Stream, TmpStream),
  
  % Turn the situatiun around: from writing to reading.
  stream_property(TmpStream, file_name(TmpFile)),
  setup_call_cleanup(
    open(TmpFile, read, EntryStream),
    (
      load_structure(EntryStream, DOM, []),
      call(Goal, DOM)
    ),
    close(EntryStream),
    delete_file(TmpFile)
  ),
  
  % Start looking for the next entry.
  xml_stream0(Stream, TmpStream, StartTag-EndTag, Goal).
% Continues an entry.
xml_stream0(Stream, TmpStream, Tags, Goal):-
  copy_stream_line(Stream, TmpStream),
  xml_stream0(Stream, TmpStream, Tags, Goal).
