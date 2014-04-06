:- module(
  xml_stream,
  [
    xml_stream/3, % +File:atom
                  % +Tag:atom
                  % :Goal
    xml_stream/4 % +File:atom
                 % +Tag:atom
                 % :Goal
                 % :StoreGoal
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
@version 2013/06-2013/07
*/

:- use_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(os(io_ext)).

:- meta_predicate(xml_stream(+,+,1)).
:- meta_predicate(xml_stream(+,+,1,0)).
:- meta_predicate(xml_stream0(+,+,1,0,+)).

:- db_add_novel(user:prolog_file_type(tmp, temporary)).

:- setting(
  store_number,
  integer,
  100000, % Set to 100,000.
  'The number of items after which an intermediate save is made.'
).



xml_stream(File, Tag, Goal):-
  xml_stream(File, Tag, Goal, true).

xml_stream(File, Tag, Goal, StoreGoal):-
  is_absolute_file_name(File), !,
  setting(store_number, StoreNumber),
  format(atom(StartTag), '<~w>', [Tag]),
  format(atom(EndTag), '</~w>', [Tag]),
  setup_call_cleanup(
    open(File, read, Stream, [alias(big_stream),encoding(utf8),type(test)]),
    xml_stream0(Stream, StartTag-EndTag, Goal, StoreGoal, StoreNumber),
    close(Stream)
  ).

% Intermediate storage of results.
xml_stream0(Stream, Tags, Goal, StoreGoal, StoreNumber):-
  flag(processed_items, StoreNumber, 0), !,
  thread_create(StoreGoal, _Id, []),
  xml_stream0(Stream, Tags, Goal, StoreGoal, StoreNumber).
% End of stream.
xml_stream0(Stream, _Tags, _Goal, StoreGoal, _StoreNumber):-
  at_end_of_stream(Stream), !,
  call(StoreGoal).
% Processes an entry.
xml_stream0(Stream, StartTag-EndTag, Goal, StoreGoal, StoreNumber):-
  (
    peek_atom(Stream, StartTag)
  ->
    % Read the next content between the given start and end tags
    % (including those tags).
    setup_call_cleanup(
      tmp_file_stream(utf8, File, Out),
      xml_stream1(Stream, StartTag-EndTag, Out),
      close(Out)
    ),

    % Turn the situation around: from writing to reading.
    setup_call_cleanup(
      open(File, read, In, [encoding(utf8),type(test)]),
      (
        load_structure(
          In,
          DOM,
          [
            case_sensitive_attributes(true),
            dialect(xml),
            max_errors(1),
            shorttag(false),
            space(remove)
          ]
        ),
        % Execute the custom goal on the DOM of a single tag-delimited entry.
        call(Goal, DOM)
      ),
      (
        close(In),
        % Do not safe-delete temporary files.
        delete_file(File)
      )
    ),

    flag(processed_items, X, X + 1)
  ;
    % Skips a line. Notify user.
    read_line_to_codes(Stream, Codes),
    line_count(Stream, Line),
    atom_codes(Atom, Codes),
    debug(xml_stream, 'Skipping line ~w: ~w', [Line,Atom])
  ),
  xml_stream0(Stream, StartTag-EndTag, Goal, StoreGoal, StoreNumber).

% Closes an entry.
xml_stream1(Stream, _StartTag-EndTag, Out):-
  peek_atom(Stream, EndTag), !,
  copy_stream_line(Stream, Out).
% Continues an entry.
xml_stream1(Stream, Tags, Out):-
  copy_stream_line(Stream, Out),
  xml_stream1(Stream, Tags, Out).

