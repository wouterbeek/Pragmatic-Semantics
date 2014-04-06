:- module(
  datetime_ext,
  [
    current_date/1, % ?Date:atom
    current_date_time/1, % ?DateTime:atom
    current_time/1, % ?Time:atom
    date_directories/2, % +Dir:atom
                        % -DateDir:atom
    hash_date/1, % -Hash:atom
    iso8601_dateTime/1, % -DateTime:atom
    latest_file/2, % +Files:list(atom)
                   % -File:atom
    posix_date/1, % -Date:atom
    posix_time/1, % -Time:atom
    seconds/3 % ?Hours:integer
              % ?Minutes:integer
              % ?Second:integer
  ]
).

/** <module> DATETIME_EXT

Extensions for date and time.

@author Wouter Beek
@version 2013/06-2013/07, 2013/11
*/

:- use_module(generics(meta_ext)).
:- use_module(os(dir_ext)).



%! current_date(-Date:atom) is det.
% Returns an atom representing the current date.
%
% This can be used for file names.
%
% @compat This uses the ISO 8601 date format, but with underscores instead
%         of dashes.

current_date(Date):-
  get_time(TimeStamp),
  format_time(atom(Date), '%Y_%m_%d', TimeStamp).

%! current_date_time(+DateTime:atom) is semidet.
%! current_date_time(-DateTime:atom) is det.
% @see Combines the result of current_date/1 and current_time/1.

current_date_time(DateTime):-
  current_date(Date),
  current_time(Time),
  atomic_list_concat([Date,Time], ':', DateTime).

%! current_time(-Time:atom) is det.
% Returns an atomic representation of the current time.
%
% This can be used for file names.

current_time(Time):-
  get_time(TimeStamp),
  format_time(atom(Time), '%H_%M_%S', TimeStamp).

%! date_directories(+Dir:atom, -DateDir:atom) is det.
% Create and retuns the current date subdirectory of the given absolute
% directory name.
%
% Example: from =|/home/wouterbeek/tmp|= to
% =|/home/wouterbeek/tmp/2013/05/10|=

date_directories(Dir, DateDir):-
  get_time(TimeStamp),
  format_time(atom(Day), '%d', TimeStamp),
  format_time(atom(Month), '%m', TimeStamp),
  RelativeSubDirs1 =.. [Month,Day],
  format_time(atom(Year), '%Y', TimeStamp),
  RelativeSubDirs2 =.. [Year,RelativeSubDirs1],
  RelativeDirs =.. [Dir,RelativeSubDirs2],
  create_nested_directory(RelativeDirs, DateDir).

%! hash_date(-Hash:atom) is det.
% Returns the hash of the current timestamp.
%
% @arg Hash An atomic hash.

hash_date(Hash):-
  get_time(TimeStamp),
  variant_sha1(TimeStamp, Hash).

%! iso8601_dateTime(-ISO8601_DateTime) is det.

iso8601_dateTime(DT):-
  get_time(TimeStamp),
  format_time(atom(DT), '%FT%T%z', TimeStamp).

%! latest_file(+Files:list(atom), -Latest:atom) is det.
% Returns the most recently created or altered file from within a list of
% files.
%
% @arg Files A list of atomic absolute file names.
% @arg Latest An atomic absolute file name.

latest_file([First | Files], Latest):-
  time_file(First, FirstTime),
  latest_file(Files, FirstTime-First, Latest).

latest_file([], _Time-Latest, Latest).
latest_file([File | Files], TopTime/TopFile, Latest):-
  time_file(File, Time),
  (
    Time > TopTime
  ->
    NewTopTime = Time,
    NewTopFile = File
  ;
    NewTopTime = TopTime,
    NewTopFile = TopFile
  ),
  latest_file(Files, NewTopTime-NewTopFile, Latest).

%! posix_date(-Date:atom) is det.
% Returns the current date in POSIX format.
%
% @compat POSIX strfdate()
% @arg Date A compound term of the form =Year/Month/Day=,
%        where =Year= consists of 4, =Month= consists of 2,
%        and =Day= consists of 2 digits.

posix_date(Date):-
  get_time(TimeStamp),
  format_time(atom(Date), '%F', TimeStamp).

%! posix_time(Time) is det.
% Returns the current time in POSIX format.
%
% @compat POSIX strftime()
% @arg Time The atomic default textual representation of a time in PraSem,
%        i.e. =Hour:Minute:Second=.

posix_time(Time):-
  get_time(TimeStamp),
  format_time(atom(Time), '%T', TimeStamp).

%! seconds(?Hours:integer, ?Minutes:integer, ?Seconds:integer) is det.
% Converts hours and minutes into seconds and vice versa.
%
% @arg Hours An integer
% @arg Minutes An integer
% @arg Seconds An integer

seconds(Hours, Minutes, Seconds):-
  nonvar(Seconds), !,
  Minutes is Seconds mod 60,
  Hours is Seconds / 60.
seconds(Hours, Minutes, Seconds):-
  default(0, Hours),
  default(0, Minutes),
  Seconds is (Minutes + (Hours * 60)) * 60.

