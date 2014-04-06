:- module(
  xsd_dateTime_ext,
  [
    posix_timestamp_to_xsd_dateTime/2, % +PosixTimeStamp:float
                                       % -XsdDateTime:compound
    prolog_date_to_xsd_dateTime/2, % +PrologDate:compound,
                                   % -XxdDateTime:compound
    xsd_dateTime/1 % -DateTime:compound
  ]
).

/** <module> XSD dateTime extensions

Extensions for XSD dateTime support.

The standards-compliant support for parsing/generating XSD dateTime
values is in module [xsd_dateTime].

@author Wouter Beek
@version 2013/08-2013/11, 2014/03
*/

:- use_remote_module(xsd(xsd_dateTime)).
:- use_remote_module(xsd(xsd_dateTime_generic)).
:- use_remote_module(xsd(xsd_dateTime_support)).



%! posix_timestamp_to_xsd_dateTime(
%!   +PosixTimeStemp:float,
%!   -XsdDateTime:compound
%! ) is det.
% Converts a POSIX timestamp to an XSD dateTime compound term.
%
% @arg PosixTimeStamp A floating point number expressing the time
%      in seconds since the Epoch at 1970-1-1.
% @arg XsdDateTime A compound term representing a data-time value,
%      as defined by XML schema 1.1 Part 2: Datatypes.
%
% @see http://en.wikipedia.org/wiki/Unix_time
% @see http://www.w3.org/TR/xmlschema11-2/#dt-dt-7PropMod

posix_timestamp_to_xsd_dateTime(PosixTimestamp, XsdDateTime):-
  stamp_date_time(PosixTimestamp, PrologDate, local),
  prolog_date_to_xsd_dateTime(PrologDate, XsdDateTime).


%! prolog_date_to_xsd_dateTime(
%!   +PrologDate:compound,
%!   -XsdDateTime:compound
%! ) is det.
% In the SWI-Prolog representation the timezone is an atom (e.g. `CEST`)
% and the offset is an integer representing the offset relative to UTC
% in _seconds_.
%
% In the XSD representation the timezone is the offset relative to UTC
% in _minutes_.
%
% @arg PrologDate A compound term representing a date-time value.
%      date-time representations.
% @arg XsdDateTime A compound term representing a data-time value,
%      as defined by XML schema 1.1 Part 2: Datatypes.
%
% @see http://www.swi-prolog.org/pldoc/man?section=timedate
% @see http://www.w3.org/TR/xmlschema11-2/#dt-dt-7PropMod

prolog_date_to_xsd_dateTime(
  date(Y,M,D,H,MM,S,Offset,_TZ,_DST),
  dateTime(Y,M,D,H,MM,S,TZ)
):-
  TZ is Offset / 60.


%! xsd_dateTime(-XsdDateTime) is det.
% Similar to get_time/1, but returns the date and time in
% the canonical lexical format of the =dateTime= datatype
% from XML Schema 2: Datatypes W3C standard.

xsd_dateTime(XsdDateTime):-
  get_time(PosixTimestamp),
  posix_timestamp_to_xsd_dateTime(PosixTimestamp, DateTime),
  phrase(xsd_dateTime_canonical_map(DateTime), Codes),
  atom_codes(XsdDateTime, Codes).

