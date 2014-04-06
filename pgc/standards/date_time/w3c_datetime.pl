:- module(w3c_datetime, []).

/** <module> W3C_DATETIME

The datetime note defines a profile of ISO 8601, which describes a large
number of date/time formats. To reduce the scope for error and the complexity
of software, it is useful to restrict the supported formats to a small number.
This profile defines a few date/time formats, likely to satisfy most
requirements.

## Century omission

A particular problem with ISO 8601-1 (1988) is that it allows the century
to be omitted from years, which is likely to cause trouble.
This profile avoids the problem by expressing the year as four digits in
all cases.

Note that ISO 8601-3 (2004) does not allow century omission.

## Granularity levels

Components:
  * =YYYY=
    four-digit year
  * =MM=
    two-digit month (01=January, etc.)
  * =DD=
    two-digit day of month (01 through 31)
  * =hh=
    two digits of hour (00 through 23) (am/pm NOT allowed)
  * =mm=
    two digits of minute (00 through 59)
  * =ss=
    two digits of second (00 through 59)
  * =s=
    one or more digits representing a decimal fraction of a second
  * =TZD=
    time zone designator (Z or +hh:mm or -hh:mm)

Year:
~~~
YYYY
~~~
Example: =1997=

Year and month:
~~~
YYYY-MM
~~~
Example: =|1997-07|=

Complete date:
~~~
YYYY-MM-DD
~~~
Example: =|1997-07-16|=

Complete date plus hours and minutes:
~~~
YYYY-MM-DDThh:mmTZD
~~~
Example: =|1997-07-16T19:20+01:00|=

Complete date plus hours, minutes and seconds:
~~~
YYYY-MM-DDThh:mm:ssTZD
~~~
Example: =|1997-07-16T19:20:30+01:00|=

Complete date plus hours, minutes, seconds and a decimal fraction of a
second:
~~~
YYYY-MM-DDThh:mm:ss.sTZD
~~~
Example: =|1997-07-16T19:20:30.45+01:00|=

## Decimal fraction for seconds

This profile does not specify how many digits may be used to represent
the decimal fraction of a second. An adopting standard that permits fractions
of a second must specify both the minimum number of digits (a number greater
than or equal to one) and the maximum number of digits (the maximum may be
stated to be "unlimited").

## Examples

The examples represent November 5, 1994, 8:15:30 am, US Eastern Standard Time.

~~~
1994-11-05T08:15:30-05:00
1994-11-05T13:15:30Z
~~~

--

@author Wouter Beek
@see http://www.w3.org/TR/1998/NOTE-datetime-19980827
@version 2013/08
*/

