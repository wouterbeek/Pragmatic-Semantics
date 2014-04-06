:- module(
  dcg_date,
  [
    date//2, % ?Lang:atom
             % ?Date:compound
    day//2, % ?Lang:atom
            % ?Day:integer
    month//2, % ?Lang:atom
              % ?Month:integer
    year//2 % ?Lang:atom
            % ?Year:integer
  ]
).

/** <module> DCG date

DCG rules for parsing/generating dates.

@author Wouter Beek
@version 2013/06, 2014/03
*/



date(Lang, date(Year,Month,Day,_,_,_)) -->
  % DCG_YEAR cannot be used for consecutive YYYYMMDD representations,
  % since we need the unwarrented asseumption that a year lies between
  % 0000 and 9999.
  year(Lang, Year),
  ("-" ; ""),
  month(Lang, Month),
  ("-" ; ""),
  day(Lang, Day).


day(_, Day) -->
  [D1,D2],
  {
    number_codes(Day, [D1,D2]),
    between(1, 31, Day)
  }.


month(_, Month) -->
  [D1,D2],
  {
    number_codes(Month, [D1,D2]),
    between(1, 12, Month)
  }.


year(_, Year) -->
  [Y1,Y2,Y3,Y4],
  {number_codes(Year, [Y1,Y2,Y3,Y4])}.

