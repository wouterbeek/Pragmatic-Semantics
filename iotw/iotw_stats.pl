:- module(iotw_stats, []).

/** <module IOTW statistics

~~~{.R}
lower_recall <- c(1.0,0.37172100183246687,0.24123550044617229,0.23375413992878838,0.19251810221158194,0.1916729296953922,0.1790536768898471,0.17912482945205316,0.1696252362320524,0.17011849853438268)
higher_recall <- c(1.0,0.976185052819561,0.9600384749021372,0.9425765362723856,0.9297564861502547,0.9239282325982691,0.9288077551757412,0.900878631549093,0.886987450993775,0.8005503359226204)
quality <- c(0.5190405227795084,0.23261438049447705,0.1904353344556299,0.18505145463363135,0.17658153004935476,0.17387635867899948,0.17168948193128328,0.17017370794837366,0.16844506704419984,0.1677780567227606)
plot(
  lower_recall,
  cex=.6,
  cex.lab=0.95,
  col="darkgreen",
  pch=22,
  type="o",
  xaxt="n",
  xlab="Perc. of removed identity pairs",
  ylab="Recall in percent",
  ylim=c(0.0,1.0)
)
lines(
  higher_recall,
  col="darkred",
  type="o"
)
lines(
  quality,
  col="black",
  lty=2,
  type="o"
)
axis(
  1,
  at=1:11,
  labels=seq(0.0, 1.0, by=0.1)
)
~~~

~~~{.R}
in_higher <- c(1.0,0.5877680311890837,0.5747530425162004,0.572660333493667,0.5831761670185315,0.5799385908868745,0.5701984042084377,0.5692284399224807,0.558405393333526,0.5051324217787632)
plot(
  in_higher,
  cex=.6,
  cex.lab=0.95,
  type="o",
  xaxt="n",
  xlab="Percentage of removed identity pairs",
  ylab="Perc. of rem. id. pairs in higher approx.",
  ylim=c(0.1,1.0)
)
axis(
  1,
  at=1:10,
  labels=seq(0.0, 0.9, by=0.1),
  font=0.5
)
~~~

@author Wouter Beek
@version 2013/12
*/

:- use_module(library(aggregate)).
:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(prolog_pack)).
:- catch(use_module(library(real)), _, ignore(pack_install(real))).

:- initialization(iotw_stats).

iotw_stats:-
  absolute_file_name(
    '/home/wbeek/Git/PraSem/IOTW/stats_smoothing_5',
    File,
    [access(read)]
  ),
  csv_read_file(File, Rows, [arity(7)]),
  
  findall(
    I-Triples,
    (
      % Argument
      between(1, 6, I),
      findall(
        J-Values-Avg,
        (
          % Precision
          between(0, 9, J),
          findall(
            Value,
            (
              nth0(K, Rows, Row),
              Row =.. [row|Args],
              J =:= (K mod 10),
              nth0(I, Args, Value)
            ),
            Values
          ),
          avgerage(Values, Avg)
        ),
        Triples
      )
    ),
    Results
  ),
  forall(
    member(I-Triples, Results),
    (
      argument(I, ArgLabel),
      write('Argument:'), write(I), write(' ('), write(ArgLabel), write(')'), nl,
      forall(
        member(J-_-Avg, Triples),
        (write('Stage:'), write(J), write('::'), write(Avg), nl)
      )
    )
  ),
  halt.

argument(0, 'Percentage').
argument(1, 'Lower recall').
argument(2, 'Higher recall').
argument(3, 'Quality').
argument(4, 'Higher cover').
argument(5, 'Removed identity pairs in higher').
argument(6, 'Removed identity pairs in higher (corrected)').

plot(Coords):-
  absolute_file_name(
    '/home/wbeek/Git/PraSem/IOTW/plot.svg',
    File,
    [access(write)]
  ),
  <- svg(+File),
  coords <- Coords,
  <- plot(coords),
  <- dev..off(.).

avgerage(Numbers, Average):-
  sum_list(Numbers, Sum),
  length(Numbers, NumberOfNumbers),
  Average is Sum / NumberOfNumbers.
