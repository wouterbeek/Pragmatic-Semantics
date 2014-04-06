:- module(
  collect_lines,
  [
    collect_lines/2 % +FromFile:atom
                    % +ToFile:atom
  ]
).

/** <module> Collect lines

The first step in the STCN conversion.
Some statements span multiple lines.
We first want to create a text file with one line per statement.

@author Wouter Beek
@version 2013/09-2013/10
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(generics(codes_ext)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(os(file_ext)).
:- use_module(stcn(stcn_kmc)).



collect_lines(F1, F2):-
  new_file(F1, F2),
  setup_call_cleanup(
    open(F2, write, Out, [encoding(utf8),type(text)]),
    phrase_from_file(collect_lines(Out), F1, [encoding(utf8),type(text)]),
    close(Out)
  ).

% Skip empty lines.
collect_lines(Out) -->
  end_of_line, !,
  collect_lines(Out).
% Line 2.946.252 contains the erratic character sequences:
%  * [239,187,191]
%    ** =ï=, Latin Small Letter I With Diaeresis
%    ** =»=, Right-Pointing Double Angle Quotation Mark
%    ** =¿=, Inverted Question Mark
%  * [65279]
%    ** Zero Width No-Break Space
collect_lines(Out) -->
  ([239,187,191] ; [65279]),
  end_of_line, !,
  collect_lines(Out).
% A collected line.
collect_lines(Out) -->
  collect_line(L), !,
  {put_codes(Out, L), nl(Out), flush_output(Out)},
  collect_lines(Out).
% Done!
collect_lines(_Out, L, L).

collect_line(L) -->
  collect_line(L, out).

% SET line.
collect_line(L, out) -->
  dcg_peek(atom('SET')), !,
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, L),
  end_of_line.
% Ingevoerd line, always spans two rows.
collect_line(L, out) -->
  dcg_peek(atom('Ingevoerd')), !,
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, L1),
  end_of_line,
  (
    dcg_peek(dcg_multi(decimal_digit, 2))
  ->
    dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, L2),
    end_of_line,
    {append(L1, L2, L)}
  ;
    {L = L1}
  ).
% Start a KMC line.
collect_line(L, out) -->
  peek_kmc_start, !,
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, H),
  end_of_line,
  collect_line(T, in),
  {append(H, T, L)}.

% We are now inside a KMC line...
% A blank line ends a KMC line.
collect_line([], in) -->
  end_of_line, !.
% Another KMC ends a KMC line.
collect_line([], in) -->
  peek_kmc_start, !.
% Another SET ends a KMC line.
collect_line([], in) -->
  dcg_peek(atom('SET')), !.
% Another Ingevoerd ends a KMC line. (This may never occur.)
collect_line([], in) -->
  dcg_peek(atom('Ingevoerd')), !.
% The document ends. This surely ends a KMC line.
collect_line([], in) -->
  dcg_end, !.
% The KMC line continues.
collect_line(L, in) -->
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, H1),
  end_of_line,
  collect_line(T, in),
  % We sometimes have to add spaces and sometimes not,
  % but we are not
  {H2 = [32|H1]},
  {append(H2, T, L)}.

peek_kmc_start -->
  dcg_peek(5, Cs),
  {phrase(kmc_start(_KMC), Cs)}.
