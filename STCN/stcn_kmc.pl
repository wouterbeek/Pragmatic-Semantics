:- module(
  stcn_kmc,
  [
    assert_schema_kmcs/1, % +Graph:atom
    kmc//3, % +KMC:atom
            % +Graph:atom
            % +PPN:iri
    kmc_code/3, % ?KMC:atom
                % ?Active:boolean
                % ?Suffix:atom
    kmc_start//1 % -KMC:atom
  ]
).

/** <module> STCN_KMC

Support for STCN KMCs.

@author Wouter Beek
@version 2013/06, 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_meta)).
:- use_module(dcg(dcg_multi)).
:- use_module(kmc(kmc_0500)). % Meta-calls.
:- use_module(kmc(kmc_1100)). % Meta-calls.
:- use_module(kmc(kmc_1200)). % Meta-calls.
:- use_module(kmc(kmc_1500)). % Meta-calls.
:- use_module(kmc(kmc_1700)). % Meta-calls.
:- use_module(kmc(kmc_3000)). % Meta-calls.
:- use_module(kmc(kmc_3011)). % Meta-calls.
:- use_module(kmc(kmc_3210)). % Meta-calls.
:- use_module(kmc(kmc_4043)). % Meta-calls.
:- use_module(kmc(kmc_4062)). % Meta-calls.
:- use_module(kmc(kmc_6511)). % Meta-calls.

:- discontiguous(kmc_code/2).
:- discontiguous(kmc_code/3).



assert_schema_kmcs(G):-
  % Only for active KMCs the corresponmding schemas are added.
  forall(
    kmc_code(_, true, Suffix),
    (
      format(atom(Predicate), 'assert_schema_kmc_~w', [Suffix]),
      (  current_predicate(stcn_kmc:Predicate/1)
      -> call(Predicate, G)
      ;  true)
    )
  ).

kmc(KMC, G, PPN) -->
  {kmc_code(KMC, true, Suffix)}, !,

  % The suffix under which a KMC is processed is not always the exact
  % numeric code that was parsed, since some codes occur in ranges,
  % e.g. =|7000-7099|=, where each code is treated similarly.
  {atomic_list_concat([kmc,Suffix], '_', DCG_Rule)},

  dcg_call(DCG_Rule, G, PPN).
kmc(_KMC, _G, _PPN) -->
  dcg_until([end_mode(exclusive),output_format(atom)], line_feed, _).

%! kmc_code(?KMC:atom, ?Active:boolean, ?Suffix:atom) is nondet.
% This predicate is needed since for some KMCs the predicate suffix is
% different from the KMC itself. This is the case for KMC ranges.
%
% This predicate occurs before kmc_code/2 for efficiency.

kmc_code(KMC, Active, Suffix):-
  kmc_code(KMC, Active),
  Suffix = KMC.

kmc_code('0500', true ). % Status
kmc_code('1100', true ). % Year
kmc_code('1200', true ). % Typographic property
kmc_code('1500', true ). % Language
kmc_code('1700', true ). % Country
kmc_code('2275', false). % Fingerprint
kmc_code('3000', false). % Primary author
kmc_code(KMC_Code, false, '3011'):- % Secondary authors
  between(3011, 3019, KMC),
  atom_number(KMC_Code, KMC).
kmc_code('3210', false). % Sorting title
kmc_code('3211', false).
kmc_code('3220', false).
kmc_code('3261', false).
kmc_code('3269', false).
kmc_code('3400', false).
kmc_code('4000', false).
kmc_code('4020', false).
kmc_code('4040', false).
kmc_code('4043', false). % Printers
kmc_code('4060', false).
kmc_code('4062', false). % Format
kmc_code('4064', false).
kmc_code('4160', false).
kmc_code('4201', false).
kmc_code('4203', false).
kmc_code('4400', false).
kmc_code('4700', false).
kmc_code('4701', false).
kmc_code('4710', false).
kmc_code('4711', false).
kmc_code('4712', false).
kmc_code('4900', false).
kmc_code('5300', false).
kmc_code('5301', false).
kmc_code('5421', false).
kmc_code('6501', false).
kmc_code(KMC_Code, false, '6511'):- % Topic
  between(6511, 6519, KMC),
  atom_number(KMC_Code, KMC).
kmc_code(KMC_Code, false, '7000'):-
  between(7000, 7099, KMC),
  atom_number(KMC_Code, KMC).
kmc_code('7100', false).
kmc_code('7134', false).
kmc_code('7800', false).
kmc_code('7900', false).

kmc_start(KMC) -->
  dcg_multi1(decimal_digit, 4, Codes),
  " ",
  {atom_codes(KMC, Codes)},
  {stcn_kmc:kmc_code(KMC, _Active, _Suffix)}, !.

