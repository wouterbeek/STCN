:- module(
  kmc_generics,
  [
    assert_schema_kmcs/1, % +Graph:atom
    kmc_code/2 % ?Kmc:nonneg
               % -Property:iri
  ]
).

/** <module> STCN KMC: Generics

@author Wouter Beek
@version 2013/06, 2013/09, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(generics/atom_ext)).
:- use_module(plc(math/radix)).

:- discontiguous(kmc_code/2).
:- discontiguous(kmc_code/3).

:- rdf_meta(stcn_code(?,r)).





assert_schema_kmcs(G):-
  % Only for active KMCs the corresponmding schemas are added.
  forall(
    kmc_code(_, Suffix),
    (
      format(atom(Predicate), 'assert_schema_kmc_~w', [Suffix]),
      (   current_predicate(stcn_kmc:Predicate/1)
      ->  call(Predicate, G)
      ;   true
      )
    )
  ).



%! kmc_code(?Kmc:nonneg, ?Property:iri) is nondet.

kmc_code(Kmc, P):-
  kmc_code(Kmc),
  atom_number(Kmc0, Kmc),
  rdf_global_id(stcno:Kmc0, P).

% Status
kmc_code(500, stcno:'0500').
% Year
kmc_code(1100).
% Typographic property
kmc_code(1200).
% Language
kmc_code(1500).
% Country
kmc_code(1700).
% Fingerprint
kmc_code(2275).
% Primary author
kmc_code(3000).
% Secondary authors
kmc_code(Kmc, '3011'):-
  between(3011, 3019, Kmc).
% Sorting title
kmc_code(3210).
kmc_code(3211).
kmc_code(3220).
kmc_code(3261).
kmc_code(3269).
kmc_code(3400).
kmc_code(4000).
kmc_code(4020).
kmc_code(4040).
% Printers
kmc_code(4043).
kmc_code(4060).
% Format
kmc_code(4062).
kmc_code(4064).
kmc_code(4160).
kmc_code(4201).
kmc_code(4203).
kmc_code(4400).
kmc_code(4700).
kmc_code(4701).
kmc_code(4710).
kmc_code(4711).
kmc_code(4712).
kmc_code(4900).
kmc_code(5300).
kmc_code(5301).
kmc_code(5421).
kmc_code(6501).
% Topic
kmc_code(Kmc, stcno:'6511'):-
  between(6511, 6519, Kmc).
kmc_code(Kmc, stcno:'7000'):-
  between(7000, 7099, Kmc).
kmc_code(7100).
kmc_code(7134).
kmc_code(7800).
kmc_code(7900).
