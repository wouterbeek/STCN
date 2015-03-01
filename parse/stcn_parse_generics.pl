:- module(
  stcn_parse_generics,
  [
    kmc_start//2 % -Kmc:nonneg
                 % -Property:iri
  ]
).

/** <module> STCN Parse: Generics

@author Wouter Beek
@version 2015/02
*/

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(math/radix)).

:- use_module(stcn(kmc/kmc_generics)).





kmc_start(Kmc, P) -->
  decimal_digit(N1),
  decimal_digit(N2),
  decimal_digit(N3),
  decimal_digit(N4),
  {weights_nonneg([N1,N2,N3,N4], Kmc)},
  " ",
  {kmc_code(Kmc, P)}, !.
