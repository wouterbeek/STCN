:- module(
  stcn_script_kmc,
  [
    stcn_parse_value_script/1 % +Graph:atom
  ]
).

/** <module> STCN KMC: Script

Script for enriching a given STCN graph by parsing
the values of KMC properties.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_generics)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).

:- use_module(stcn(kmc/kmc_0500)). % Meta-calls.
:- use_module(stcn(kmc/kmc_1100)). % Meta-calls.
:- use_module(stcn(kmc/kmc_1200)). % Meta-calls.
:- use_module(stcn(kmc/kmc_1500)). % Meta-calls.
:- use_module(stcn(kmc/kmc_1700)). % Meta-calls.
:- use_module(stcn(kmc/kmc_3000)). % Meta-calls.
:- use_module(stcn(kmc/kmc_3011)). % Meta-calls.
:- use_module(stcn(kmc/kmc_3210)). % Meta-calls.
:- use_module(stcn(kmc/kmc_4043)). % Meta-calls.
:- use_module(stcn(kmc/kmc_4062)). % Meta-calls.
:- use_module(stcn(kmc/kmc_6511)). % Meta-calls.





stcn_script_kmc(G):-
  forall(
    (
      rdf_string(Publication, P, String, G),
      rdf_global_id(stcno:Kmc0, P)
    ),
    (
      dcg_phrase(kmc:kmc_value(Kmc0, G, Publication), String),
      rdf_retractall_string(Publication, P, String, G)
    )
  ).
