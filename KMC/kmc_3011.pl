:- module(
  kmc_3011,
  [
    assert_schema_kmc_3011/1, % +Graph:graph
    kmc_3011//2, % +Graph:atom
                % +PPN:uri
    statistics_kmc3011/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 3011 - SECONDARY AUTHOR

@author Wouter Beek
@version 2013/01-2013/04, 2013/06, 2013/09
*/

:- use_module(kmc(kmc_30xx)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_statistics)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- nodebug(kmc_3011).



assert_schema_kmc_3011(G):-
  assert_schema_kmc_30xx(G),
  
  rdfs_assert_subproperty(stcnv:secondary_author, stcnv:author, G),
  rdfs_assert_label(stcnv:secondary_author, en, 'secundairy author', G),
  rdfs_assert_label(stcnv:secondary_author, nl, 'secundaire auteur', G),
  rdf_assert_literal(stcnv:secondary_author, stcnv:kb_name, 'KMC 301X', G),
  rdfs_assert_seeAlso(stcnv:secondary_author,
    'http://www.kb.nl/kbhtml/stcnhandleiding/301X.html', G),
  rdf_assert_literal(stcnv:secondary_author, stcnv:picarta_name, nl,
    'Vertaler / Bewerker', G).

kmc_3011(G, PPN) -->
  {rdf_global_id(stcnv:secondary_author, Predicate)},
  kmc_30xx(G, PPN, Predicate).

statistics_kmc3011(G, [[A1,V1]]):-
  A1 = 'Publications with at least one secondary author',
  count_subjects(stcnv:secondary_author, _, G, V1).

