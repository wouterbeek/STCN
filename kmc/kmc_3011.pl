:- module(
  kmc_3011,
  [
    assert_schema_kmc_3011/1, % +Graph:graph
    kmc_3011//2, % +Graph:atom
                % +PPN:uri
    statistics_kmc_3011/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 3011 - SECONDARY AUTHOR

@author Wouter Beek
@version 2013/01-2013/04, 2013/06, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(stcn(kmc/kmc_30xx)).





assert_schema_kmc_3011(G):-
  assert_schema_kmc_30xx(G),

  rdfs_assert_subproperty(stcno:secondary_author, stcno:author, G),
  rdfs_assert_label(stcno:secondary_author, 'secundairy author', G),
  rdfs_assert_label(stcno:secondary_author, [nl]-'secundaire auteur', G),
  rdf_assert_string(stcno:secondary_author, stcno:kb_name, 'KMC 301X', G),
  rdfs_assert_seeAlso(
    stcno:secondary_author,
    'http://www.kb.nl/kbhtml/stcnhandleiding/301X.html',
    G
  ),
  rdf_assert_langstring(
    stcno:secondary_author,
    stcno:picarta_name,
    [nl]-'Vertaler / Bewerker',
    G
  ).

kmc_3011(G, PPN) -->
  {rdf_global_id(stcno:secondary_author, Predicate)},
  kmc_30xx(G, PPN, Predicate).

statistics_kmc_3011(G, [[A1,V1]]):-
  A1 = 'Publications with at least one secondary author',
  count_subjects(stcno:secondary_author, _, G, V1).

