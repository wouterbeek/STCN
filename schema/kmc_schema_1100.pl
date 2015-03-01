:- module(kmc_schema_1100, []).

/** <module> Schema: KMC 1100

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(1100, Graph):-
  rdf_assert_property(stcno:publication_year, G),
  rdfs_assert_label(stcno:publication_year, 'publication year', G),
  rdfs_assert_label(stcno:publication_year, [nl]-publicatiejaar, G),
  rdfs_assert_domain(stcno:publication_year, stcno:'Publication', G),
  rdfs_assert_range(stcno:publication_year, xsd:gYear, G),
  rdf_assert_string(stcno:publication_year, stcno:kb_name, 'KMC 1100', G),
  rdfs_assert_seeAlso(
    stcno:publication_year,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1100.html',
    G
  ),
  rdf_assert_langstring(
    stcno:publication_year,
    stcno:picarta_name,
    [nl]-'Jaar',
    G
  ),

  rdfs_assert_subproperty(
    stcno:exact_publication_year,
    stcno:publication_year,
    G
  ),
  rdfs_assert_label(
    stcno:exact_publication_year,
    'exact publication year',
    G
  ),
  rdfs_assert_label(
    stcno:exact_publication_year,
    [nl]-'exact publicatiejaar',
    G
  ),

  rdfs_assert_subproperty(
    stcno:earliest_publication_year,
    stcno:publication_year,
    G
  ),
  rdfs_assert_label(
    stcno:earliest_publication_year,
    'earliest publication year',
    G
  ),
  rdfs_assert_label(
    stcno:earliest_publication_year,
    [nl]-'vroegste publicatiejaar',
    G
  ),

  rdfs_assert_subproperty(
    stcno:latest_publication_year,
    stcno:publication_year,
    G
  ),
  rdfs_assert_label(
    stcno:latest_publication_year,
    'latest publicationyear',
    G
  ),
  rdfs_assert_label(
    stcno:latest_publication_year,
    [nl]-'laatste publicatiejaar',
    G
  ).
