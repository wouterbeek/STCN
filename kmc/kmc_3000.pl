:- module(
  kmc_3000,
  [
    assert_schema_kmc_3000/1, % +Graph:graph
    kmc_3000//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc_3000/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 3000 - Primary author

KMC 3000 contains the primary author.

The format is:
~~~{.txt}
voornaam/tussenvoegsel@achternaam!ppn!
~~~

For example:

~~~{.txt}
Jan/de@Wit!123456789!
~~~

# Looking up an author PPN in Picarta

~~~{.txt}
http://picarta.pica.nl/DB=3.11/SET=1/TTL=1/CMD?
  ACT=SRCHA&
  IKT=1016&
  SRT=YOP&
  TRM=ppn+070105464&
  REC=*
~~~

@author Wouter Beek
@version 2013/01-2013/06, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(stcn(kmc/kmc_30xx)).





assert_schema_kmc_3000(G):-
  assert_schema_kmc_30xx(G),
  rdfs_assert_subproperty(stcno:primary_author, stcno:author, G),
  rdfs_assert_label(stcno:primary_author, 'primary author', G),
  rdfs_assert_label(stcno:primary_author, [nl]-'primaire autheur', G),
  rdf_assert_string(stcno:primary_author, stcno:kb_name, 'KMC 3000', G),
  rdfs_assert_seeAlso(
    stcno:primary_author,
    'http://www.kb.nl/kbhtml/stcnhandleiding/3000.html',
    G
  ),
  rdf_assert_langstring(
    stcno:primary_author,
    stcno:picarta_name,
    [nl]-'Auteur',
    G
  ).

kmc_3000(G, PPN) -->
  {rdf_global_id(stcno:primary_author, Predicate)},
  kmc_30xx(G, PPN, Predicate).

statistics_kmc_3000(G, [[A1,V1]|T]):-
  statistics_kmc30xx(G, T),
  A1 = 'Publications with at least one primary author',
  count_subjects(stcno:primary_author, _, G, V1).

