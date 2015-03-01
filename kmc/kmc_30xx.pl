:- module(
  kmc_30xx,
  [
    assert_schema_kmc_30xx/1, % +Graph:graph
    statistics_kmc30xx/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 30XX

Code shared by KMC 3000 and KMC 3011.

This includes PPN-based author lookup on Picarta.

Some author PPNs have lower case x in them, e.g. PPN 06907352x.
Some author PPNs have upper case x in them, e.g. PPN 14895961X.
These are considered to be the same. Mapped to upper case X using option
=|case(upper)|=.

@author Wouter Beek
@version 2013/01-2013/03, 2013/05-2013/06, 2013/09, 2013/12-2014/01, 2014/03,
         2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plc(dcg/dcg_ascii)). % Meta-DCG.
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(process/thread_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(owl/owl_build)).
:- use_module(plRdf(owl/owl_read)).
:- use_module(plRdf(term/rdf_term)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plSparql(query/sparql_query_api)).

:- use_module(lodCache(lod_cache)).

:- use_module(plRdfEntailment(rdf_ent_stat)).

:- use_module(stcn(stcn_generics)).

:- multifile(kmc:kmc_value//3).

:- rdf_meta(link_to_dbpedia_agent(+,r)).





% SCHEMA %

assert_schema_kmc_30xx(G):-
  % Author.
  rdfs_assert_subclass(stcno:'Author', foaf:'Agent', G),
  rdfs_assert_label(stcno:'Author', author, G),
  rdfs_assert_label(stcno:'Author', [nl]-auteur, G),

  % Has author.
  rdf_assert_property(stcno:author, G),
  rdfs_assert_label(stcno:author, 'has author', G),
  rdfs_assert_label(stcno:author, [nl]-'heeft auteur', G),

  % Author name.
  rdf_assert_property(stcn:author_name, G),
  rdfs_assert_label(stcno:author_name, 'has author name', G),
  rdfs_assert_label(stcno:author_name, [nl]-'heeft auteursnaam', G).





% GRAMMAR %

kmc:kmc_value(KmcCode, Publication, Graph) -->
  {between(3000, 3099, KmcCode)}, !,
  '...',
  "!",
  ppn('Author', AuthorCode),
  "!",
  {
    rdf_global_id(stcno:AuthorCode, Author),
    atom_number(KmcLocalName, KmcCode),
    rdf_global_id(stcno:KmcLocalName, P),
    rdf_assert(Publication, P, Author, Graph)
  }.





% STATISTICS %

statistics_kmc30xx(Graph) -->
  html([
    h3('Publications with at least one author'),
    {count_subjects(stcno:author, _, G, V1)},
    \html_pl_term(thousands_integer(V1)),
    
    h3('Publications with at least one DBpedia author'),
    {
      aggregate_all(
        count,
        (
          rdf(DBpediaAuthorPPN, stcno:author, AuthorPPN, G),
          owl_resource_identity(AuthorPPN, _DBpediaAuthor),
          rdf_global_id(dbpedia:_, DBpediaAuthor)
        ),
        V2
      )
    },
    \html_pl_term(thousands_integer(V2)),
    
    h3('Number of authors (including pseudonyms)'),
    {count_instances_by_class(stcno:'Author', V3)},
    \html_pl_term(V3),
    
    h3('Number of DBpedia authors (including pseudonyms)'),
    {
      aggregate_all(
        count,
        (
          rdf(_, stcno:author, AuthorPPN, G),
          owl_resource_identity(AuthorPPN, DBpediaAuthor),
          rdf_global_id(dbpedia:_, DBpediaAuthor)
        ),
        V4
      )
    },
    \html_pl_term(thousands_integer(V4)),
    
    h3('Publications written under at least one pseudonym'),
    {
      aggregate_all(
        count,
        (
          rdf(PPN_Pseudonym, stcno:author, Author, G),
          rdf_string(Author, stcno:pseudonym, Pseudonym, G)
        ),
        V5
      )
    },
    \html_pl_term(thousands_integer(V5)),
    
    h3('Number of pseudonyms'),
    {
      aggregate_all(
        count,
        rdf_string(_, stcno:pseudonym, Pseudonym, G),
        V6
      )
    },
    \html_pl_term(thousands_integer(V6))
  ]).
