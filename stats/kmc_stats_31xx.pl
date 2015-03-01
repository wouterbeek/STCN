:- module(kmc_stats_31xx, []).

/** <module> KMC Statistics: 3100-3199

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(owl/owl_read)).
:- use_module(plRdf(vocabulary/rdf_read)).

:- use_module(plRdfHtml(html_pl_term)).

:- use_module(plRdfEntailment(rdf_ent_stats)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(KmcCode, Graph) -->
  {between(3000, 3099, KmcCode)}, !,
  
  html([
    h3('Publications with at least one author'),
    {count_subjects(stcno:author, _, Graph, V1)},
    \html_pl_term(thousands_integer(V1)),
    
    h3('Publications with at least one DBpedia author'),
    {
      aggregate_all(
        count,
        (
          rdf(DBpediaAuthorPPN, stcno:author, AuthorPPN, Graph),
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
          rdf(_, stcno:author, AuthorPPN, Graph),
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
          rdf(PPN_Pseudonym, stcno:author, Author, Graph),
          rdf_string(Author, stcno:pseudonym, Pseudonym, Graph)
        ),
        V5
      )
    },
    \html_pl_term(thousands_integer(V5)),
    
    h3('Number of pseudonyms'),
    {
      aggregate_all(
        count,
        rdf_string(_, stcno:pseudonym, Pseudonym, Graph),
        V6
      )
    },
    \html_pl_term(thousands_integer(V6)),
    
    h3('Publications with at least one primary author'),
    {count_subjects(stcno:primary_author, _, Graph, V7)},
    \html_pl_term(thousands_integer(V7)),
    
    h3('Publications with at least one secondary author'),
    {count_subjects(stcno:secondary_author, _, Graph, V8)},
    \html_pl_term(thousands_integer(V8))
  ]).
