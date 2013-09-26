:- module(
  kmc_30xx,
  [
    assert_schema_kmc_30xx/1, % +Graph:graph
    kmc_30xx//3, % +Graph:atom
                 % +PPN:uri
                 % +Predicate
    populate_dbpedia/1, % +DBpedia_Graph:atom
    statistics_kmc_30xx/2 % +Graph:atom
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
@version 2013/01-2013/03, 2013/05-2013/06, 2013/09
*/

:- use_module(datasets(dbpedia)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(meta_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(owl(owl_build)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_statistics)).
:- use_module(rdf(rdf_term)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(sparql(sparql_ext)).
:- use_module(stcn(stcn_generic)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- rdf_meta(link_to_dbpedia_agent(+,r)).

:- debug(kmc_30xx).



assert_schema_kmc_30xx(G):-
  rdfs_assert_subclass(stcnv:'Author', foaf:'Agent', G),
  rdfs_assert_label(stcnv:'Author', nl, auteur, G),
  
  rdfs_assert_property_class(stcnv:'AuthorProperty', G),
  rdfs_assert_label(stcnv:'AuthorProperty', nl, autheurseigenschap, G),
  
  rdf_assert_property(stcnv:author, G),
  rdfs_assert_label(stcnv:author, nl, 'heeft auteur', G),
  
  rdf_assert_property(stcn:author_name, G),
  rdfs_assert_label(stcnv:author_name, nl, 'heeft auteursnaam', G).

link_to_dbpedia_agent(G, Agent):-
  rdf_datatype(Agent, foaf:name, string, Name, G),
  
  rdf_datatype(Agent, stcnv:birth, gYear, Birth, G),
  rdfs_assert_label(stcnv:birth, nl, geboortejaar, G),
  
  rdf_datatype(Agent, stcnv:death, gYear, Death, G),
  rdfs_assert_label(stcnv:death, nl, sterftejaar, G),
  
  find_dbpedia_agent(Name, Birth, Death, DBpediaAgent),
  owl_assert_resource_identity(Agent, DBpediaAgent, G),
  debug(
    dbpedia,
    'Agent ~w linked to DBpedia agent ~w.',
    [Agent,DBpediaAgent]
  ).

link_to_dbpedia_agents(G):-
  setoff(
    Agent,
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      rdf_subject(G, Agent)
    ),
    Agents
  ),
  run_on_sublists(Agents, link_to_dbpedia_agents(G)).

link_to_dbpedia_agents(G, Agents):-
  maplist(link_to_dbpedia_agent(G), Agents).

kmc_30xx(G, PPN, Predicate) -->
  dcg_until([end_mode(exclusive)], exclamation_mark, _Codes),
  % PPN codes for authors can contain non-numbers,
  % e.g., '14895961X' for 'Livinus van der Minnen'.
  ppn(AuthorPPN),
  exclamation_mark,
  {
    rdf_global_id(stcn:AuthorPPN, Author),
    rdf_assert_individual(Author, foaf:'Agent', G),
    rdf_assert(PPN, Predicate, Author, G)
  }.

% @tbd Use threading via run_on_sublists/2.

populate_dbpedia(DBpediaG):-
  forall(
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      owl_resource_identity(Agent, DBpedia_Agent),
      rdf_global_id(dbpedia:_, DBpedia_Agent)
    ),
    (
      describe_resource(dbpedia, DBpedia_Agent, Rows),
      forall(
        member(row(Predicate, Object), Rows),
        (
          rdf_assert(DBpedia_Agent, Predicate, Object, DBpediaG),
          flag(populate_dbpedia_triples, ID1, ID1 + 1)
        )
      ),
      flag(populate_dbpedia_agents, ID2, ID2 + 1)
    )
  ).

statistics_kmc_30xx(G, [[A1,V1],[A2,V2],[A3,V3],[A4,V4],[A5,V5],[A6,V6]]):-
  A1 = 'Publications with at least one author',
  count_subjects(stcnv:author, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1,V1]),

  A2 = 'Publications with at least one DBpedia author',
  setoff(
    DBpediaAuthorPPN,
    (
      rdfs(DBpediaAuthorPPN, stcnv:author, AuthorPPN, G),
      owl_resource_identity(AuthorPPN, _DBpediaAuthor),
      rdf_global_id(dbpedia:_, DBpediaAuthor)
    ),
    DBpediaAuthorPPNs
  ),
  length(DBpediaAuthorPPNs, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2,V2]),

  A3 = 'Number of authors (including pseudonyms)',
  count_individuals(stcnv:'Author', G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3,V3]),

  A4 = 'Number of DBpedia authors (including pseudonyms)',
  setoff(
    DBpediaAuthor,
    (
      rdfs(_PPN, stcnv:author, AuthorPPN, G),
      owl_resource_identity(AuthorPPN, DBpediaAuthor),
      rdf_global_id(dbpedia:_, DBpediaAuthor)
    ),
    DBpediaAuthors
  ),
  length(DBpediaAuthors, V4),
  debug(stcn_statistics, '-- ~w: ~w', [A4,V4]),

  A5 = 'Publications written under at least one pseudonym',
  setoff(
    PPN_Pseudonym,
    (
      rdfs(PPN_Pseudonym, stcnv:author, Author, G),
      rdf_literal(Author, stcnv:pseudonym, Pseudonym, G)
    ),
    PPN_Pseudonyms
  ),
  length(PPN_Pseudonyms, V5),
  debug(stcn_statistics, '-- ~w: ~w', [A5,V5]),

  A6 = 'Number of pseudonyms',
  setoff(
    Pseudonym,
    rdf_literal(_Author, stcnv:pseudonym, Pseudonym, G),
    NumberOfPseudonyms
  ),
  length(NumberOfPseudonyms, V6),
  debug(stcn_statistics, '-- ~w: ~w', [A6,V6]).

