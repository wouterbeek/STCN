:- module(
  kmc_30xx,
  [
    assert_schema_kmc_30xx/1, % +Graph:graph
    kmc_30xx//3, % +Graph:atom
                 % +PPN:uri
                 % +Predicate
    populate_dbpedia/1, % +DBpedia_Graph:atom
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
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plc(dcg/dcg_ascii)). % Meta-DCG.
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(process/thread_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(owl/owl_build)).
:- use_module(plRdf(owl/owl_read)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(stcn(stcn_generic)).

:- rdf_meta(link_to_dbpedia_agent(+,r)).





assert_schema_kmc_30xx(G):-
  % Author.
  rdfs_assert_subclass(stcno:'Author', foaf:'Agent', G),
  rdfs_assert_label(stcno:'Author', author, en, G),
  rdfs_assert_label(stcno:'Author', auteur, nl, G),

  % Has author.
  rdf_assert_property(stcno:author, G),
  rdfs_assert_label(stcno:author, 'has author', en, G),
  rdfs_assert_label(stcno:author, 'heeft auteur', nl, G),

  % Author name.
  rdf_assert_property(stcn:author_name, G),
  rdfs_assert_label(stcno:author_name, 'has author name', en, G),
  rdfs_assert_label(stcno:author_name, 'heeft auteursnaam', nl, G).

link_to_dbpedia_agent(G, Agent):-
  rdf_string(Agent, foaf:name, Name, G),

  rdf_datatype(Agent, stcno:birth, Birth, xsd:gYear, G),
  rdfs_assert_label(stcno:birth, geboortejaar, nl, G),

  rdf_datatype(Agent, stcno:death, Death, xsd:gYear, G),
  rdfs_assert_label(stcno:death, sterftejaar, nl, G),

  sparql_select(dbpedia, [dbp,foaf], [writer], [
      rdf(var(writer), rdf:type, foaf:'Person'),
      rdf(var(writer), rdfs:label, var(label)),
      filter(regex(var(label), string(Name), [case_insensitive])),
      rdf(var(writer), dbpprop:dateOfBirth, var(birth)),
      filter(regex(var(birth), string(Birth))),
      rdf(var(writer), dbpprop:dateOfDeath, var(death)),
      filter(regex(var(death), string(Death)))
    ], DBpediaAgent, [distinct(true),limit(1)]).
  owl_assert_resource_identity(Agent, DBpediaAgent, G),
  debug(
    dbpedia,
    'Agent ~w linked to DBpedia agent ~w.',
    [Agent,DBpediaAgent]
  ).

link_to_dbpedia_agents(G):-
  aggregate_all(
    set(Agent),
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      rdf_subject(Agent, G)
    ),
    Agents
  ),
  run_on_sublists(Agents, link_to_dbpedia_agents(G), 10).

link_to_dbpedia_agents(G, Agents):-
  maplist(link_to_dbpedia_agent(G), Agents).

kmc_30xx(G, PPN, Pred) -->
  dcg_until([end_mode(exclusive)], exclamation_mark, _Codes),
  ppn('Author', AuthorPPN),
  "!",
  {
    rdf_global_id(stcn:AuthorPPN, Author),
    rdf_assert(PPN, Pred, Author, G)
  }.

populate_dbpedia(DBpediaG):-
  forall(
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      owl_resource_identity(Agent, DBpedia_Agent),
      rdf_global_id(dbpedia:_, DBpedia_Agent)
    ),
    (
      sparql_cache(DBpedia_Agent, _, Propositions),
      forall(
        member([S,P,O], Propositions),
        (
          rdf_assert(S, P, O, DBpediaG),
          flag(populate_dbpedia_triples, Id1, Id1 + 1)
        )
      ),
      flag(populate_dbpedia_agents, Id2, Id2 + 1)
    )
  ).

statistics_kmc30xx(G, [[A1,V1],[A2,V2],[A3,V3],[A4,V4],[A5,V5],[A6,V6]]):-
  A1 = 'Publications with at least one author',
  count_subjects(stcno:author, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1,V1]),

  A2 = 'Publications with at least one DBpedia author',
  aggregate_all(
    set(DBpediaAuthorPPN),
    (
      rdfs(DBpediaAuthorPPN, stcno:author, AuthorPPN, G),
      owl_resource_identity(AuthorPPN, _DBpediaAuthor),
      rdf_global_id(dbpedia:_, DBpediaAuthor)
    ),
    DBpediaAuthorPPNs
  ),
  length(DBpediaAuthorPPNs, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2,V2]),

  A3 = 'Number of authors (including pseudonyms)',
  count_individuals(stcno:'Author', G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3,V3]),

  A4 = 'Number of DBpedia authors (including pseudonyms)',
  aggregate_all(
    set(DBpediaAuthor),
    (
      rdfs(_PPN, stcno:author, AuthorPPN, G),
      owl_resource_identity(AuthorPPN, DBpediaAuthor),
      rdf_global_id(dbpedia:_, DBpediaAuthor)
    ),
    DBpediaAuthors
  ),
  length(DBpediaAuthors, V4),
  debug(stcn_statistics, '-- ~w: ~w', [A4,V4]),

  A5 = 'Publications written under at least one pseudonym',
  aggregate_all(
    set(PPN_Pseudonym),
    (
      rdfs(PPN_Pseudonym, stcno:author, Author, G),
      rdf_string(Author, stcno:pseudonym, Pseudonym, G)
    ),
    PPN_Pseudonyms
  ),
  length(PPN_Pseudonyms, V5),
  debug(stcn_statistics, '-- ~w: ~w', [A5,V5]),

  A6 = 'Number of pseudonyms',
  aggregate_all(
    set(Pseudonym),
    rdf_string(_, stcno:pseudonym, Pseudonym, G),
    NumberOfPseudonyms
  ),
  length(NumberOfPseudonyms, V6),
  debug(stcn_statistics, '-- ~w: ~w', [A6,V6]).

