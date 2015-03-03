:- module(
  stcn_link,
  [
    link_to_dbpedia_agents/1 % +Graph:atom
  ]
).

/** <module> STCN Link

Predicates for asserting links between the STCN and other datasets.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/owl_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).

:- use_module(plSparql(query/sparql_query_api)).





%! link_to_dbpedia_agent(+Agent:iri, +Graph:atom) is det.

link_to_dbpedia_agent(Agent, G):-
  rdf_string(Agent, foaf:name, Name, G),

  rdf_typed_literal(Agent, stcno:birth, Birth, xsd:gYear, G),
  rdfs_assert_label(stcno:birth, [nl]-geboortejaar, G),

  rdf_typed_literal(Agent, stcno:death, Death, xsd:gYear, G),
  rdfs_assert_label(stcno:death, [nl]-sterftejaar, G),

  sparql_select(
    dbpedia,
    [dbp,foaf],
    [writer],
    [
      rdf(var(writer), rdf:type, foaf:'Person'),
      rdf(var(writer), rdfs:label, var(label)),
      filter(regex(var(label), string(Name), [case_insensitive])),
      rdf(var(writer), dbpprop:dateOfBirth, var(birth)),
      filter(regex(var(birth), string(Birth))),
      rdf(var(writer), dbpprop:dateOfDeath, var(death)),
      filter(regex(var(death), string(Death)))
    ],
    DBpediaAgent,
    [distinct(true),limit(1)]
  ),
  owl_assert_resource_identity(Agent, DBpediaAgent, G),
  debug(
    stcn_link,
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




populate_dbpedia:-
  forall(
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      owl_resource_identity(Agent, DBpedia_Agent),
      rdf_global_id(dbpedia:_, DBpedia_Agent)
    ),
    lod_cache_assert(DBpedia_Agent, [])
  ).
