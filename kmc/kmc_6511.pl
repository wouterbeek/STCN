:- module(
  kmc_6511,
  [
    assert_schema_kmc_6511/1, % +Graph:graph
    kmc_6511//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc6511/2 % +Graph:atom
                          % -Rows:list(list)
  ]
).

/** <module> KMC 6511 - TOPICS

# Numbers taken from _|redactiebladen|_ file

These numbers were calculated using the following regular expression schema:
_|\n6511!|_.

| *KMC*   | *Occurrences* |
| 6511    | 136.272       |
| 6512    |  97.020       |
| 6513    |  50.100       |
| 6514    |  10.373       |
| 6515    |   2.119       |
| 6516    |     777       |
| 6517    |     543       |
| 6518    |     154       |
| 6519    |       1       |
| All     | 297.359       |
| Parsed  | 297.293       |

Perform an RE search that matches on "\nKMC !PPN", i.e. without the trailing
exclamation mark.

# Parsing problems

PPN 324985231 has no exclamation mark after the topic PPN in KMC 6512.

@author Wouter Beek
@version 2013/03, 2013/06, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plc(dcg/dcg_ascii)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).

:- use_module(stcn(stcn_generic)).





assert_schema_kmc_6511(G):-
  rdfs_assert_class(stcno:'Topic', G),
  rdf_assert_property(stcno:topic, G),
  rdfs_assert_label(stcno:topic, [nl]-onderwerp, G),
  rdf_assert_string(stcno:topic, stcno:kb_name, 'KMC 6511', G),
  rdfs_assert_seeAlso(
    stcno:topic,
    'http://www.kb.nl/kbhtml/stcnhandleiding/6511.html',
    G
  ),
  rdf_assert_langstring(
    stcno:topic,
    stcno:picarta_name,
    [nl]-'Onderwerpstrefwoord',
    G
  ).

kmc_6511(G, PPN) -->
  exclamation_mark,
  ppn('Topic', TopicPPN),
  exclamation_mark,
  {
    ppn_to_topic(G, TopicPPN, Topic),
    rdf_assert(PPN, stcno:topic, Topic, G)
  }.

ppn_to_topic(_G, TopicPPN, Topic):-
  rdf_global_id(stcno:TopicPPN, Topic),
  rdfs_individual_of(Topic, stcno:'Topic'), !.
ppn_to_topic(G, TopicPPN, Topic):-
  rdf_global_id(stcno:TopicPPN, Topic),
  rdf_assert_instance(Topic, stcno:'Topic', G).

statistics_kmc6511(_G, []).

