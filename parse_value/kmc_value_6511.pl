:- module(kmc_value_6511, []).

/** <module> Parse KMC Value: 6511 (Topics)

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

---

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdf_build)).

:- use_module(stcn(stcn_generics)).

:- multifile(kmc:kmc_value//3).





kmc:kmc_value(6511, Graph, Publication) -->
  exclamation_mark,
  ppn('Topic', TopicPPN),
  "!",
  {
    ppn_to_topic(Graph, TopicPPN, Topic),
    rdf_assert(Publication, stcno:topic, Topic, Graph)
  }.

ppn_to_topic(_, TopicPPN, Topic):-
  rdf_global_id(stcno:TopicPPN, Topic),
  rdfs_individual_of(Topic, stcno:'Topic'), !.
ppn_to_topic(Graph, TopicPPN, Topic):-
  rdf_global_id(stcno:TopicPPN, Topic),
  rdf_assert_instance(Topic, stcno:'Topic', Graph).
