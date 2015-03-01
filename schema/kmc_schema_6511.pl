:- module(kmc_schema_6511, []).

/** <module> Schema: KMC 6511 (Topics)

@author Wouter Beek
@version 2015/02
*/

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(6511, Graph):-
  rdfs_assert_class(stcno:'Topic', Graph),
  rdf_assert_property(stcno:topic, Graph),
  rdfs_assert_label(stcno:topic, [nl]-onderwerp, Graph),
  rdf_assert_string(stcno:topic, stcno:kb_name, 'KMC 6511', Graph),
  rdfs_assert_seeAlso(
    stcno:topic,
    'http://www.kb.nl/kbhtml/stcnhandleiding/6511.html',
    Graph
  ),
  rdf_assert_langstring(
    stcno:topic,
    stcno:picarta_name,
    [nl]-'Onderwerpstrefwoord',
    Graph
  ).
