:- module(kmc_schema_4043, []).

/** <module> Schema: KMC 4043 (Bookseller/Publisher)

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(management/rdf_load_any)).

:- use_module(stcn(kmc/kmc_4043)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_schema_kmc(4043, Graph):-
  rdfs_assert_class(stcno:'Printer', Graph),
  rdfs_assert_label(stcno:'Printer', [nl]-drukker, Graph),
  
  rdf_assert_property(stcno:printer, Graph),
  rdfs_assert_label(stcno:printer, [nl]-'heeft drukker', Graph),
  rdf_assert_string(stcno:printer, stcno:kb_name, 'KMC 4043', Graph),
  rdfs_assert_seeAlso(
    stcno:printer,
    'http://www.kb.nl/kbhtml/stcnhandleiding/4043.html',
    Graph
  ),
  rdf_assert_langstring(
    stcno:printer,
    stcno:picarta_name,
    [nl]-'Drukker / Uitgever',
    Graph
  ),
  
  rdfs_assert_label(stcno:'City', stad, nl, Graph),
  % Assert the cities that act as printers/publishers.
  forall(
    city_printer(CityName, PPN),
    (
      rdf_global_id(stcn:PPN, Resource),
      rdf_assert_instance(Resource, stcno:'City', Graph),
      rdfs_assert_label(Resource, CityName, Graph)
    )
  ).
