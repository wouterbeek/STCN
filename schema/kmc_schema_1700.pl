:- module(kmc_schema_1700, []).

/** <module> Schema: KMC 1700 (Country)

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(management/rdf_load_any)).

:- use_module(stcn(kmc/kmc_1700)).

:- multifile(kmc:assert_kmc_schema/2).





kmc:assert_kmc_schema(1700, Graph):-
  rdf_assert_property(stcno:landcode, Graph),
  rdfs_assert_label(stcno:landcode, [nl]-'land van uitgave', Graph),
  rdf_assert_string(stcno:landcode, stcno:kb_name, 'KMC 1700', Graph),
  rdfs_assert_seeAlso(
    stcno:landcode,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1700.html',
    Graph
  ),
  rdfs_assert_seeAlso(
    stcno:landcode,
    'http://support.oclc.org/ggc/richtlijnen/?id=12&ln=nl&sec=k-1700',
    Graph
  ),
  rdfs_assert_domain(stcno:landcode, stcno:'Publication', Graph),
  rdfs_assert_range(stcno:landcode, 'iso3166-1':'Country', Graph),

  rdfs_assert_subproperty(stcno:displayed_country, stcno:landcode, Graph),
  rdfs_assert_label(
    stcno:displayed_country,
    [nl]-'weergegeven land van uitgave',
    Graph
  ),
  rdfs_assert_comment(
    stcno:displayed_country,
    [nl]-'In geval van een gefingeerd of onjuist impressum wordt in /2 de \c
          landcode opgenomen die hoort bij het juiste impressum zoals dat \c
          in een annotatie is verantwoord.',
    Graph
  ),

  rdfs_assert_subproperty(stcno:actual_country, stcno:landcode, Graph),
  rdfs_assert_label(
    stcno:actual_country,
    [nl]-'daadwerkelijk land van uitgave',
    Graph
  ),

  rdfs_assert_class(stcno:'Country', Graph),
  rdfs_assert_label(
    stcno:'Country',
    'OCLC country code not supported by ISO',
    Graph
  ),

  forall(
    unrecognized_country(Abbr1, Name),
    (
      atomic_list_concat(['Country',Abbr1], '/', Abbr2),
      rdf_global_id(stcno:Abbr2, Abbr3),
      rdf_assert_instance(Abbr3, stcno:'Country', Graph),
      rdfs_assert_label(Abbr3, Name, Graph)
    )
  ),
  % Add Dutch labels for countries that occur in the OCLC.
  rdf_load_any(
    stcn('rdf/iso3166-1.ttl'),
    [format(turtle),graph('iso3166-1')]
  ),
  forall(
    (
      recognized_country(LocalName0, NL_Name),
      upcase_atom(LocalName0, LocalName),
      rdf_global_id('iso3166-1':LocalName, Country)
    ),
    rdfs_assert_label(Country, [nl]-NL_Name, Graph)
  ).
