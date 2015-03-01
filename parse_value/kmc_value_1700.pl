:- module(kmc_value_1700, []).

/** <module> Parse KMC Value: 1700

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- use_module(stcn(kmc/kmc_1700)).

:- multifile(kmc:kmc_value//3).





kmc:kmc_value(1700, Graph, Publication) -->
  "/",
  country_property(Property),
  atom(LocalName0),
  (   {
        upcase_atom(LocalName0, LocalName),
        rdf_global_id('iso3166-1':LocalName, Country),
        rdf(Country, _, _, 'iso3166-1')
      }
  ;   {unrecognized_country(LocalName0, _)},
      {
        debug(
          kmc_1700,
          '[Publication ~w] Unrecognized country code: ~w.',
          [Publication,LocalName0]
        ),
        atomic_list_concat(['Country',LocalName0], '/', LocalName00),
        rdf_global_id(stcno:LocalName00, Country)
    }
  ), !,
  {rdf_assert(Publication, Property, Country, Graph)}.

% The country that is mentioned in the publication itself.
country_property(Property) -->
  "1",
  {rdf_global_id(stcno:displayed_country, Property)}.
% Actual country of publication.
country_property(Property) -->
  "2",
  {rdf_global_id(stcno:actual_country, Property)}.
