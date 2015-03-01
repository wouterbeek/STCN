:- module(kmc_stats_0500, []).

/** <module> STCN Statistics: KMC 0500

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(http/html_write)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(500, Graph) -->
  html([
    h2('Status (KMC 0500)'),
    \rdf_html_triple_table(_, stcno:status, _, Graph, plTabular)
  ]).
