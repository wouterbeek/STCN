:- module(kmc_stats_1200, []).

/** <module> STCN Statistics: KMC 1200

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(http/html_write)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(1200, Graph) -->
  html([
    h3('Typografosch kenmerk (KMC 1200)'),
    \rdf_html_triple_table(_, stcno:typographic_property, _, Graph, plTabular)
  ]).
