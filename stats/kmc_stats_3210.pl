:- module(kmc_stats_3210, []).

/** <module> STCN Statistics: KMC 3210

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(http/html_write)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plRdf(vocabulary/rdf_stats)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(3210, Graph) -->
  html([
    h3('Publications with title'),
    {count_subjects(stcno:title, _, Graph, V1)},
    \html_pl_term(thousands_separator(V1))
  ]).
