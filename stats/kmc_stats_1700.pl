:- module(kmc_stats_1700, []).

/** <module> STCN Statistics: KMC 1700

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(http/html_write)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plRdf(vocabulary/rdf_stats)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(1700, Graph) -->
  html([
    h3('Publications with some country'),
    {count_subjects(stcno:landcode, _, Graph, V1)},
    \html_pl_term(thousands_integer(V1)),
    
    h3('Publication with at least one displayed country'),
    {count_subjects(stcno:displayed_country, _, Graph, V2)},
    \html_pl_term(thousands_integer(V2)),
    
    h3('Publications with at least one actual country'),
    {count_subjects(stcno:actual_country, _, Graph, V3)},
    \html_pl_term(thousands_integer(V3)),
    
    h3('Countries used'),
    {count_objects(_, stcno:country, Graph, V4)},
    \html_pl_term(thousands_integer(V4))
  ]).
