:- module(kmc_stats_1500, []).

/** <module> STCN Statistics: KMC 1500

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(http/html_write)).

:- use_module(plHtml(html_pl_term)).

:- use_module(plRdf(vocabulary/rdf_stats)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(1500, Graph) -->
  html([
    h3('Actual language (KMC 1500)'),
    \rdf_html_triple_table(_, stcno:actual_language, _, G, plTabular),
    
    h3('Translated via language (KMC 1500)'),
    \rdf_html_triple_table(_, stcno:translated_via, _, G, plTabular),
    
    h3('Translated from language (KMC 1500)'),
    \rdf_html_triple_table(_, stcno:translated_from, _, G, plTabular),
    
    h3('Publications with at least one language'),
    {count_subjects(stcno:actual_language, _, G, V1)},
    \html_pl_term(thousands_integer(V1)),
    
    h3('Publications with at least one actual language'),
    {count_subjects(stcno:actual_language, _, G, V2)},
    \html_pl_term(thousands_integer(V2)),
    
    h3('Publications that are translated via at least one value'),
    {count_subjects(stcno:translated_via, _, G, V3)},
    \html_pl_term(thousands_integer(V3)),
    
    h3('Publications translated from at least one language'),
    {count_subjects(stcno:translated_from, _, G, V4)},
    \html_pl_term(thousands_integer(V4)),
    
    h3('Publications that have no language information'),
    {count_subjects(stcno:language, stcn:'Unknown', G, V5)},
    \html_pl_term(thousands_integer(V5)),
    
    h3('Number of languages used'),
    {count_objects(_, stcno:language, G, V6)},
    \html_pl_term(thousands_integer(V6))
  ]).
