:- module(kmc_stats_1100, []).

/** <module> STCN Statistics: KMC 0500

@author Wouter Beek
@tbd Use R for an exact publication years graphic.
@version 2015/02
*/

:- use_module(library(http/html_write)).

:- use_module(plc(dcg/dcg_bracket)).

:- use_module(plRdf(vocabulary/rdf_stats)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(1100, Graph) -->
  html([
    h3('Publications that are dated'),
    {count_subjects(stcno:publication_year, _, Graph, V1)},
    \html_pl_term(thousands_integer(V1)),
    
    h3('Publications that are exactly dated'),
    {count_subjects(stcno:exact_publication_year, _, Graph, V2)},
    \html_pl_term(thousands_integer(V2)),
    
    h3('Publications that are approximately dated'),
    {count_subjects(stcno:earliest_publication_year, _, Graph, V3)},
    \html_pl_term(thousands_integer(V3)),
    
    h3('Exact publication years'),
    {
      aggregate_all(
        set(Year),
        rdf_typed_literal(
          _,
          stcno:exact_publication_year,
          Year,
          xsd:gYear,
          Graph
        ),
        Years
      ),
      (   Years == []
      ->  % No exact year, so no years range, so no debug statement.
          FirstYear =0,
          LastYear = 0
      ;   first(Years, FirstYear),
          last(Years, LastYear)
      )
    },
    bracketed((
      \html_pl_term(thousands_integer(FirstYear)),
      '-',
      \html_pl_term(thousands_integer(LastYear))
    ))
  ).
