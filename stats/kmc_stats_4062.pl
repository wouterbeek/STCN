:- module(kmc_stats_4062, []).

/** <module> STCN Statistics: KMC 4062

@author Wouter Beek
@tbd Use R for an exact publication years graphic.
@version 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_bracket)).

:- ise_module(plHtml(html_pl_term)).

:- use_module(plRdf(vocabulary/rdf_stats)).

:- multifile(kmc:kmc_stats//2).





kmc:kmc_stats(4062, Graph) -->
  html([
    h3('Publications with an STCN format'),
    {count_subjects(stcno:format, _, Graph, V1)},
    \html_pl_term(thousands_integer(V1)),
    
    h3('Publications with no STCN format'),
    {count_subjects(stcno:format, stcno:unknown_format, Graph, V2)},
    \html_pl_term(thousands_integer(V2)),
    
    {
      aggregate_all(
        set(Name),
        format(Name, _, _, _, _, _),
        Names
      )
    },
    \format_stats(Graph, Names)
  ]).

format_stats(_, []) --> [].
format_stats(Graph, [Name|T]) -->
  {
    rdf_global_id(stcno:Name, Format),
    count_subjects(stcno:format, Format, Graph, V)
  },
  h3(['Publications with format ',Name]),
  \html_pl_term(thousands_integer(V)),
  \format_stats(Graph, T).
