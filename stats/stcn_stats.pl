:- module(
  stcn_stats,
  [
    stcn_stats/1 % -Rows:list(list)
  ]
).

/** <module> STCN statistics

Statistics for the STCN dataset.

# Old parse results

## 2013/01

| *Name*                               | *Value*   |
| Number of parsed PPNs                | 126847    |
| -- Monographies                      | 120853    |
| -- Multi-part works                  | 6552      |
| -- Magazines                         | 514       |
| PPNs that are dated                  | 126847    |
| -- PPNs that are exactly dated       | 115146    |
| -- PPNs that are approximately dated | 12126     |
| -- Year range                        | 1473-1847 |
| PPNs with language                   | 126828    |
| -- PPNs with actual language         | 126825    |
| -- PPNs translated via               | 825       |
| -- PPNs translated from              | 22987     |
| -- Languages used                    | 52        |
| PPNs with country                    | 126819    |
| -- PPNs with displayed country       | 126815    |
| -- PPNs with actual country          | 7436      |
| -- Countries used                    | 33        |

## 2013/02

Note that only includes about half of the STCN data.

| *Name*                               | *Value*   |
| Parsed PPNs | 59000 |
| -- Monograph PPNs | 54544 |
| -- Multi-part PPNs | 4195 |
| -- Magazine PPNs | 249 |
| -- Unknown type PPNs | 12 |
| PPNs that are dated | 59000 |
| -- PPNs that are exactly dated | 51281 |
| -- PPNs that are approximately dated | 7719 |
| -- Year range | 1700-1779 |
| Number of typographic properties | 58793 |
| -- Number of book lists | 5774 |
| -- Number of font types | 58623 |
| -- Number of illustration | 19928 |
| -- Number of others | 3449 |
| -- Number of title pages | 58688 |
| PPNs with language | 58992 |
| -- PPNs with actual language | 58992 |
| -- PPNs translated via | 280 |
| -- PPNs translated from | 9592 |
| -- PPNs whose language is unknown | 311 |
| -- Languages used | 43 |
| PPNs with country | 58978 |
| -- PPNs with displayed country | 58978 |
| -- PPNs with actual country | 3795 |
| -- Countries used | 28 |
| PPNs with author | 39123 |
| -- PPNs with DBpedia author | 2635 |
| -- DBpedia authors (including pseudonyms) | 333 |
| -- Picarta authors (including pseudonyms) | 11863 |
| -- PPNs written under pseudonym | 897 |
| -- Pseudonyms | 192 |
| -- PPNs with primary author | 36153 |
| -- Primary Picarta authors | 9735 |
| -- Primary DBpedia authors | 278 |
| -- PPNs with secondary author | 10185 |
| -- Secondary Picarta authors | 4047 |
| -- Secondary DBpedia authors | 160 |
| PPNs with a format | 59000 |
| -- PPNs with format Plano | 10538 |
| -- PPNs with format Folio | 3992 |
| -- PPNs with format Quarto | 18379 |
| -- PPNs with format Octavo | 25998 |
| -- PPNs with format Duodecimolong | 8869 |
| -- PPNs with format Duodecimocommon | 96 |
| -- PPNs with format 16mo | 1553 |
| -- PPNs with format 18mo | 68 |
| -- PPNs with format 24mo-long | 147 |
| -- PPNs with format 24mo | 4 |
| -- PPNs with format 32mo | 33 |
| -- PPNs with format 64mo | 2 |
| -- PPNs with unknown format | 2 |

---

@author Wouter Beek
@version 2013/01-2013/03, 2013/06, 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).

:- use_module(plRdfEntailment(rdf_ent_stat)).

:- use_module(stcn(stats/kmc_stats_0500)).
:- use_module(stcn(stats/kmc_stats_1100)).
:- use_module(stcn(stats/kmc_stats_1200)).
:- use_module(stcn(stats/kmc_stats_1500)).
:- use_module(stcn(stats/kmc_stats_1700)).
:- use_module(stcn(stats/kmc_stats_3000)).
:- use_module(stcn(stats/kmc_stats_3011)).
:- use_module(stcn(stats/kmc_stats_4062)).





stcn_stats -->
  html([
    h3('Parsed PPNs'),
    {count_instances_by_class(stcno:'Publication', stcn, V1)},
    \html_pl_term(thousands_integer(V1)),
    
    \kmc_stats(500),
    \kmc_stats(1100),
    \kmc_stats(1200),
    \kmc_stats(1500),
    \kmc_stats(1700),
    \kmc_stats(3000),
    \kmc_stats(3011),
    \kmc_stats(4062)
  ]).

kmc_stats(500) -->
  html([
    h2('Status (KMC 0500)'),
    \rdf_html_triple_table(_, stcno:status, _, stcn, plTabular)
  ]).
