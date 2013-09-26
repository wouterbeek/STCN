:- module(
  kmc_1100,
  [
    assert_schema_kmc_1100/1, % +Graph:graph
    kmc_1100//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc_1100/2 % +Graph:atom
			  % -Rows:list(list)
  ]
).

/** <module> KMC 1100 - YEAR

Required field. Cannot be repeated.

KMC 1100 encodes the year of publishing.

# Form

## Exact year

Four digits.

If the impressum contains a year of publishing, then this is encoded in
kmc_1100.pl. This also applies to years that occur in square brackets.

## Approximate periods

If no reliable year appears in the impressum, then either one or two =X='es
are added to the certain part of the year.

Examples with =X=:

| *Impressum*    | *|KMC 1100|* |
| 1744           | =1744=       |
| [1744]         | =1744=       |
| c.[spatie]1744 | =174X=       |
| 1744?          | =174X=       |

Examples with =XX=:

| *Impressum*                     | *|KMC 1100|*                      |
| [2nd half 18th century]         | =17XX=                            |
| [Late 16th, early 17th century] | =16XX= (and =s##= in KMC 700X)    |
|                                 | or =17XX= (and =S##= in KMC 700X) |

but,

| *Impressum*                     | *|KMC 1100|*                           |
| [Late 18th, early 19th century] | In general this is encoded as =17XX=   |
|                                 | (and =S##= in KMC 700X), so not =18XX= |

Also see kmc_4040.pl for dating.

@author Wouter Beek
@version 2013/01-2013/04, 2013/06, 2013/09
*/

:- use_module(dcg(dcg_year)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)). % Meta-call.
:- use_module(rdf(rdf_statistics)).
:- use_module(rdf(rdf_year)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- nodebug(kmc_1100).



assert_schema_kmc_1100(G):-
  rdf_assert_property(stcnv:publication_year, G),
  rdfs_assert_label(stcnv:publication_year, nl, publicatiejaar, G),
  rdf_assert_literal(stcnv:publication_year, stcnv:kb_name, 'KMC 1100', G),
  rdf_assert(stcnv:publication_year, stcnv:documentation,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1100.html', G),
  rdf_assert_literal(stcnv:publication_year, stcnv:picarta_name, nl, 'Jaar',
    G),
  rdfs_assert_class(stcnv:'PublicationYearProperty', G),
  rdfs_assert_property_class(stcnv:'PublicationYearProperty', G),
  
  rdf_assert_individual(stcn:exact_publication_year,
    stcnv:'PublicationYearProperty', G),
  rdfs_assert_label(stcn:exact_publication_year, nl, 'exact publicatiejaar',
    G),
  rdfs_assert_subproperty(stcn:exact_publication_year, stcnv:publication_year,
    G),
  
  rdf_assert_individual(stcn:earliest_publication_year,
    stcnv:'PublicationYearProperty', G),
  rdfs_assert_subproperty(stcn:earliest_publication_year,
    stcnv:publication_year, G),
  rdfs_assert_label(stcn:latest_publication_year, nl, 'eerste publicatiejaar',
    G),
  
  rdf_assert_individual(stcn:latest_publication_year,
    stcnv:'PublicationYearProperty', G),
  rdfs_assert_subproperty(stcn:latest_publication_year,
    stcnv:publication_year, G),
  rdfs_assert_label(stcn:latest_publication_year, nl,
    'laatste publicatiejaar', G).

% Note the order of these DCG rule clauses: first exactly 4, then exactly 3,
% and then exactly 2 digits.
kmc_1100(G, PPN) -->
  year(_Lang, Year),
  {
    % Assert the year (single point range, i.e., absence of uncertainty).
    rdf_assert_year(
      PPN,
      stcnv:earliest_publication_year,
      stcnv:latest_publication_year,
      stcnv:exact_publication_year,
      Year,
      G
    )
  }.

statistics_kmc_1100(G, [[A1,V1],[A2,V2],[A3,V3]|T]):-
  A1 = 'Publications that are dated',
  count_subjects(stcnv:publication_year, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1, V1]),

  A2 = 'Publications that are exactly dated',
  count_subjects(stcnv:exact_publication_year, _, G, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2, V2]),

  A3 = 'Publications that are approximately dated',
  count_subjects(stcnv:earliest_publication_year, _, G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3, V3]),

  setoff(
    Year,
    rdf_datatype(_PPN, stcnv:exact_publication_year, gYear, Year, G),
    Years
  ),
  (
    Years == []
  ->
    % No exact year, so no years range, so no debug statement.
    T = []
  ;
    first(Years, FirstYear),
    last(Years, LastYear),
    A4 = 'Year range',
    format(atom(V4), '~w ~w', [FirstYear,LastYear]),
    debug(stcn_statistics, '-- ~w: ~w.', [A4, V4]),
    T = [[A4,V4]]
  ).

