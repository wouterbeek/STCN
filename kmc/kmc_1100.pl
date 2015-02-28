:- module(
  kmc_1100,
  [
% SCHEMA ASSERTION
    assert_schema_kmc_1100/1, % +Graph:graph
% GRAMMAR
    kmc_1100//2, % +Graph:atom
                 % +PPN:uri
% STATISTICS
    statistics_kmc_1100/2 % +Graph:atom
                         % -Rows:list(list)
  ]
).

/** <module> KMC 1100 - Year

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
@tbd Add R output: year by number of publications.
@version 2013/01-2013/04, 2013/06, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_content)). % Meta-DCG.
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/list_ext)).

:- use_module(plNlp(dcg_year)).

:- use_module(plXsd(dateTime/xsd_dateTime_aux)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)). % Meta-call.
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).





assert_schema_kmc_1100(G):-
  rdf_assert_property(stcno:publication_year, G),
  rdfs_assert_label(stcno:publication_year, 'publication year', G),
  rdfs_assert_label(stcno:publication_year, [nl]-publicatiejaar, G),
  rdfs_assert_domain(stcno:publication_year, stcno:'Publication', G),
  rdfs_assert_range(stcno:publication_year, xsd:gYear, G),
  rdf_assert_string(stcno:publication_year, stcno:kb_name, 'KMC 1100', G),
  rdfs_assert_seeAlso(
    stcno:publication_year,
    'http://www.kb.nl/kbhtml/stcnhandleiding/1100.html',
    G
  ),
  rdf_assert_langstring(
    stcno:publication_year,
    stcno:picarta_name,
    [nl]-'Jaar',
    G
  ),

  rdfs_assert_subproperty(
    stcno:exact_publication_year,
    stcno:publication_year,
    G
  ),
  rdfs_assert_label(
    stcno:exact_publication_year,
    'exact publication year',
    G
  ),
  rdfs_assert_label(
    stcno:exact_publication_year,
    [nl]-'exact publicatiejaar',
    G
  ),

  rdfs_assert_subproperty(
    stcno:earliest_publication_year,
    stcno:publication_year,
    G
  ),
  rdfs_assert_label(
    stcno:earliest_publication_year,
    'earliest publication year',
    G
  ),
  rdfs_assert_label(
    stcno:earliest_publication_year,
    [nl]-'vroegste publicatiejaar',
    G
  ),

  rdfs_assert_subproperty(
    stcno:latest_publication_year,
    stcno:publication_year,
    G
  ),
  rdfs_assert_label(
    stcno:latest_publication_year,
    'latest publicationyear',
    G
  ),
  rdfs_assert_label(
    stcno:latest_publication_year,
    [nl]-'laatste publicatiejaar',
    G
  ).

% Year interval.
kmc_1100(G, PPN) -->
  year_interval(_, Y1-Y2), !,
  {
    newDateTime(Y1, _, _, _, _, _, _, DT1),
    rdf_assert_typed_literal(
      PPN,
      stcno:earliest_publication_year,
      DT1,
      xsd:gYear,
      G
    ),
    newDateTime(Y2, _, _, _, _, _, _, DT2),
    rdf_assert_typed_literal(
      PPN,
      stcno:latest_publication_year,
      DT2,
      xsd:gYear,
      G
    )
  }.
% Year point.
kmc_1100(G, PPN) -->
  year_point(_, Y), !,
  {
    newDateTime(Y, _, _, _, _, _, _, DT),
    rdf_assert_typed_literal(
      PPN,
      stcno:exact_publication_year,
      DT,
      xsd:gYear,
      G
    )
  }.
% Cannot parse.
kmc_1100(_G, PPN) -->
  dcg_until(end_of_line, Line, [end_mode(exclusive),output_format(atom)]),
  {debug(kmc_1100, '[PPN ~w] Could not parse KMC 1100: ~w', [PPN,Line])}.

statistics_kmc_1100(G, [[A1,V1],[A2,V2],[A3,V3]|T]):-
  A1 = 'Publications that are dated',
  count_subjects(stcno:publication_year, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1,V1]),

  A2 = 'Publications that are exactly dated',
  count_subjects(stcno:exact_publication_year, _, G, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2,V2]),

  A3 = 'Publications that are approximately dated',
  count_subjects(stcno:earliest_publication_year, _, G, V3),
  debug(stcn_statistics, '-- ~w: ~w', [A3,V3]),

  aggregate_all(
    set(Year),
    rdf_typed_literal(_, stcno:exact_publication_year, Year, xsd:gYear, G),
    Years
  ),
  (   Years == []
  ->  % No exact year, so no years range, so no debug statement.
      T = []
  ;   first(Years, FirstYear),
      last(Years, LastYear),
      A4 = 'Year range',
      format(atom(V4), '~w ~w', [FirstYear,LastYear]),
      debug(stcn_statistics, '-- ~w: ~w.', [A4,V4]),
      T = [[A4,V4]]
  ).

