:- module(kmc_value_1100, []).

/** <module> Parse KMC Value: 1100 (Year)

Required field. Cannot be repeated.

KMC 1100 encodes the year of publishing.

Form
====

Exact year
----------

Four digits.

If the impressum contains a year of publishing, then this is encoded in
kmc_1100.pl. This also applies to years that occur in square brackets.

Approximate periods
-------------------

If no reliable year appears in the impressum, then either one or two =X='es
are added to the certain part of the year.

Examples with =X=:

| *Impressum*    | *|KMC 1100|* |
| 1744           | =1744=       |
| [1744]         | =1744=       |
| c.[spatie]1744 | =174X=       |
| 1744?          | =174X=       |

Examples with `XX`:

| *Impressum*                     | *|KMC 1100|*                      |
| [2nd half 18th century]         | =17XX=                            |
| [Late 16th, early 17th century] | =16XX= (and =s##= in KMC 700X)    |
|                                 | or =17XX= (and =S##= in KMC 700X) |

but,

| *Impressum*                     | *|KMC 1100|*                           |
| [Late 18th, early 19th century] | In general this is encoded as =17XX=   |
|                                 | (and =S##= in KMC 700X), so not =18XX= |

Also see kmc_4040.pl.

---

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/0500.html
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_content)).

:- use_module(plNlp(dcg_year)).

:- use_module(plXsd(dateTime/xsd_dateTime_aux)).

:- use_module(plRdf(api/rdf_build)).

:- multifile(kmc:kmc_value//3).





% Year interval.
kmc:kmc_value(1100, Publication, Graph) -->
  year_interval(_, Y1-Y2), !,
  {
    newDateTime(Y1, _, _, _, _, _, _, DT1),
    rdf_assert_typed_literal(
      Publication,
      stcno:earliest_publication_year,
      DT1,
      xsd:gYear,
      Graph
    ),
    newDateTime(Y2, _, _, _, _, _, _, DT2),
    rdf_assert_typed_literal(
      Publication,
      stcno:latest_publication_year,
      DT2,
      xsd:gYear,
      Graph
    )
  }.
% Year point.
kmc:kmc_value(1100, Publication, Graph) -->
  year_point(_, Y), !,
  {
    newDateTime(Y, _, _, _, _, _, _, DT),
    rdf_assert_typed_literal(
      Publication,
      stcno:exact_publication_year,
      DT,
      xsd:gYear,
      Graph
    )
  }.
% Cannot parse.
kmc:kmc_value(1100, Publication, _) -->
  '...',
  end_of_line, !,
  {debug(kmc_1100, '[PPN ~w] Could not parse KMC 1100.', [Publication])}.
