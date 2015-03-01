:- module(kmc_value_1700, []).

/** <module> Parse KMC Value: 1700 (Sorting Title)

Facultative field. Cannot be repeated.

KMC 3210 contains the sorting title of bibles and anonymous popular proze.

# § 65

## Bibles

Bibles and anonymous popular proze receive a sorting title in KMC 3210.
The following table gives the sorting titles for popular bible books.

!!kmc_3210!!bible_book!!2!!

The basic structure for bibles is as follows:

```text
'@Bible' + LANGUAGE + ((';' + PART)! + ' ' + BOOK (+ ' ' + SELECTION)!)!
```

Examples:

```text
@Bible Polyglot ; O.T. Job
@Bible Dutch ; O.T. Song of Solomon
@Bible Dutch
@Bible Dutch ; O.T. Apocrypha Tobit. Selection
@Bible French ; Metrical Psalms
```

Older descriptions sometimes have a sorting code appearing before the =@=.
These codes can be neglected.

Metrical psalms are described distinctly and do not appear in between the
other psalm prints. After the =/= the poet who put the text on rhyme is
mentioned, after =|Rhymed version by|= instead of =Adaptation=,
=|Translated from the French|=, etc. The name is taken up in thesaurus style
in KMC 3011.

Examples:

```text
kmc 4000: De @CL psalmen Davids. / Rhymed version by P. Dathenus
kmc 3011: Petrus@Dathenus!068066961!
```

If the poet who put the text on rhyme is unknown, then use =|Rhymed version|=.

Used for prints of the _Talmud_:
=|Talmud Bavli|= or =|Talmud Yerusalmi|=  for the Babylonian and the
Jerusalem Talmud, respectively.

## Anonymous popular works

Anonymous popular works also receive a sorting title in KIMC 3210 (see table).

The following table enumerates the anonymous popular works.

!!kmc_3210!!anonymous_popular!!2!!

The same holds for several other works, i.e., =Almanak=, =Geuzenliedboek= and
=Koran=.

=Almanak= includes all kinds of [almanakken], including [comptoiralmanachs],
provided that they appear yearly. The editors are included in KMC 3011.
In KMC 4000 the editors are mentioned in the order in which they appear on the
title page, preceded by =By= or =|Compiled by|=.

Publications of the Heidelberg catechismus (first question: "Welke is uw
enige troost beide in't leven ende sterven?") and the Augsburg confession
(first article: "Eerstelijk wordt eendrachtig geleerd ende gehouden naar het
besluit des Concilii Nicaeni") receive as sorting title in KMC 3210
=|@Heidelberg Catechism (+ LANGUAGE)|= and
=|@Augsburg Confession (+ LANGUAGE)|= respectively.

Note that additions such as =profeet=, for instance in case of prophet David,
are not included in a 4-2-2-1 search. [?]

---

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- use_module(stcn(kmc/kmc_3210)).

:- multifile(kmc:kmc_value//3).





kmc:kmc_value(3210, Graph, Publication) -->
  "&",
  {anonymous_popular(Title)},
  atom(Title),
  {
    rdf_assert_string(Publication, stcno:title, Title, Graph),
    debug(
      kmc_3210,
      'Recognized title \'~w\' for publication ~w.',
      [Title,Publication]
    )
  }.
kmc_3210(Graph, Publication) -->
  "&",
  atom('Bible'),
  " ",
  atom(Language),
  % Just checking!
  {rdf(Publication, stcn:language, Language, Graph)}.
kmc_3210(_, Publication) -->
  {debug(kmc_3210, 'Cannot parse title for publication ~w.', [Publication])}.
