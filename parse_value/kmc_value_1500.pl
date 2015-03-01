:- module(kmc_value_1500, []).

/** <module> Parse KMC Value: 1500 (Language)

Format:

```text
/1[3-letter language code]
/2[3-letter language code]
/3[3-letter language code]
```

| *Format* | *Meaning* |
| =|/1|=   | One of the languages in which the work is written. |
| =|/2|=   | One of the languages of a work that this work is a translation of and that is itself _not_ the original language in which the work was written. |
| =|/3|=   | One of the languages of a work that this work is a translation of and that is itself the original language in which the work was written.. |

# Tables for language codes

## Recognized language codes (STCN and ISO overlap)

!!kmc_1500!!recognized_language!!2!!

## Language identities (in STCN)

!!kmc_1500!!same_language!!2!!

## Language code translations (from STCN to ISO)

!!kmc_1500!!translate_language!!2!!

## Special, non-language codes in STCN

Codes =mis= and =mul=.

The semantics of these is not documented?

## Unrecognitzed language codes (in STCN but not in ISO)

!!kmc_1500!!unrecognized_language!!2!!

## Language families (iso639-5)

Some STCN language codes do not denote languages but language families. These
are mapped to ISO 639-3 for language families.

!!kmc_1500!!'iso639-5'!!2!!

---

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/1500.html
@see http://support.oclc.org/ggc/richtlijnen/?id=12&ln=nl&sec=k-1500
@tbd See whether languages can be related to their language families.
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- multifile(kmc:kmc_value//3).





kmc:kmc_value(1500, Graph, Publication) -->
  "/",
  language_property(Property),
  language(Publication, Language),
  {rdf_assert(Publication, Property, Language, Graph)}, !,
  kmc_1500(Graph, Publication).

%! language_property(-Predicate:uri) is nondet.
% Parses a language code indicator (prefix) and returns
% the corresponding predicate resource.

% Language code '/1<language>'.
% Language used in the book.
language_property(Property) -->
  "1",
  {rdf_global_id(stcno:actual_language, Property)}.
% Language code '/2<language>'.
% Language translated from, which is not the original language, i.e., an
% in-between language.
language_property(Property) -->
  "2",
  {rdf_global_id(stcno:translated_via, Property)}.
% Language code '/3<language>'.
% Language translated from, which is the original language of the work.
language_property(Property) -->
  "3",
  {rdf_global_id(stcno:translated_from, Property)}.

%! language(+PPN:iri, -Language:iri)// is det.
% Processes STCN language codes and returns their ISO 639-3 correlate.

% Case 1: The STCN language codes stand for languages that are in the
%         ISO standards, but under different names.
%         Wouter Beek has made these translations (i.e. they may be wrong).
language(_, Lang) -->
  atom(STCN_Lang),
  {
    translate_language(STCN_Lang, ISO_Lang),
    rdf_global_id('iso639-3':ISO_Lang, Lang)
  }.
% Case 2: The STCN language codes do not denote a language but a family of
%         languages. These are found in the ISO 639-5 standard.
language(Publication, Lang) -->
  atom(LocalName),
  {
    rdf_global_id('iso639-5':LocalName, Lang),
    rdf(Lang, _, _),
    debug(
      kmc_1500,
      '[PPN ~w] Found a language family code: ~w',
      [Publication,Lang]
    )
  }.
% Case 3: Special language codes defined by GGC and STCN for KMC 1500
%         that do not identify any specific language.
language(_, Lang) -->
  ("mis" ; "mul"), !,
  {rdf_global_id(stcno:unknown, Lang)}.
% Case 4: A language code as defined by ISO 639-3.
language(Publication, Lang) -->
  atom(LocalName),
  {
    rdf_global_id('iso639-3':LocalName, Lang),
    rdf(Lang, _, _),
    debug(
      kmc_1500,
      '[PPN ~w] Found a language family code: ~w',
      [Publication,Lang]
    )
  }.

%! translate_language(?STCN:atom, ?ISO:atom) is nondet.
% Translations between STCN and ISO codes as verified by Wouter Beek.

translate_language(arm, hye).
translate_language(chi, cmn).
% Middlehigh German (ca. 1050-1500).
translate_language(dmh, gmh).
% ISO 639-3 distinguishes between the West-Frisian language
% (spoken in The Netherlands) and the North-Frisian language
% (spoken in Germany).
translate_language(fri, fry).
translate_language(grk, grc).
translate_language(hon, hun).
translate_language(jap, jpn).
translate_language(ned, nld).
translate_language(per, fas).
translate_language(zwe, swe).
