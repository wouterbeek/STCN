:- module(picarta_value_1200, []).

/** <module> Picarta Parser: KMC 1200

This applies to KMC 1200 values as they occur in content that
was scraped from Picarta.

---

@author Wouter Beek
@version 2015/02
*/

:- use_module(plc(dcg/dcg_bracket)).
:- use_module(plc(dcg/dcg_content)).

:- use_module(stcn(kmc/kmc_1200)).

:- multifile(kmc:picarta_value//3).





kmc:picarta_value(1200, Graph, Publication) -->
  [Code],
  {code_type(Code, alnum)},
  " ",
  bracketed('...'),
  {
    char_code(Char1, Code),
    kmc_1200_table(Category, Char1, _, _),
    kmc_1200_table(Category, Relation1, _),
    rdf_global_id(picarta:Relation1, Relation2),
    atomic_list_concat(['TypografischKenmerk',Category,Char1], '/', Char2),
    rdf_global_id(stcno:Char2, Char3),
    rdf_assert(Publication, Relation2, Char3, Graph)
  }.
kmc:picarta_value(1200, _, Publication) -->
  {debug(
    kmc_1200,
    '[Publication ~w] Cannot parse Picarta value.',
    [Publication]
  )}.
