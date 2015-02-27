:- module(
  stcn_clean,
  [
    stcn_clean/1 % +Graph:atom
  ]
).

/** <module> STCN clean

Cleaning the STCN database.

This assumes that the STCN_SCRAPE script has been successfully completed
and the STCN graph files are in the `data` subdirectory.

@author Wouter Beek
@version 2013/01-2013/03, 2013/06, 2013/09, 2014/03, 2015/02
*/

:- use_module(plc(dcg/dcg_generics)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).

:- use_module(stcn(kmc/kmc_1200)).





stcn_clean(G):-
  % Assert occurrences in literal enumerations as separate triples.
  rdf_update_literal(
    _,
    picarta:printer_publisher,
    _,
    _,
    _,
    G,
    split_lexical_form('; ')
  ),
  debug(stcn_script, 'Printer-publishers were split.', []),

  rdf_update_literal(
    _,
    picarta:printer_publisher,
    _,
    _,
    _,
    G,
    lexical_form(strip_atom([' ']))
  ),
  debug(stcn_script, 'Printer-publishers were stripped.', []),

  rdf_update_literal(
    _,
    picarta:topical_keyword,
    _,
    _,
    _,
    G,
    split_lexical_form('; ')
  ),
  debug(stcn_script, 'Topics were split.', []),

  rdf_update_literal(
    _,
    picarta:typographic_information,
    _,
    _,
    _,
    G,
    split_lexical_form(' , ')
  ),
  debug(stcn_script, 'Typographic information was split.', []),
  
  % KMC 1200
  forall(
    rdf_string(PPN, picarta:typographic_information, LexicalForm, G),
    (
      dcg_phrase(kmc_1200_picarta(G, PPN), LexicalForm),
      rdf_retractall_string(PPN, picarta:typographic_information, G)
    )
  ).

