:- module(
  stcn_clean,
  [
    stcn_clean/0
  ]
).

/** <module> STCN_CLEAN

Cleaning the STCN database.

This assumes that the STCN_SCRAPE script has been successfully completed
and the STCN graph files are in =|/Data|=.

@author Wouter Beek
@version 2013/01-2013/03, 2013/06, 2013/09
*/

:- use_module(kmc(kmc_1200)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_clean)).
:- use_module(rdf(rdf_lit)).
:- use_module(stcn(stcn_generic)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').



stcn_clean:-
  % KMC 1200
  forall(
    rdf_literal(PPN, stcnv:typographic_information, Lit, Graph),
    (
      atom_codes(Lit, Codes),
      phrase(kmc_1200_picarta(Graph, PPN), Codes)
    )
  ),
  rdf_literal_to_uri(_PPN1, stcnv:author, stcn, Graph),
  rdf_literal_to_uri(_PPN2, stcnv:printer_publisher, stcn, Graph),
  rdf_literal_to_uri(_PPN3, stcnv:translator_editor, stcn, Graph).

