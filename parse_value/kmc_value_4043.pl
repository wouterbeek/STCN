:- module(kmc_value_4043, []).

/** <module> Parse KMC Value: 4043 (Bookseller/Publisher)

We expect that there are 222.585 publication/printer-pairs in the
redactiebladen. This number is obtained using the search string "\n4043[^ ]".

Parse cities
============

E.g. PPN 234597046.

!!kmc_4043!!city_printer!!2!!

Parse errors/exceptions
=======================

PPN 173117708
-------------

TBD

---

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_content)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- use_module(plRdfHtml(rdf_html_triple_table)).

:- use_module(stcn(stcn_generics)).

:- multifile(kmc:kmc_value//3).





kmc:kmc_value(4043, _, Publication) -->
  '...',
  "!",
  % PPN codes for printers can contain non-numbers (just like any other PPN).
  ppn('Printer', PrinterPPN),
  exclamation_mark,
  {
    ppn_resource(printer_publisher, PrinterPPN, Printer),
    rdf_assert(Publication, stcno:printer, Printer, stcn)
  }.
% E.g. PPN 317155091.
kmc:kmc_value(4043, _, _) -->
  atom('THESAUREREN').
% E.g. PPN 234597046.
kmc:kmc_value(4043, Graph, Publication) -->
  atom(PrinterName),
  {
    once(city_printer(PrinterName, Publication)),
    ppn_resource(printer_publisher, Publication, Printer),
    rdf_assert(Publication, stcno:printer, Printer, Graph)
  }.
