:- module(
  stcn_parse,
  [
    parse_redactiebladen/1 % +Graph:atom
  ]
).

/** <module> STCN parse

Parser for the STCN redactiebladen file.

@author Wouter Beek
@version 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(rdf(rdf_build)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_kmc)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

:- assert(user:prolog_file_type(txt, text)).

:- debug(stcn_parse).



parse_redactiebladen(G):-
  absolute_file_name(
    data(redactiebladen2),
    File,
    [access(read),file_type(text)]
  ),

  phrase_from_file(
    redactiebladen(G, _PPN),
    File,
    [encoding(utf8),type(text)]
  ).

redactiebladen(G, PPN) -->
  end_of_line, !,
  redactiebladen(G, PPN).
redactiebladen(G, _PPN) -->
  "SET", !,
  dcg_until([end_mode(inclusive)], atom('PPN: '), _),
  ppn(PPN),
  {rdf_assert_individual(PPN, stcnv:'Publication', G)},
  dcg_until([end_mode(inclusive)], end_of_line, _),
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  "Ingevoerd", !,
  dcg_until([end_mode(inclusive)], end_of_line, _),
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  kmc_start(KMC), !,
  {gtrace},
  kmc(KMC, G, PPN),
  end_of_line,
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  dcg_until([end_mode(exclusive),output_format(atom)], end_of_line, Line), !,
  {debug(stcn_parse, '[~w] .... ~w', [PPN,Line])},
  end_of_line,
  redactiebladen(G, PPN).
redactiebladen(_G, _PPN) -->
  dcg_end, {gtrace}, !.

