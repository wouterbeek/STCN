:- module(
  stcn_parse,
  [
    parse_redactiebladen/0
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
:- use_module(dcg(dcg_multi)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(standards(abnf)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_kmc)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

number_of_redactiebladen_batches(4).
redactiebladen_graph('STCN_Redactiebladen').

:- assert(user:prolog_file_type(txt, text)).

:- debug(stcn_parse).



parse_redactiebladen:-
  G = stcn,
  absolute_file_name(
    data(redactiebladen),
    File,
    [access(read),file_type(text)]
  ),
  phrase_from_file(redactiebladen(G, _PPN), File).

redactiebladen(G, PPN) -->
  'CRLF', !,
  redactiebladen(G, PPN).
redactiebladen(G, _PPN) -->
 "SET",
  dcg_until([end_mode(inclusive)], atom('PPN: '), _),
  ppn(PPN),
  dcg_until([end_mode(inclusive)], 'CRLF', _), !,
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  "Ingevoerd",
  dcg_until([end_mode(inclusive)], 'CRLF', _),
  % Second line.
  "11", 'CRLF', !,
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  dcg_multi(decimal_digit, 4, Codes, []),
  " ",
  {atom_codes(KMC, Codes), gtrace},
  kmc(KMC, G, PPN), !,
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  dcg_until([end_mode(exclusive),output_format(atom)], 'CRLF', Line),
  {debug(stcn_parse, '[~w] .... ~w', [PPN,Line])},
  redactiebladen(G, PPN).

