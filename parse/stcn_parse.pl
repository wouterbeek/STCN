:- module(
  stcn_parse,
  [
    redactiebladen//2 % +Graph:atom
                      % -PPN:atom
  ]
).

/** <module> STCN parse

Parser for the STCN redactiebladen file.

This parses 139.817 PPN entries in the redactiebladen file.

@author Wouter Beek
@version 2013/09, 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(pio)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_generics)).

:- use_module(plRdf(api/rdf_build)).

:- use_module(stcn(stcn_generics)).
:- use_module(stcn(stcn_kmc)).

:- assert(user:prolog_file_type(txt, text)).





redactiebladen(G, PPN) -->
  end_of_line, !,
  redactiebladen(G, PPN).
redactiebladen(G, _PPN) -->
  "SET", !,
  dcg_until([end_mode(inclusive)], atom('PPN: '), _),
  ppn('Publication', PPN),
  {
    flag(publications, N, N + 1),
    (N rem 1000 =:= 0 -> debug(stcn_parse, '~w PPNs parsed.', [N]) ; true),
    rdf_assert_instance(PPN, stcno:'Publication', G)
  },
  dcg_until([end_mode(inclusive)], end_of_line, _),
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  "Ingevoerd", !,
  dcg_until([end_mode(inclusive)], end_of_line, _),
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  kmc_start(KMC), !,
  kmc(KMC, G, PPN),
  end_of_line,
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  dcg_until([end_mode(exclusive),output_format(atom)], end_of_line, Line), !,
  {debug(stcn_parse, '[~w] .... ~w', [PPN,Line])},
  end_of_line,
  redactiebladen(G, PPN).
redactiebladen(_G, _PPN) -->
  dcg_end, !.

