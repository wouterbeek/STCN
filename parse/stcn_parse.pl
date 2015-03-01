:- module(
  stcn_parse,
  [
    redactiebladen//2 % +Graph:atom
                      % -Ppn:atom
  ]
).

/** <module> STCN parse

Parser for the STCN redactiebladen file.

This parses 139.817 PPN entries in the redactiebladen file.

@author Wouter Beek
@version 2013/09, 2015/02
*/

:- use_module(library(debug)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)). % Meta-argument.

:- use_module(plRdf(api/rdf_build)).

:- use_module(stcn(stcn_generics)).
:- use_module(stcn(parse/stcn_parse_generics)).

:- assert(user:prolog_file_type(txt, text)).





redactiebladen(Graph, Ppn) -->
  end_of_line, !,
  redactiebladen(Graph, Ppn).
redactiebladen(Graph, _) -->
  "SET", !,
  '...',
  "PPN: ", !,
  ppn('Publication', Ppn),
  {
    flag(publications, N, N + 1),
    (   N rem 1 =:= 0
    ->  debug(stcn_parse, 'PPNs extracted: ~D', [N])
    ;   true
    ),
    rdf_assert_instance(Ppn, stcno:'Publication', Graph)
  },
  '...',
  end_of_line, !,
  redactiebladen(Graph, Ppn).
redactiebladen(Graph, Ppn) -->
  "Ingevoerd", !,
  '...',
  end_of_line, !,
  redactiebladen(Graph, Ppn).
redactiebladen(Graph, Ppn) -->
  kmc_start(Kmc0, _), !,
  '...'(Content0),
  end_of_line, !,
  {
    atom_number(Kmc, Kmc0),
    rdf_global_id(stcno:Kmc, P),
    atom_codes(Content, Content0),
    rdf_assert_string(Ppn, P, Content, Graph)
  },
  redactiebladen(Graph, Ppn).
redactiebladen(Graph, Ppn) -->
  '...'(Line),
  end_of_line, !,
  {
    atom_codes(Atom, Line),
    debug(stcn_parse, 'Could not process line: ~a of PPN ~w', [Atom,Ppn])
  },
  redactiebladen(Graph, Ppn).
redactiebladen(_, _) -->
  dcg_end, !.
