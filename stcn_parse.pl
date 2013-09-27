:- module(
  stcn_parse,
  [
    collect_lines/0,
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
:- use_module(dcg(dcg_multi)).
:- use_module(generics(codes_ext)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(rdf(rdf_build)).
:- use_module(standards(abnf)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_kmc)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

number_of_redactiebladen_batches(4).
redactiebladen_graph('STCN_Redactiebladen').

:- assert(user:prolog_file_type(txt, text)).

:- set_prolog_stack(global, limit(2*10**9)).
:- set_prolog_stack(local,  limit(2*10**9)).

:- debug(stcn_parse).



collect_lines:-
  absolute_file_name(data(redactiebladen1), InFile, [access(read),file_type(text)]),
  absolute_file_name(data(redactiebladen2), OutFile, [access(write),file_type(text)]),
  setup_call_cleanup(
    open(OutFile, write, Out, [encoding(utf8),type(text)]),
    phrase_from_file(collect_lines(Out), InFile),
    close(Out)
  ).

% Skip empty lines.
collect_lines(Out) -->
  'CRLF', !,
  collect_lines(Out).
% Another one.
collect_lines(Out) -->
  collect_line(L), !,
  {put_codes(Out, L), nl(Out), flush_output(Out)},
  collect_lines(Out).
% Done!
collect_lines(Out) -->
  {gtrace}, dcg_end, !,
  {nl(Out)}.

collect_line(L) -->
  collect_line(L, out).

% SET line.
collect_line(L, out) -->
  dcg_peek(atom('SET')), !,
  dcg_until([end_mode(exclusive),output_format(codes)], 'CRLF', L),
  'CRLF'.
% Ingevoerd line, always spans two rows.
collect_line(L, out) -->
  dcg_peek(atom('Ingevoerd')), !,
  dcg_until([end_mode(exclusive),output_format(codes)], 'CRLF', L1),
  'CRLF',
  dcg_peek(atom('11')),
  dcg_until([end_mode(exclusive),output_format(codes)], 'CRLF', L2),
  'CRLF',
  {append(L1, L2, L)}.
% Start a KMC line.
collect_line(L, out) -->
  peek_kmc_start, !,
  dcg_until([end_mode(exclusive),output_format(codes)], 'CRLF', H),
  'CRLF',
  collect_line(T, in),
  {append(H, T, L)}.
% The next KMC. We are done here.
collect_line([], in) -->
  peek_kmc_start, !.
% Everything else.
collect_line(L, Mode) -->
  dcg_until([end_mode(exclusive),output_format(codes)], 'CRLF', H1),
  'CRLF',
  collect_line(T, Mode),
  {(Mode == in -> H2 = [32|H1] ; H2 = H1)},
  {append(H2, T, L)}.

peek_kmc_start -->
  dcg_peek_length(5, Cs),
  {phrase(kmc_start(_KMC), Cs)}.

parse_redactiebladen(G):-
  absolute_file_name(data(redactiebladen2), File, [access(read),file_type(text)]),
  phrase_from_file(redactiebladen(G, _PPN), File).

redactiebladen(G, PPN) -->
  'CRLF', !,
  redactiebladen(G, PPN).
redactiebladen(G, _PPN) -->
 "SET", !,
  dcg_until([end_mode(inclusive)], atom('PPN: '), _),
  ppn(PPN),
  {rdf_assert_individual(PPN, stcnv:'Publication', G)},
  dcg_until([end_mode(inclusive)], 'CRLF', _),
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  "Ingevoerd", !,
  dcg_until([end_mode(inclusive)], 'CRLF', _),
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  kmc_start(KMC), !,
  kmc(KMC, G, PPN),
  redactiebladen(G, PPN).
redactiebladen(G, PPN) -->
  dcg_until([end_mode(exclusive),output_format(atom)], 'CRLF', Line),
  {debug(stcn_parse, '[~w] .... ~w', [PPN,Line])},
  redactiebladen(G, PPN).

