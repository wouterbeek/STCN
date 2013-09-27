:- module(
  collect_lines,
  [
    collect_lines/0
  ]
).

/** <module> Collect lines

The first step in the STCN conversion.
Some statements span multiple lines.
We first want to create a text file with one line per statement.

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
:- use_module(os(file_ext)).
:- use_module(stcn(stcn_kmc)).

:- debug(collect_lines).



collect_lines:-
  absolute_file_name2(
    data(redactiebladen2),
    _,
    [access(read),file_type(text)]
  ), !.
collect_lines:-
  absolute_file_name(
    data(redactiebladen1),
    InFile,
    [access(read),file_type(text)]
  ),
  absolute_file_name(
    data(redactiebladen2),
    OutFile,
    [access(write),file_type(text)]
  ),
  setup_call_cleanup(
    open(OutFile, write, Out, [encoding(utf8),type(text)]),
    phrase_from_file(collect_lines(Out), InFile),
    close(Out)
  ).

% Skip empty lines.
collect_lines(Out) -->
  end_of_line, !,
  collect_lines(Out).
% Another one.
collect_lines(Out) -->
  collect_line(L), !,
  {put_codes(Out, L), nl(Out), flush_output(Out)},
  collect_lines(Out).
% Done!
collect_lines(Out) -->
  [], !, {gtrace},
  {nl(Out)}.

collect_line(L) -->
  collect_line(L, out).

% SET line.
collect_line(L, out) -->
  dcg_peek(atom('SET')), !,
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, L),
  end_of_line.
% Ingevoerd line, always spans two rows.
collect_line(L, out) -->
  dcg_peek(atom('Ingevoerd')), !,
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, L1),
  end_of_line,
  dcg_peek(dcg_multi(decimal_digit, 2)),
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, L2),
  end_of_line,
  {append(L1, L2, L)}.
% Start a KMC line.
collect_line(L, out) -->
  peek_kmc_start, !,
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, H),
  end_of_line,
  collect_line(T, in),
  {append(H, T, L)}.
% Another KMC. The previous KMC is done.
collect_line([], in) -->
  peek_kmc_start, !.
% Another SET. The previous KMC is done.
collect_line([], in) -->
  dcg_peek(atom('SET')), !.
% Another Ingevoerd. The previous KMC is done. This may never occur.
collect_line([], in) -->
  dcg_peek(atom('Ingevoerd')), !.
% KMC continuations.
collect_line(L, in) -->
  dcg_until([end_mode(exclusive),output_format(codes)], end_of_line, H1),
  end_of_line,
  collect_line(T, in),
  {H2 = [32|H1]},
  {append(H2, T, L)}.

peek_kmc_start -->
  dcg_peek_length(5, Cs),
  {phrase(kmc_start(_KMC), Cs)}.

