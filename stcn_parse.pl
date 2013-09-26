:- module(
  stcn_parse,
  [
    parse_redactiebladen/0
  ]
).

/** <module> STCN parse

Parser for the STCN redactiebladen file.

KMC modules can be arbitrarily loaded.

A KMC module contains the following public predicates:
    * parse/2
    * stcn_statistics/1, called in module [stcn_statistics.pl].

@author Wouter Beek
@version 2013/01-2013/03, 2013/06, 2013/09
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_multi)).
:- use_module(dcg(dcg_os)).
:- use_module(generics(cowspeak)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(pio)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(radix)).
:- use_module(os(filepath_ext)).
:- use_module(os(io_ext)).
:- use_module(rdf(rdf_serial)).
:- use_module(project(stcn_generic)).
:- use_module(project(stcn_kmc)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(stcn,  'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').

number_of_redactiebladen_batches(4).
redactiebladen_graph('STCN_Redactiebladen').

:- assert(user:prolog_file_type(txt, text)).

:- debug(stcn_parse).



new_ppn(PPN) -->
  "SET",

  % Skip until the 'PPN' keyword is reached.
  dcg_until([end_mode(exclusive)], atom('PPN:'), _),

  "PPN: ",

  % Extract the PPN code and use it to find the resource identifier for
  % this redactiebladen entry.
  % Not all PPNs are numbers, e.g., '31785237X' for 'Atlas van Duitsland'.
  ppn(PPNAtom),

  {rdf_global_id(stcn:PPNAtom, PPN)}.

%! parse_redactiebladen is det.
% Parser of the redactiebladen.
% This allows the redactiebladen to be processed in N chunks.

/*
parse_redactiebladen:-
  number_of_redactiebladen_batches(N),
  numlist(1, N, Numbers),
  maplist(parse_redactiebladen, Numbers),
  maplist(
    absolute_file_name_number,
    data('STCN_Redactiebladen'),
    [access(read),file_type(turtle)],
    Numbers,
    Files
  ),
  absolute_file_name(
    data('STCN_Publications'),
    File,
    [access(read),file_type(turtle)]
  ),
  rdf_graph:rdf_graph_merge([File|Files], 'STCN_Publications'),
  rdf_save2('STCN_Publications').

parse_redactiebladen(N):-
  % Open a numbered redactiebladen file.
  atomic_concat('Redactiebladen', N, Name),
  absolute_file_name(data(Name), File, [access(read), extensions([txt])]),

  setup_call_cleanup(
    (
      open(File, read, Stream),
      set_stream(Stream, buffer(line))
    ),
    parse_redactiebladen(Stream, _PPN),
    close(Stream)
  ),
  cowspeak:cowspeak('Done parsing redactiebladen ~w.', [N]),

  % Store the result for =N= in a numbered file.
  absolute_file_name(data(Name), File, [access(write), file_type(turtle)]),
  redactiebladen_graph(Graph),
  rdf_save2(Name, [format(turtle), graph(Graph)]),
  rdf_unload_graph(Graph).
*/

parse_redactiebladen:-
  absolute_file_name(
    data(redactiebladen),
    File,
    [access(read),file_type(text)]
  ),
  phrase_from_file(redactiebladen, File).

% Skip line.
redactiebladen -->
  newline,
  redactiebladen.
% Skip line.
redactiebladen -->
  "Ingevoerd",
  dcg_until([end_mode(inclusive)], newline, _),
  redactiebladen.
% Skip line.
redactiebladen -->
 "SET",
  dcg_until([end_mode(inclusive)], newline, _),
  redactiebladen.
redactiebladen -->
  "11",
  dcg_until([end_mode(inclusive)], newline, _),
  redactiebladen.
redactiebladen -->
  dcg_multi(decimal_digit, 4, Codes, []),
  " ",
  {atom_codes(KMC, Codes)},
  dcg_until([end_mode(exclusive),output_format(atom)], newline, Value),
  {debug(stcn_parse, '~w ~w', [KMC,Value])},
  redactiebladen.
redactiebladen -->
  dcg_until([end_mode(exclusive),output_format(atom)], newline, Value),
  {debug(stcn_parse, '.... ~w', [Value])},
  redactiebladen.



/*
peek_at_line_head(empty) -->
  dcg_peek(atom('\n')).
peek_at_line_head(set) -->
  dcg_peek(atom('SET')).
peek_at_line_head(ingevoerd) -->
  dcg_peek(atom('Ingevoerd')).
peek_at_line_head(kmc) -->
  dcg_peek_length(5, Peek),
  phrase(kmc(_KMC, _Active, _Suffix), Peek).
peek_at_line_head(double_decimal) -->
  dcg_peek_length(3, Peek),
  phrase(dcg_multi(decimal_digit, 2)).
peek_at_line_head(_Stream, ufo) -->
  [].
*/

% Read a statement, then parse it.
parse_redactiebladen(Stream, PPN1):-
  read_statement_to_codes(Stream, Head, Codes),
  (
    Head == set
  ->
    % A line that starts with 'SET' introduces a new PPN.
    phrase(new_ppn(PPN2), Codes, _Rest1)
  ;
    phrase(parse_statement(Head, Stream, PPN1), Codes, _Rest2),
    PPN2 = PPN1
  ),
  parse_redactiebladen(Stream, PPN2).
parse_redactiebladen(_Stream, _PPN).

%! parse_statement(
%!   +Head:oneof([double_decimal,empty,ingevoerd,kmc,set]),
%!   +Stream:stream,
%!   +PPN:uri
%! )// is det.
% Parses a line in the STCN redactiebladen text file.
%
% @param Head Gives this rule a head start by telling it which clause to
%        use for parsing.
% @param Stream A stream, which is only used by the rule that parses the
%        SET-statement, allowing a new PPN to be shared with all subsequent
%        DCG rules (until the next SET-statement occurs).
% @param Publication A resource representing the current STCN redactiebladen
%        entry.

% Which line are we talking about?
% Empty line.
parse_statement(empty, _Stream, _PPN) --> [], !.
% A line that starts with 'Ingevoerd'.
parse_statement(ingevoerd, _Stream, PPN) -->
  atom('Ingevoerd'), !,
  {debug(stcn_parse, 'Parsed \'Ingevoerd\'-statement for ~w.', [PPN])}.
% A KMC-statement that is encoded in multiple lines and that is active.
parse_statement(kmc, _Stream, PPN) -->
  kmc(_KMC, true, Suffix),
  {(rdf_global_id(stcn:'204085489', PPN) -> gtrace ; true)},
  {(rdf_global_id(stcn:'324985231', PPN) -> gtrace ; true)},
  (
    {redactiebladen_graph(Graph)},
    parse_kmc(Suffix, Graph, PPN), !
  ;
    {cowspeak:cowspeak('PPN <~w> with KMC <~w>.', [PPN,Suffix])}
  ).
% A KMC-statement that is encoded in multiple lines and that is inactive.
parse_statement(kmc, _Stream, _PPN) -->
  kmc(_KMC, false, _Suffix).
% Since KMC-codes start with double digits as well, this rule should come
% after the KMC-rules.
parse_statement(double_decimal, _Stream, PPN) -->
  digit(_), digit(_), !,
  {debug(stcn_parse, 'Parsed a void statement for ~w.', [PPN])}.
parse_statement(Head, _Stream, PPN) -->
  {gtrace}, %DEB
  {debug(stcn_parse, '~w ~w', [Head, PPN])}.

%! peek_at_line_head(
%!   -Head:oneof([double_decimal,empty,ingevoerd,kmc,kmc_extended,set]),
%!   +All:list(integer),
%!   -Rest:list(integer)
%! ) is det.
% Returns the head type for the given list of codes.
% This is used to peek ahead and, in the case of KMC statements, to look
% ahead at an arbitrary number of further lines with a =kmc_extended= head.

peek_at_line_head(Stream, empty):-
  peek_char(Stream, '\n'), !.
peek_at_line_head(Stream, set):-
  peek_atom(Stream, 'SET'), !.
peek_at_line_head(Stream, ingevoerd):-
  peek_atom(Stream, 'Ingevoerd'), !.
peek_at_line_head(Stream, kmc):-
  peek_length(Stream, 5, Peek),
  phrase(kmc(_KMC, _Active, _Suffix), Peek), !.
% This seems to be used as a void statement?
peek_at_line_head(Stream, double_decimal):-
  peek_length(Stream, 3, Peek),
  phrase((digit(_), digit(_), end_of_line), Peek), !.
peek_at_line_head(Stream, end_of_file):-
  at_end_of_stream(Stream), !.
% UFO lines are lines that do not start with a defined prefix.
% This indicates that they belong together with a previous line.
peek_at_line_head(_Stream, ufo).

%! read_statement_to_codes(
%!   +Stream:stream,
%!   -CurrentHead:oneof([double_decimal,empty,ingevoerd,kmc,kmc_extended,set]),
%!   -StatementLines:list(integer)
%! )
% In the case of the STCN redactiebladen text file, we want to distinguish
% between statements and lines, since some statements span multiple lines.

read_statement_to_codes(Stream, CurrentHead, StatementLines):-
  peek_at_line_head(Stream, CurrentHead),
  \+ CurrentHead == end_of_file,
  read_line_to_codes(Stream, CurrentLine),
  peek_at_line_head(Stream, NextHead),
  (
    NextHead == ufo
  ->
    % Aha! The next line cannot be parsed, indicating an additional line for
    % the current KMC statement.
    read_statement_to_codes(Stream, NextHead, NextLines),
    append(CurrentLine, [32 | NextLines], StatementLines)
  ;
    % The next line can be parsed. Erge no additional content for the current
    % KMC statement.
    StatementLines = CurrentLine
  ),
  atom_codes(DebugAtom, StatementLines),
  debug(stcn_parse, 'Parsing: ~w', [DebugAtom]).

skip_redactiebladen(Stream, Skip):-
  peek_at_line_head(Stream, Head),
  (
    % Head and the number of skips have been processed. Done!
    Head == set,
    flag(number_of_ppns, Skip, Skip)
  ->
    true
  ;
    (
      % A SET-head, so process the PPN.
      Head == set
    ->
      % Update the PPN flag for SET lines
      % (which are assumed to always be statements).
      read_line_to_codes(Stream, PPN_Line),
      phrase(new_ppn(_Publication), PPN_Line, _Rest)
    ;
      % Skip all non-SET lines.
      read_line_to_codes(Stream, _PPN_Line)
    ),
    % Iterate.
    skip_redactiebladen(Stream, Skip)
  ).

