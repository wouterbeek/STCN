:- module(
  collect_lines,
  [
    collect_lines//1 % +Input:stream
  ]
).

/** <module> Collect lines

The first step in parsing a database dump of the STCN.
Some statements span multiple lines.
We first want to create a text file with one line per statement.
The database dump is ambiguous.

---

@author Wouter Beek
@version 2013/09-2013/10, 2015/02
*/

:- use_module(library(pio)).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_peek)).
:- use_module(plc(generics/code_ext)).
:- use_module(plc(io/file_ext)).

:- use_module(stcn(parse/stcn_parse_generics)).





%! collect_line(-Line:list(code))// is det.

% SET line.
collect_line(Line) -->
  dcg_peek(atom('SET')), !,
  '...'(Line),
  end_of_line.
% Ingevoerd line, always spans two rows.
collect_line(Line) -->
  dcg_peek(atom('Ingevoerd')), !,
  '...'(Line1),
  end_of_line,
  (   dcg_peek(double_digit)
  ->  '...'(Line2),
      end_of_line,
      {append(Line1, Line2, Line)}
  ;   {Line = Line1}
  ).
% KMC line.
collect_line(Line) -->
  peek_kmc_start, !,
  '...'(Line1),
  end_of_line,
  collect_the_rest_of_a_kmc_line(Line2),
  {append(Line1, Line2, Line)}.

%! collect_the_rest_of_a_kmc_line(-Rest:list(code))// is det.

% We are now inside a KMC line...
% A blank line ends a KMC line.
collect_the_rest_of_a_kmc_line([]) -->
  end_of_line, !.
% Another KMC ends a KMC line.
collect_the_rest_of_a_kmc_line([]) -->
  peek_kmc_start, !.
% Another SET ends a KMC line.
collect_the_rest_of_a_kmc_line([]) -->
  dcg_peek(atom('SET')), !.
% Another Ingevoerd ends a KMC line. (This may never occur.)
collect_the_rest_of_a_kmc_line([]) -->
  dcg_peek(atom('Ingevoerd')), !.
% The document ends. This surely ends a KMC line.
collect_the_rest_of_a_kmc_line([]) -->
  dcg_end, !.
% The KMC line continues.
collect_the_rest_of_a_kmc_line(Line) -->
  '...'(Line0),
  end_of_line, !,
  collect_the_rest_of_a_kmc_line(Line2),
  % We sometimes have to add spaces and sometimes not.
  % We add a space in very circumstance and trim spaces later.
  {Line1 = [32|Line0]},
  {append(Line1, Line2, Line)}.



%! collect_lines(+Out:stream)// is det.

% Skip empty lines.
collect_lines(Out) -->
  end_of_line, !,
  collect_lines(Out).
/*
% Line  contains the erratic character sequences:
%  - [239,187,191]
%    - `ï`, Latin Small Letter I With Diaeresis
%    - `»`, Right-Pointing Double Angle Quotation Mark
%    - `¿`, Inverted Question Mark
collect_lines(Out) -->
  [239,187,191],
  end_of_line, !,
  {
    flag(collected_lines, N, N),
    debug(collect_lines, 'Irregular content [239,187,191] at line ~D.', [N])
  },
  collect_lines(Out).
*/
% Irregular content: code line [65279] (Zero Width No-Break Space)
% at line 2,946,252.
collect_lines(Out) -->
  [65279],
  end_of_line, !,
  {
    flag(collected_lines, N, N),
    debug(collect_lines, 'Irregular content [65279] at line ~D.', [N])
  },
  collect_lines(Out).
% A collected line.
collect_lines(Out) -->
  collect_line(L), !,
  {
    put_codes(Out, L),
    nl(Out),
    flush_output(Out)
  },
  collect_lines(Out).
% Done!
collect_lines(_) -->
  dcg_done.





% HELPERS %

%! double_digit// is semidet.

double_digit -->
  decimal_digit,
  decimal_digit.



%! peek_kmc_start// is semidet.

peek_kmc_start -->
  dcg_peek(5, Codes),
  {phrase(kmc_start(_), Codes)}.
