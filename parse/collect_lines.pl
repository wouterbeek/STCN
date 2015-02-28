:- module(
  collect_lines,
  [
    collect_lines/2 % +FromFile:atom
                    % +ToFile:atom
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

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_peek)).
:- use_module(plc(generics/code_ext)).
:- use_module(plc(io/file_ext)).

:- use_module(stcn(stcn_kmc)).





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
  (   dcg_peek('#'(2, decimal_digit, []))
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
collect_the_rest_of_a_kmc_line(L) -->
  dcg_until(end_of_line, H1, [end_mode(exclusive),output_format(codes)]),
  end_of_line,
  collect_the_rest_of_a_kmc_line(T),
  % We sometimes have to add spaces and sometimes not,
  % but we are not
  {H2 = [32|H1]},
  {append(H2, T, L)}.



%! collect_lines(+FromFile:atom, -ToFile:atom) is det.

collect_lines(F1, F2):-
  new_file_name(F1, F2),
  flag(collected_line, _, 0),
  setup_call_cleanup(
    open(F2, write, Out, [encoding(utf8),type(text)]),
    phrase_from_file(collect_lines(Out), F1, [encoding(utf8),type(text)]),
    close(Out)
  ).



%! collect_lines(+Out:stream)// is det.

% Skip empty lines.
collect_lines(Out) -->
  end_of_collected_line, !,
  collect_lines(Out).
% Line 2.946.252 contains the erratic character sequences:
%  - [239,187,191]
%    - `ï`, Latin Small Letter I With Diaeresis
%    - `»`, Right-Pointing Double Angle Quotation Mark
%    - `¿`, Inverted Question Mark
collect_lines(Out) -->
  [239,187,191],
  end_of_collected_line, !,
  {debug(collect_lines, '', [])},
  collect_lines(Out).
%  - [65279]
%    - Zero Width No-Break Space
collect_lines(Out) -->
  [65279],
  end_of_collected_line, !,
  {debug(collect_lines, '', [])},
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

peek_kmc_start -->
  dcg_peek(5, Cs),
  {phrase(kmc_start(_KMC), Cs)}.

end_of_collected_line -->
  end_of_line,
  {flag(collected_line, N, N + 1)}.
