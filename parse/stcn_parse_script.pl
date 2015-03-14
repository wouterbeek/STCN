:- module(
  stcn_parse_script,
  [
    stcn_parse_script/2 % +Uri:atom
                        % -Graph:atom
  ]
).

/** <module> STCN Parse Script

Script for parsing the STCN from redactiebladen.

@author Wouter Beek
@tbd Unable to phrase_from_stream/2 on the URI directly; due to dcg_peek//1.
@version 2015/02-2015/03
*/

:- use_module(library(debug)).
:- use_module(library(pure_input)).

:- use_module(plc(dcg/replace_in_file)).
:- use_module(plc(io/archive_ext)).

:- use_module(plHttp(download_to_file)).

:- use_module(plRdf(management/rdf_save_any)).

:- use_module(stcn(parse/collect_lines)).
:- use_module(stcn(parse/stcn_parse)).





%! stcn_parse_script(+Uri:atom, +Graph:atom) is det.

stcn_parse_script(Uri, G):-
  absolute_file_name(stcn(data/'lines1.txt'), F1),
  (   exists_file(F1)
  ->  true
  ;   download_to_file(Uri, ArchiveFile, []),
      archive_extract(ArchiveFile, _, _, [LocalName-_]),
      file_directory_name(ArchiveFile, ArchiveDir),
      absolute_file_name(
        LocalName,
        F0,
        [access(read),relative_to(ArchiveDir)]
      ),
      flag(collected_lines, _, 0),
      setup_call_cleanup(
        open(F1, write, Out),
        phrase_from_file(collect_lines(Out), F0),
        close(Out)
      ),
      flag(collected_lines, N, 0),
      debug(stcn_parse_script, 'Collected ~D lines.', [N])
  ),
  absolute_file_name(stcn(data/'lines2.txt'), F2),
  (   exists_file(F2)
  ->  true
  ;   trim_spaces(F1, F2)
  ),

  % Parse input document.
  flag(publications, _, 0),
  G = 'Redactiebladen',
  phrase_from_file(redactiebladen(G, _), F2, [encoding(utf8),type(text)]),
  flag(publications, N, 0),
  debug(stcn_parse_script, 'Number of PPNs processed: ~D.', [N]),
gtrace,
  rdf_save_any(
    file_spec(stcn('rdf/parsed.ttl')),
    [format(turtle),graph(G)]
  ),
  debug(stcn_parse_script, 'Done saving the parsed redactiebladen.', []).

