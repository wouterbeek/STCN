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
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(pure_input)).

:- use_module(plc(dcg/replace_in_file)).
:- use_module(plc(io/archive_ext)).

:- use_module(plHttp(download_to_file)).

:- use_module(plRdf(management/rdf_save_any)).

:- use_module(stcn(parse/collect_lines)).
:- use_module(stcn(parse/stcn_parse)).





stcn_parse_script(Uri, G):-
  % Unarchive the redactiebladen.
  download_to_file(Uri, ArchiveFile, []),
  archive_extract(ArchiveFile, _, _, [LocalName-_]),

  % Prepare the redactiebladen.
  absolute_file_name(data(LocalName), F0),
  collect_lines(F0, F1),
  % Perform some in-file replacements.
  trim_spaces(F1, F2),
  replace_in_file(F2, "Â°", "°", F3),
  replace_in_file(F3, "Ãª", "°", F4),
  debug(
    stcn_script,
    'Done with preparing the redactiebladen file for scraping.',
    []
  ),

  % Parse the redactiebladen.
  flag(publications, _, 0),
  G = 'Redactiebladen',
  phrase_from_file(redactiebladen(G, _), F4, [encoding(utf8),type(text)]),
  flag(publications, N, 0),
  debug(stcn_script, 'Done parsing the redactiebladen.', []),
  debug(stcn_script, 'Number of PPNs processed: ~D.', [N]),
  absolute_file_name(
    stcn(rdf/parsed),
    RdfFile,
    [access(write),file_type(turtle)]
  ),
  rdf_save_any(RdfFile, [format(turtle),graph(G)]),
  debug(stcn_script, 'Done saving the parsed redactiebladen.', []).

