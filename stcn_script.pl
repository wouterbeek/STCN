:- module(stcn_script, []).

/** <module> STCN script

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@version 2013/09-2013/10
*/

:- use_module(generics(replace_in_file)).
:- use_module(generics(script_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(archive)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(datetime_ext)).
:- use_module(os(file_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_clean)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_serial)).
:- use_module(stcn(collect_lines)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_parse)).
:- use_module(stcn(stcn_schema)).
:- use_module(stcn(stcn_scrape)).
:- use_module(stcn(stcn_void)).
:- use_module(vocab(void_file)).

:- debug(stcn_script).

:- initialization(script).



script:-
  set_prolog_stack(global, limit(2*10**9)),
  set_prolog_stack(local, limit(2*10**9)),
  script_begin,
  stcn_schema,
  script_stage(0, redactiebladen_copy),
  script_stage(1, redactiebladen_prepare),
  script_stage(2, redactiebladen_parse),
  script_stage(3, picarta_scrape1),
  script_stage(4, picarta_scrape2),
  stcn_void.

%! picarta_scrape(+FromDirectory:atom, +ToDirectory:atom) is det.
% Picarta scraping 1: publications.

picarta_scrape1(_FromDir, ToDir):-
  absolute_file_name2(
    'PicartaPublications',
    ToFile,
    [access(read),file_type(turtle),relative_to(ToDir)]
  ),
  rdf_load2(ToFile, [format(turtle),graph('PicartaPublications')]),
  debug(stcn_script, 'The Picarta scrape was loaded from file.', []), !.
picarta_scrape1(FromDir, ToDir):-
  absolute_file_name(
    'Redactiebladen',
    FromFile,
    [access(read),file_type(turtle),relative_to(FromDir)]
  ),
  rdf_load2(FromFile, [format(turtle),graph('Redactiebladen')]),
  
  G = 'PicartaPublications',
  stcn_scrape('Redactiebladen', 'Publication', G),
  debug(stcn_script, 'Done scraping the redactiebladen.', []),
  
  % Intermediate save.
  absolute_file_name(data(interm), F0, [access(write),file_type(turtle)]),
  rdf_save2(F0, [format(turtle),graph(G)]),
  debug(stcn_script, 'Intermediate save of Picarta publications.', []),
  
  % Assert occurrences in literal enumerations as separate triples.
  rdf_split_literal([answer('A')], _, picarta:printer_publisher, G, '; '),
  debug(stcn_script, 'Printer-publishers were split.', []),
  rdf_strip_literal([answer('A')], [' '], _, picarta:printer_publisher, G),
  debug(stcn_script, 'Printer-publishers were stripped.', []),
  rdf_split_literal([answer('A')], _, picarta:topical_keyword, G, '; '),
  debug(stcn_script, 'Topics were split.', []),
  rdf_split_literal([answer('A')], _, picarta:typographic_information, G, ' , '),
  debug(stcn_script, 'Typographic information was split.', []),
  
  % Turn PPN literals into IRIs.
  forall(
    member(
      P1-C1,
      [
        author-'Author',
        printer_publisher-'PrintersPublisher',
        translator_editor-'TranslatorEditor'
      ]
    ),
    (
      rdf_global_id(picarta:P1, P2),
      forall(
        rdf_literal(PublicationPPN, P2, Lit, G),
        (
          ppn_resource(G, C1, Lit, PPN),
          rdf_assert(PublicationPPN, P2, PPN, G),
          rdf_retractall_literal(PublicationPPN, P2, Lit, G)
        )
      )
    )
  ),
  
  % Save the processed scrape results.
  absolute_file_name(
    G,
    ToFile,
    [access(write),file_type(turtle),relative_to(ToDir)]
  ),
  rdf_save2(ToFile, [format(turtle),graph(G)]),
  debug(stcn_script, 'Done saving the scraped redactiebladen.', []).

%! picarta_scrape2(+FromDirectory:atom, +ToDirectory:atom) is det.
% Picarta scraping 1: authors, printer-publishers, topics,
% translator-editors.

picarta_scrape2(FromDir, ToDir):-
  absolute_file_name(
    'PicartaPublications',
    FromFile,
    [access(read),file_type(turtle),relative_to(FromDir)]
  ),
  rdf_load2(FromFile, [format(turtle),graph('PicartaPublications')]),
  
  forall_thread(
    (
      member(C, ['Authors','PrintersPublishers','Topic','TranslatorEditor']),
      format(atom(Msg), 'Scraping Picarta for ~w', [C])
    ),
    (
      atomic_concat('Picarta', C, ToG),
      stcn_scrape('PicartaPublications', C, ToG),
      debug(stcn_script, 'Done scraping ~w from Picarta.', [C]),
      absolute_file_name(
        ToG,
        ToFile,
        [access(write),file_type(turtle),relative_to(ToDir)]
      ),
      rdf_save2(ToFile, [format(turtle),graph(ToG)]),
      debug(stcn_script, 'Done saving the scraped ~w from Picarta.', [C])
    ),
    stcn_script,
    Msg
  ).

%! redactiebladen_copy(+FromDirectory:atom, +ToDirectory:atom) is det.

redactiebladen_copy(FromDir, ToDir):-
  absolute_file_name(
    'redactiebladen.txt.tar.gz',
    FromFile,
    [access(read),relative_to(FromDir)]
  ),
  archive_extract(FromFile, ToDir, []).

%! redactiebladen_parse(+FromDirectory:atom, +ToDirectory:atom) is det.
% Parses the redactiebladen.

redactiebladen_parse(_FromDir, ToDir):-
  absolute_file_name2(
    'Redactiebladen',
    ToFile,
    [access(read),file_type(turtle),relative_to(ToDir)]
  ),
  rdf_load2(ToFile, [format(turtle),graph('Redactiebladen')]),
  debug(stcn_script, 'The redactiebladen parse were loaded from file.', []), !.
redactiebladen_parse(FromDir, ToDir):-
  absolute_file_name(
    redactiebladen,
    FromFile,
    [access(read),file_type(text),relative_to(FromDir)]
  ),
  parse_redactiebladen(FromFile, 'Redactiebladen'),
  debug(stcn_script, 'Done parsing the redactiebladen.', []),
  absolute_file_name(
    'Redactiebladen',
    ToFile,
    [access(write),file_type(turtle),relative_to(ToDir)]
  ),
  rdf_save2(ToFile, [format(turtle),graph('Redactiebladen')]),
  debug(stcn_script, 'Done saving the parsed redactiebladen.', []).

%! redactiebladen_prepare(+FromDirectory:atom, +ToDirectory:atom) is det.
% Prepares the redactiebladen file.

redactiebladen_prepare(_FromDir, ToDir):-
  absolute_file_name2(
    redactiebladen,
    ToFile,
    [access(read),file_type(text),relative_to(ToDir)]
  ),
  access_file(ToFile, read), !.
redactiebladen_prepare(FromDir, ToDir):-
  absolute_file_name2(
    redactiebladen,
    FromFile,
    [access(read),file_type(text),relative_to(FromDir)]
  ),
debug(stcn_script, 'Begin collecting lines.', []),
  collect_lines(FromFile, F1),
debug(stcn_script, 'End collecting lines.', []),
  % Perform some in-file replacements.
  trim_spaces(F1, F2),
  replace_in_file(F2, "Â°", "°", F3),
  absolute_file_name(
    redactiebladen,
    ToFile,
    [access(write),file_type(text),relative_to(ToDir)]
  ),
  replace_in_file(F3, "Ãª", "°", ToFile),
  debug(
    stcn_script,
    'Done with preparing the redactiebladen file for scraping.',
    []
  ).

%! stcn_schema is det.
% Asserts the schema for STCN.

stcn_schema:-
  stcn_schema('STCNV'),
  absolute_file_name(output('STCNV'), F, [access(write),file_type(turtle)]),
  rdf_save2(F, [format(turtle),graph('STCNV')]).

stcn_void:-
  absolute_file_name(output('VoID'), File, [access(write),file_type(turtle)]),
  stcn_void('VoID'),
  void_save_library('VoID', File).
