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
:- use_module(library(filesex)).
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

:- initialization(thread_create(stcn_script, _, [])).



stcn_script:-
  set_prolog_stack(global, limit(2*10**9)),
  set_prolog_stack(local, limit(2*10**9)),
  Process = 'STCN',
  script(
    [to_file('VoID.ttl')],
    Process,
    [
      stage([to_file('STCNV.ttl'),to_output(true)], stcn_schema),
      stage(
        [
          from_file('redactiebladen.txt.tar.gz'),
          to_file('redactiebladen.txt')
        ],
        redactiebladen_copy
      ),
      stage(
        [from_file('redactiebladen.txt'),to_file('redactiebladen.txt')],
        redactiebladen_prepare
      ),
      stage(
        [from_file('redactiebladen.txt'),to_file('Redactiebladen.ttl')],
        redactiebladen_parse
      ),
      stage(
        [from_file('Redactiebladen.ttl'),to_file('PicartaPublications.ttl')],
        picarta_scrape_publications
      ),
      stage(
        [
          from_file('PicartaPublications.ttl'),
          to_file('PicartaPublications.ttl')
        ],
        picarta_scrape_publications_split
      ),
      stage(
        [
          from_file('PicartaPublications.ttl'),
          to_file('PicartaPublications.ttl')
        ],
        picarta_scrape_publications_ppns
      ),
      stage([from_file('PicartaPublications.ttl')], picarta_scrape_others),
      stage([to_output(true)], assert_stcn_void)
    ]
  ).

assert_stcn_void(_PS, _FromDir, ToDir):-
  absolute_file_name2(
    'VoID',
    ToFile,
    [access(write),file_type(turtle),relative_to(ToDir)]
  ),
  stcn_void('VoID'),
  void_save_library('VoID', ToFile).

% Picarta scraping 1: publications.
picarta_scrape_publications(_PS, _FromFile, ToFile):-
  rdf_load2(ToFile, [format(turtle)]),
  debug(stcn_script, 'The Picarta scrape was loaded from file ~w.', [ToFile]), !.
picarta_scrape_publications(_PS, FromFile, ToFile):-
  file_to_graph_name(FromFile, FromG),
  rdf_load2(FromFile, [format(turtle),graph(FromG)]),

  file_to_graph_name(ToFile, ToG),
  stcn_scrape(FromG, 'Publication', ToG),
  debug(stcn_script, 'Done scraping the redactiebladen for publications.', []),

  rdf_save2(ToFile, [format(turtle),graph(ToG)]),
  debug(stcn_script, 'Done scraping Picarta for publications.', []),
  rdf_unload_graph(ToG).

picarta_scrape_publications_split(_PS, _FromFile, ToFile):-
  rdf_load2(ToFile, [format(turtle)]),
  debug(stcn_script, 'The Picarta scrape with splits was loaded from file ~w.', [ToFile]), !.
picarta_scrape_publications_split(_PS, FromFile, ToFile):-
  file_to_graph_name(FromFile, G),
  rdf_load2(FromFile, [format(turtle),graph(G)]),
  
  % Assert occurrences in literal enumerations as separate triples.
  rdf_split_literal([answer('A')], _, picarta:printer_publisher, G, '; '),
  debug(stcn_script, 'Printer-publishers were split.', []),

  rdf_strip_literal([answer('A')], [' '], _, picarta:printer_publisher, G),
  debug(stcn_script, 'Printer-publishers were stripped.', []),

  rdf_split_literal([answer('A')], _, picarta:topical_keyword, G, '; '),
  debug(stcn_script, 'Topics were split.', []),

  rdf_split_literal([answer('A')], _, picarta:typographic_information, G, ' , '),
  debug(stcn_script, 'Typographic information was split.', []),
  
  rdf_save2(ToFile, [format(turtle),graph(G)]),
  debug(stcn_script, 'The Picarta scrape with splits were saved to file ~w.', [ToFile]),
  rdf_unload_graph(G).

picarta_scrape_publications_ppns(_PS, _FromFile, ToFile):-
  rdf_load2(ToFile, [format(turtle)]),
  debug(stcn_script, 'The Picarta scrape with PPNs was loaded from file ~w.', [ToFile]), !.
picarta_scrape_publications_ppns(_PS, FromFile, ToFile):-
  file_to_graph_name(FromFile, G),
  rdf_load2(FromFile, [format(turtle),graph(G)]),
  
  % Turn PPN literals into IRIs.
  forall(
    member(
      P1-Category,
      [
        author-'Author',
        printer_publisher-'PrinterPublisher',
        translator_editor-'TranslatorEditor'
      ]
    ),
    (
      rdf_global_id(picarta:P1, P2),
      forall(
        rdf_literal(PublicationPPN, P2, Lit, G),
        (
          ppn_resource(Category, Lit, PPN),
          rdf_assert(PublicationPPN, P2, PPN, G),
          rdf_retractall_literal(PublicationPPN, P2, Lit, G)
        )
      )
    )
  ),

  % Save the processed scrape results.
  rdf_save2(ToFile, [format(turtle),graph(G)]),
  debug(stcn_script, 'Done saving the scraped redactiebladen to file ~w.', [ToFile]),
  rdf_unload_graph(G).

% Picarta scraping 2: authors, printer-publishers, topics,
% translator-editors.
picarta_scrape_others(_PS, _FromFile, ToDir):-
  absolute_file_name(
    'PicartaAuthors',
    _ToFile,
    [access(read),file_type(turtle),relative_to(ToDir)]
  ), !.
picarta_scrape_others(_PS, FromFile, ToDir):-
  file_to_graph_name(FromFile, FromG),
  rdf_load2(FromFile, [format(turtle),graph(FromG)]),
  
  forall_thread(
    (
      member(C, ['Author','PrinterPublisher','Topic','TranslatorEditor']),
      format(atom(Msg), 'Scraping Picarta for class ~w', [C])
    ),
    (
      atomic_list_concat(['Picarta',C,'s'], ToG),
      stcn_scrape(FromG, C, ToG),
      debug(stcn_script, 'Done scraping class ~w from Picarta.', [C]),
      absolute_file_name(
        ToG,
        ToFile,
        [access(write),file_type(turtle),relative_to(ToDir)]
      ),
      rdf_save2(ToFile, [format(turtle),graph(ToG)]),
      debug(stcn_script, 'Done saving the scraped class ~w from Picarta.', [C])
    ),
    stcn_script,
    Msg
  ).

redactiebladen_copy(_PS, FromFile, ToFile):-
  directory_file_path(ToDir, _, ToFile),
  archive_extract(FromFile, ToDir, []).

% Parses the redactiebladen.
redactiebladen_parse(_PS, _FromDir, ToDir):-
  absolute_file_name2(
    'Redactiebladen',
    ToFile,
    [access(read),file_type(turtle),relative_to(ToDir)]
  ),
  rdf_load2(ToFile, [format(turtle),graph('Redactiebladen')]),
  debug(stcn_script, 'The redactiebladen parse were loaded from file.', []), !.
redactiebladen_parse(_PS, FromDir, ToDir):-
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

% Prepares the redactiebladen file.
redactiebladen_prepare(_PS, FromFile, ToFile):-
  collect_lines(FromFile, F1),
  % Perform some in-file replacements.
  trim_spaces(F1, F2),
  replace_in_file(F2, "Â°", "°", F3),
  replace_in_file(F3, "Ãª", "°", ToFile),
  debug(
    stcn_script,
    'Done with preparing the redactiebladen file for scraping.',
    []
  ).

% Asserts the schema for STCN.
stcn_schema(_PS, _FromDir, ToFile):-
  stcn_schema('STCNV'),
  rdf_save2(ToFile, [format(turtle),graph('STCNV')]).
