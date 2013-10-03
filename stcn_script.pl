:- module(
  stcn_script,
  [
    stcn_script/0
  ]
).

/** <module> STCN script

This module only contains predicates that have a domain-specific use.
These predicate should be converted to some other module or be removed.

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/frames.html
@version 2013/09-2013/10
*/

:- use_module(generics(replace_in_file)).
:- use_module(generics(thread_ext)).
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



stcn_script:-
  % Start time.
  date_time(Start),
  debug(stcn, 'Loading started at: ~w.\n', [Start]),

  % Asserts the schema for STCN.
  stcn_schema('STCNV'),
  absolute_file_name(data('STCNV'), F0, [access(write),file_type(turtle)]),
  rdf_save2(F0, [format(turtle),graph('STCNV')]),

  % Prepares the redactiebladen file.
  (
    absolute_file_name2(
      data(redactiebladen_5),
      F5,
      [access(read),file_type(text)]
    ),
    access_file(F5, read), !
  ;
    absolute_file_name2(
      data(redactiebladen_1),
      F1,
      [access(read),file_type(text)]
    ),
    collect_lines(F1, F2),

    % Performs some in-file replacements.
    trim_spaces(F2, F3),
    replace_in_file(F3, "Â°", "°", F4),
    replace_in_file(F4, "Ãª", "°", F5),
    debug(
      stcn_script,
      'Done with preparing the redactiebladen file for scraping.',
      []
    )
  ),

  % Parse the redactiebladen.
  (
    absolute_file_name2(
      data('Redactiebladen'),
      F6,
      [access(read),file_type(turtle)]
    ),
    rdf_load2(F6, [format(turtle),graph('Redactiebladen')]),
    debug(stcn_script, 'The redactiebladen parse were loaded from file.', []), !
  ;
    parse_redactiebladen(F5, 'Redactiebladen'),
    debug(stcn_script, 'Done parsing the redactiebladen.', []),
    absolute_file_name(
      data('Redactiebladen'),
      F6,
      [access(write),file_type(turtle)]
    ),
    rdf_save2(F6, [format(turtle),graph('Redactiebladen')]),
    debug(stcn_script, 'Done saving the parsed redactiebladen.', [])
  ),
  
  % Picarta scraping 1: publications.
  (
    absolute_file_name2(
      data('PicartaPublications'),
      F7,
      [access(read),file_type(turtle)]
    ),
    rdf_load2(F7, [format(turtle),graph('PicartaPublications')]),
    debug(stcn_script, 'The Picarta scrape was loaded from file.', [])
  ;
    G = 'PicartaPublications',
    
    stcn_scrape('Redactiebladen', 'Publication', G),
    debug(stcn_script, 'Done scraping the redactiebladen.', []),
    
    % Intermediate save.
    absolute_file_name(data(interm), F70, [access(write),file_type(turtle)]),
    rdf_save2(F70, [format(turtle),graph(G)]),
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
            gtrace,
            ppn_resource(G, C1, Lit, PPN),
            rdf_assert(PublicationPPN, P2, PPN, G),
            rdf_retractall_literal(PublicationPPN, P2, Lit, G)
          )
        )
      )
    ),
    
    % Save the processed scrape results.
    absolute_file_name(data(G), F7, [access(write),file_type(turtle)]),
    rdf_save2(F7, [format(turtle),graph(G)]),
    debug(stcn_script, 'Done saving the scraped redactiebladen.', [])
  ),
  
  % Picarta scraping 1: authors, printer-publishers, topics,
  % translator-editors.
  forall_thread(
    (
      member(C, ['Authors','PrintersPublishers','Topic','TranslatorEditor']),
      format(atom(Msg), 'Scraping Picarta for ~w', [C])
    ),
    (
      atomic_concat('Picarta', C, ToG),
      stcn_scrape('PicartaPublications', C, ToG),
      debug(stcn_script, 'Done scraping ~w from Picarta.', [C]),
      absolute_file_name(data(ToG), F8, [access(write),file_type(turtle)]),
      rdf_save2(F8, [format(turtle),graph(ToG)]),
      debug(stcn_script, 'Done saving the scraped ~w from Picarta.', [C])
    ),
    stcn_script,
    Msg
  ),
  
  absolute_file_name(data('VoID'), F9, [access(write),file_type(turtle)]),
  stcn_void('VoID'),
  void_save_library('VoID', F9),

  % End time.
  date_time(End),
  debug(stcn, 'Loading ended at: ~w.\n', [End]).

