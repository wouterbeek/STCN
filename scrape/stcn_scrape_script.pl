:- module(
  stcn_scrape_script,
  [
    stcn_scrape_script/1 % +Graph:atom
  ]
).

/** <module> STCN Scrape Script

Script for scraping the STCN from Picarta.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(process/thread_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(graph/rdf_graph_name)).
:- use_module(plRdf(management/rdf_save_any)).

:- use_module(stcn(stcn_generics)).
:- use_module(stcn(scrape/stcn_scrape)).





stcn_scrape_script(PGraph):-
  % Picarta scraping for publications.
  rdf_new_graph(PGraph, SGraph),
  debug(stcn_script, 'Done scraping publications.', []),
  debug(stcn_script, 'Done scraping Picarta for publications.', []),

  % Turn PPN literals into IRIs.
  forall(
    member(
      P0-Category,
      [
        author-'Author',
        printer_publisher-'PrinterPublisher',
        translator_editor-'TranslatorEditor'
      ]
    ),
    (
      rdf_global_id(picarta:P0, P),
      forall(
        (
          rdf_string(PublicationPPN, P, LexicalForm, SGraph),
          % There are both PPN literals and full name literals.
          % Only the former can be turned into URIs.
          dcg_phrase(ppn, LexicalForm)
        ),
        (
          ppn_resource(Category, LexicalForm, PPN),
          rdf_assert(PublicationPPN, P, PPN, SGraph),
          rdf_retractall_string(PublicationPPN, P, _, SGraph)
        )
      )
    )
  ),

/*
  % Load the Picarta topics.
  absolute_file_name(
    input('PicartaTopics'),
    TopicFile,
    [access(read),file_type(turtle)]
  ),
  rdf_load_any(TopicFile, [format(turtle),graph('PicartaTopics')]),
  forall(
    rdf_string(PPN, picarta:topical_keyword, TopicLit, G),
    (
      once(rdf_string(TopicPPN, _, TopicLit, G)),
      rdf_assert(PPN, picarta:topical_keyword, TopicPPN, G),
      rdf_retractall_string(PPN, picarta:topical_keyword, G)
    )
  ),
*/

  forall_thread(
    (
      member(C, ['Author','PrinterPublisher','TranslatorEditor']),
      format(atom(Msg), 'Scraping Picarta for class ~w', [C])
    ),
    (
      atomic_list_concat(['Picarta',C,'s'], ToG),
      stcn_scrape(SGraph, C, ToG),
      debug(stcn_script, 'Done scraping class ~w from Picarta.', [C]),
      format(atom(Path), 'rdf/~a.ttl', [C]),
      rdf_save_any(
        file_spec(stcn(Path)),
        [format(turtle),graph(ToG)]
      ),
      debug(
        stcn_script,
        'Done saving the scraped class ~w from Picarta.',
        [C]
      )
    ),
    stcn_script,
    Msg
  ),

  rdf_save_any(
    file_spec(stcn('rdf/SGraph.ttl')),
    [format(turtle),graph(SGraph)]
  ).

