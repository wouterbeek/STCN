:- module(
  stcn_scrape,
  [
    stcn_scrape/2 % +Category:oneof(['Author','Printer','Publication','Topic'])
                  % +Graph:atom
  ]
).

/** <module> STCN scrape

Fully automated scrape for the STCN.

@author Wouter Beek
@tbd Automate the generation of the list of initial publication PPNs
     based on the redactiebladen (using module STCN_PARSE).
@version 2013/06, 2013/09
*/

:- use_module(dcg(dcg_generic)).
:- use_module(generics(atom_ext)).
:- use_module(generics(list_script)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(file_ext)).
:- use_module(picarta(picarta_query)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_clean)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_lit)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_build)).
:- use_module(rdfs(rdfs_read)).
:- use_module(stcn(stcn_generic)).
:- use_module(stcn(stcn_schema)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf,  'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- xml_register_namespace(stcnv, 'http://stcn.data2semantics.org/resource/vocab/').



%! stcn_scrape(
%!   +Category:oneof(['Author','Printer','Publication','Topic']),
%!   +Graph:atom
%! ) is det.
% First we check whether the STCN content that we are after is already
% available locally (either in memory or stored in a file).
% Otherwise, we start scraping.
%
% The following categories are used:
%   * `author`
%   * `printer_publisher`
%   * `publications`
%   * `translator_editor`

% Scrape from remote Website (Picarta),
% Without a prior TODO file.
stcn_scrape(Category1, G):-
  rdf_global_id(stcnv:Category1, Category2),
  % We assume that the PPNs of the given category are currently loaded.
  setoff(
    PPN3,
    (
      rdfs_individual(m(t,f,f), PPN1, Category2, G),
      rdf_global_id(stcn:PPN2, PPN1),
      split_atom_exclusive('/', PPN2, [Category1,PPN3])
    ),
    PPNs
  ),

  % Write the PPNs of the given category to a TODO file.
  atomic_list_concat(['TODO',Category1], '_', TODO_FileName),
  create_file(data('.'), TODO_FileName, text, TODO_File),
  setup_call_cleanup(
    open(TODO_File, write, Stream, []),
    forall(
      member(PPN, PPNs),
      (
        format(Stream, '~w', [PPN]),
        nl(Stream),
        flush_output(Stream)
      )
    ),
    close(Stream)
  ),
  
  % Start the actual scraping, using the TODO file.
  % DONE file.
  atomic_list_concat(['DONE',Category1], '_', DONE_FileName),
  create_file(data('.'), DONE_FileName, text, DONE_File),

  % Process the TODO file.
  list_script(picarta_scrape(Category1, G), TODO_File, DONE_File).

/*
%! stcn_scrape is det.
% Scrapes all STCN content from the Picarta Website.
%
% The final content that this generates is stored in
% =STCN_Agents= and =STCN_Publications=.

stcn_scrape:-
  AuthorsGraph = 'Picarta_Authors',
  PrinterPublishersGraph = 'Picarta_PrinterPublishers',
  PublicationsGraph = 'Picarta_Publications',
  TranslatorEditorsGraph = 'Picarta_TranslatorEditors',

  % 2. Scrape the publications from Picarta.
  stcn_scrape(publications, PublicationsGraph),

  % 3. Assert occurrences in literal enumerations as separate triples.
  rdf_split_literal(_, stcnv:printer_publisher, PublicationsGraph, '; '),
  rdf_strip_literal([' '], _, stcnv:printer_publisher, PublicationsGraph),
  rdf_split_literal(_, stcnv:topical_keyword, PublicationsGraph, '; '),
  rdf_split_literal(
    _,
    stcnv:typographic_information,
    PublicationsGraph,
    ' , '
  ),

  % 4. Scrape the authors from Picarta.
  stcn_scrape(author, AuthorsGraph),

  % 5. Scrape the printers, publishers, translators, and editors from
  %    Picarta.
  stcn_scrape(printer_publisher, PrinterPublishersGraph),
  stcn_scrape(translator_editor, TranslatorEditorsGraph),

  % 6. Assert all translator/editors as FOAF agents.
  % @tbd Check out why some translators/editors are not already asserted
  %      as FOAF agents and fix the issue there.
  forall(
    rdf(Agent, _Predicate, _Object, TranslatorEditorsGraph),
    rdf_assert_individual(Agent, foaf:'Agent', TranslatorEditorsGraph)
  ),
  % 7. Authors and translators/editors are all agents in the STCN world.

  % 8. Keep the Picarta and STCN graphs separate.
  rdf_graph:rdf_graph_merge(
    [AuthorsGraph, PrinterPublishersGraph, TranslatorEditorsGraph],
    'STCN_Agents'
  ),
  rdf_graph:rdf_graph_merge([PublicationsGraph], 'STCN_Publications'),

  % Save results.
  maplist(
    rdf_save2,
    [
      AuthorsGraph,
      PrinterPublishersGraph,
      PublicationsGraph,
      'STCN_Agents',
      'STCN_Publications',
      TranslatorEditorsGraph
    ]
  ).
*/

picarta_scrape(Category, G, PPN_Name):-
  picarta_query_ppn(PPN_Name, ScrapeSite, NVPairs),
  ppn_resource(G, Category, PPN_Name, PPN),
  rdfs_assert_seeAlso(PPN, ScrapeSite, G),
  forall(
    member(N/V, NVPairs),
    (
      rdf_global_id(stcnv:N, Pred),
      rdf_assert_literal(PPN, Pred, V, G)
    )
  ).
